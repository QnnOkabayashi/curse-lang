//! Inspired by: https://github.com/jfecher/algorithm-j/blob/master/j.ml
//!
//! Version that doesn't support let polymorphism at all
#![allow(dead_code)]
#![allow(clippy::all)]
use displaydoc::Display;
use std::{cell::Cell, collections::HashMap, fmt};
use thiserror::Error;
use typed_arena::Arena;

#[derive(Copy, Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[displaydoc("a{0}")]
struct TypevarId(u64);

#[derive(Debug)]
enum Expr {
    Unit,
    Int(i32),
    Bool(bool),
    Ident(String),
    Tuple(Vec<Expr>),
    Appl { fun: Box<Expr>, arg: Box<Expr> },
    Closure(Box<Branch>, Vec<Branch>),
}

#[derive(Debug)]
struct Branch {
    param: Pat,
    body: Expr,
}

#[derive(Debug)]
enum Pat {
    Int,
    Bool,
    Ident(String),
    Tuple(Vec<Pat>),
}

#[derive(Debug, Clone, PartialEq)]
enum Type<'hm> {
    Unit,
    Int,
    Bool,
    Tuple(Vec<&'hm Type<'hm>>),
    Var(&'hm Cell<Typevar<'hm>>),
    Fun {
        input: &'hm Type<'hm>,
        output: &'hm Type<'hm>,
    },
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Tuple(tup) => {
                write!(f, "(")?;
                if let Some((x, xs)) = tup.split_first() {
                    write!(f, "{x}")?;
                    for x in xs {
                        write!(f, ", {x}")?;
                    }
                }
                write!(f, ")")
            }
            Type::Var(typevar) => write!(f, "{}", typevar.get()),
            Type::Fun { input, output } => write!(f, "({input} -> {output})"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Display)]
enum Typevar<'hm> {
    #[displaydoc("{0}")]
    Bound(&'hm Type<'hm>),
    #[displaydoc("{0}")]
    Unbound(TypevarId),
}

#[derive(Clone)]
struct Polytype<'hm> {
    typevars: Vec<TypevarId>,
    typ: &'hm Type<'hm>,
}

struct Bindings<'scope, 'hm> {
    globals: &'hm HashMap<String, Polytype<'hm>>,
    locals: &'scope mut Vec<(String, &'hm Type<'hm>)>,
    original_len: usize,
}

impl<'scope, 'hm> Bindings<'scope, 'hm> {
    pub fn new(
        globals: &'hm HashMap<String, Polytype<'hm>>,
        locals: &'scope mut Vec<(String, &'hm Type<'hm>)>,
    ) -> Self {
        let original_len = locals.len();
        Bindings {
            globals,
            locals,
            original_len,
        }
    }

    /// Search through local variables first, then search through global variables.
    pub fn get(&self, var: &str, env: &'hm Env<'hm>) -> Option<&'hm Type<'hm>> {
        // When looking up an identifier, look through the locals
        // BEFORE looking through the globals.
        self.locals
            .iter()
            .rev()
            .find_map(|(ident, ty)| (*ident == var).then_some(*ty))
            .or_else(|| {
                self.globals
                    .get(var)
                    .map(|polytype| env.monomorphize(polytype))
            })
    }

    pub fn add_local(&mut self, var: String, ty: &'hm Type<'hm>) {
        self.locals.push((var, ty));
    }

    /// Enter a new scope.
    ///
    /// This method will uniquely borrow a `Bindings` to create another `Bindings`,
    /// which represents an inner scope. When the returned type is dropped,
    /// all bindings that were added in the inner scope will be removed,
    /// leaving the original scope in its initial state and accessible again
    /// since it's no longer borrowed.
    pub fn enter_scope(&mut self) -> Bindings<'_, 'hm> {
        Bindings::new(self.globals, self.locals)
    }
}

impl Drop for Bindings<'_, '_> {
    fn drop(&mut self) {
        self.locals.truncate(self.original_len);
    }
}

struct Env<'hm> {
    current_typevar: Cell<u64>,
    types: Arena<Type<'hm>>,
    typevars: Arena<Cell<Typevar<'hm>>>,
}

impl<'hm> Env<'hm> {
    pub fn new() -> Self {
        Env {
            current_typevar: Cell::new(0),
            types: Arena::new(),
            typevars: Arena::new(),
        }
    }

    pub fn new_typevar(&'hm self) -> (TypevarId, &'hm Type<'hm>) {
        let var = self.current_typevar.get();
        self.current_typevar.set(var + 1);
        let id = TypevarId(var);

        let cell = Cell::new(Typevar::Unbound(id));
        let typ = Type::Var(&*self.typevars.alloc(cell));
        (id, self.types.alloc(typ))
    }

    // (* specializes the polytype s by copying the term and replacing the
    //     * bound type variables consistently by new monotype variables
    //     * E.g.   inst (forall a b. a -> b -> a) = c -> d -> c     *)
    //    let inst (PolyType(typevars, typ)) : typ =
    //        (* Replace any typevars found in the Hashtbl with the
    //         * associated value in the same table, leave them otherwise *)
    //        let rec replace_tvs tbl = function
    //            | TUnit -> TUnit
    //            | TVar({ contents = Bound t }) -> replace_tvs tbl t
    //            | TVar({ contents = Unbound (n, level)}) as t ->
    //                begin match ITbl.find_opt tbl n with
    //                | Some t' -> t'
    //                | None -> t
    //                end
    //            | Fn(a, b) -> Fn(replace_tvs tbl a, replace_tvs tbl b)
    //        in
    //        (* Note that the returned type is no longer a PolyType,
    //         * this means it is now monomorphic, the 'forall' is gone. *)
    //        let tvs_to_replace = ITbl.create 1 in
    //        List.iter (fun tv -> ITbl.add tvs_to_replace tv (newvar_t ())) typevars;
    //        replace_tvs tvs_to_replace typ
    pub fn monomorphize(&'hm self, polytype: &Polytype<'hm>) -> &'hm Type<'hm> {
        // Takes a polymorphic type and replaces all instances of generics
        // with a fixed, unbound type.
        // For example, id: T -> T is a polymorphic type, so it goes through
        // and replaces both `T`s with an unbound type variable like `a0`,
        // which is then bound later on.
        fn replace_unbound_typevars<'hm>(
            tbl: &HashMap<TypevarId, &'hm Type<'hm>>,
            env: &'hm Env<'hm>,
            ty: &'hm Type<'hm>,
        ) -> &'hm Type<'hm> {
            match ty {
                Type::Var(typevar) => match typevar.get() {
                    Typevar::Bound(t) => replace_unbound_typevars(tbl, env, t),
                    Typevar::Unbound(typevar_id) => match tbl.get(&typevar_id) {
                        Some(t) => *t,
                        None => ty,
                    },
                },
                Type::Fun { input, output } => env.types.alloc(Type::Fun {
                    input: replace_unbound_typevars(tbl, env, input),
                    output: replace_unbound_typevars(tbl, env, output),
                }),
                _ => ty,
            }
        }

        let tvs_to_replace = polytype
            .typevars
            .iter()
            .map(|tv| (*tv, self.new_typevar().1))
            .collect();

        replace_unbound_typevars(&tvs_to_replace, self, polytype.typ)
    }

    // (* Can a monomorphic TVar(a) be found inside this type? *)
    // let rec occurs a_id a_level (* in *) = function
    //     | TUnit -> false
    //     | TVar({ contents = Bound t }) -> occurs a_id a_level t
    //     | TVar({ contents = Unbound(b_id, b_level)} as b_typevar) ->
    //         let min_level = min a_level b_level in
    //         b_typevar := Unbound (b_id, min_level);
    //         a_id = b_id
    //     | Fn(b, c) -> occurs a_id a_level b || occurs a_id a_level c
    /// Determines whether or not cyclic types occur
    pub fn occurs(&self, a: TypevarId, typ: &'hm Type<'hm>) -> bool {
        match typ {
            Type::Var(typevar) => match typevar.get() {
                Typevar::Bound(t) => self.occurs(a, t),
                Typevar::Unbound(b) => a == b,
            },
            Type::Fun { input, output } => self.occurs(a, input) || self.occurs(a, output),
            _ => false,
        }
    }

    // let rec unify (t1: typ) (t2: typ) : unit =
    // match (t1, t2) with
    // | (TUnit, TUnit) -> ()
    // (* These two recursive calls to the bound typevar replace
    // * the 'find' in the union-find algorithm *)
    // | (TVar({ contents = Bound a' }), b) -> unify a' b
    // | (a, TVar({ contents = Bound b' })) -> unify a b'
    // | (TVar({ contents = Unbound(a_id, a_level) } as a), b) ->
    //     (* create binding for boundTy that is currently empty *)
    //     if t1 = t2 then () else (* a = a, but dont create a recursive binding to itself *)
    //     if occurs a_id a_level b then raise TypeError else
    //     a := Bound b
    // | (a, TVar({ contents = Unbound(b_id, b_level)} as b)) ->
    //     (* create binding for boundTy that is currently empty *)
    //     if t1 = t2 then () else
    //     if occurs b_id b_level a then raise TypeError else
    //     b := Bound a
    // | (Fn(a, b), Fn(c, d)) ->
    //     unify a c;
    //     unify b d
    // | (a, b) -> raise TypeError
    /// Adjusts two types to be the same, or returns an error if it's impossible.
    pub fn unify(&self, t1: &'hm Type<'hm>, t2: &'hm Type<'hm>) -> Result<(), TypeError<'hm>> {
        match (t1, t2) {
            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Tuple(tuple_a), Type::Tuple(tuple_b)) => {
                if tuple_a.len() == tuple_b.len() {
                    tuple_a
                        .iter()
                        .zip(tuple_b)
                        .try_for_each(|(a, b)| self.unify(a, b))
                } else {
                    // different length tuples
                    Err(TypeError::Unify(t1, t2))
                }
            }
            (Type::Var(typevar), a) | (a, Type::Var(typevar)) => match typevar.get() {
                Typevar::Bound(b) => self.unify(a, b),
                Typevar::Unbound(a_id) => {
                    if t1 == t2 {
                        Ok(())
                    } else if self.occurs(a_id, a) {
                        Err(TypeError::CyclicType(a_id, a))
                    } else {
                        typevar.set(Typevar::Bound(a));
                        Ok(())
                    }
                }
            },
            (
                Type::Fun {
                    input: input1,
                    output: output1,
                },
                Type::Fun {
                    input: input2,
                    output: output2,
                },
            ) => {
                self.unify(input1, input2)?;
                self.unify(output1, output2)
            }
            _ => Err(TypeError::Unify(t1, t2)),
        }
    }

    // (* The main entry point to type inference *)
    // (* All branches (except for the trivial Unit) of the first match in this function
    //    are translated directly from the rules for algorithm J, given in comments *)
    // (* infer : polytype SMap.t -> Expr -> Type *)
    // let rec infer env : expr -> typ = function
    //     | Unit -> TUnit
    //     (* Var
    //      *   x : s ∊ env
    //      *   t = inst s
    //      *   -----------
    //      *   infer env x = t
    //      *)
    //     | Identifier x ->
    //         let s = SMap.find x env in
    //         let t = inst s in
    //         t
    //     (* App
    //      *   infer env f = t0
    //      *   infer env x = t1
    //      *   t' = newvar ()
    //      *   unify t0 (t1 -> t')
    //      *   ---------------
    //      *   infer env (f x) = t'
    //      *)
    //     | FnCall(f, x) ->
    //         let t0 = infer env f in
    //         let t1 = infer env x in
    //         let t' = newvar_t () in
    //         unify t0 (Fn(t1, t'));
    //         t'
    //     (* Abs
    //      *   t = newvar ()
    //      *   infer (SMap.add x t env) e = t'
    //      *   -------------
    //      *   infer env (fun x -> e) = t -> t'
    //      *)
    //     | Lambda(x, e) ->
    //         let t = newvar_t () in
    //         (* t must be a polytype to go in our map, so make an empty forall *)
    //         let env' = SMap.add x (dont_generalize t) env in
    //         let t' = infer env' e in
    //         Fn(t, t')
    //     (* Let
    //      *   infer env e0 = t
    //      *   infer (SMap.add x (generalize t) env) e1 = t'
    //      *   -----------------
    //      *   infer env (let x = e0 in e1) = t'
    //      *
    //      * enter/exit_level optimizations are from
    //      * http://okmij.org/ftp/ML/generalization.html
    //      * In this implementation, they're required so we
    //      * don't generalize types that escape into the environment.
    //      *)
    //     | Let(x, e0, e1) ->
    //         enter_level ();
    //         let t = infer env e0 in
    //         exit_level ();
    //         let t' = infer (SMap.add x (generalize t) env) e1 in
    //         t'
    pub fn infer(
        &'hm self,
        bindings: &mut Bindings<'_, 'hm>,
        expr: &Expr,
    ) -> Result<&'hm Type<'hm>, TypeError> {
        match expr {
            Expr::Unit => Ok(&Type::Unit),
            Expr::Int(_) => Ok(&Type::Int),
            Expr::Bool(_) => Ok(&Type::Bool),
            Expr::Tuple(tup) => tup
                .iter()
                .map(|expr| self.infer(bindings, expr))
                .collect::<Result<_, _>>()
                .map(|types| &*self.types.alloc(Type::Tuple(types))),
            Expr::Ident(ident) => bindings.get(ident, self).ok_or(TypeError::NotFound),
            Expr::Appl { fun, arg } => {
                let t0 = self.infer(bindings, fun.as_ref())?;
                let t1 = self.infer(bindings, arg.as_ref())?;
                let t = self.new_typevar().1;
                self.unify(
                    t0,
                    self.types.alloc(Type::Fun {
                        input: t1,
                        output: t,
                    }),
                )?;
                Ok(t)
            }
            Expr::Closure(first, branches) => {
                fn type_of_pattern<'hm>(
                    env: &'hm Env<'hm>,
                    bindings: &mut Bindings<'_, 'hm>,
                    pat: &Pat,
                ) -> &'hm Type<'hm> {
                    match pat {
                        Pat::Int => &Type::Int,
                        Pat::Bool => &Type::Bool,
                        Pat::Ident(var) => {
                            // If the pattern in the closure argument is like `|x| ...`,
                            // then `x` is a fresh type and added to the scope such that
                            // inner code using `x` knows that it has that fresh typevar,
                            // but we also return the fresh type to tell us what the
                            // input type is.
                            let ty = env.new_typevar().1;
                            // In the scope, we add the ident name along with the fresh type
                            bindings.add_local(var.clone(), ty);
                            // And finally, the type of this pattern is the fresh type
                            ty
                        }
                        Pat::Tuple(tup) => {
                            let types = tup
                                .iter()
                                .map(|pat| type_of_pattern(env, bindings, pat))
                                .collect();
                            env.types.alloc(Type::Tuple(types))
                        } // When we add struct destructuring, we can unify the type of the field
                          // with the returned type of the pattern in that field.
                          // e.g. If we have a `struct Number(i32)` and have the pattern
                          // `Number(x)`, then `x` might be type `Var(Typevar::Unbound(0))` which
                          // we can unify with `i32` to see that it should be `Var(Typevar::Bound(&Int))`
                    }
                }

                let mut scoped_bindings = bindings.enter_scope();
                let input = type_of_pattern(self, &mut scoped_bindings, &first.param);
                let output = self.infer(&mut scoped_bindings, &first.body)?;
                drop(scoped_bindings);

                for branch in branches {
                    let mut scoped_bindings = bindings.enter_scope();

                    let branch_input = type_of_pattern(self, &mut scoped_bindings, &branch.param);
                    let branch_output = self.infer(&mut scoped_bindings, &branch.body)?;

                    self.unify(input, branch_input)?;
                    self.unify(output, branch_output)?;
                }

                Ok(self.types.alloc(Type::Fun { input, output }))
            }
        }
    }
}

#[derive(Debug, Error, PartialEq)]
enum TypeError<'hm> {
    #[error("Types `{0}` and `{1}` cannot be unified")]
    Unify(&'hm Type<'hm>, &'hm Type<'hm>),
    #[error("Cyclic type: attempted to bind type {0} to {1}")]
    CyclicType(TypevarId, &'hm Type<'hm>),
    #[error("Type not found")]
    NotFound,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        // \f.\x. f x  :  (a -> b) -> a -> b
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("f".into()),
                body: Expr::Closure(
                    Box::new(Branch {
                        param: Pat::Ident("x".into()),
                        body: Expr::Appl {
                            fun: Box::new(Expr::Ident("f".into())),
                            arg: Box::new(Expr::Ident("x".into())),
                        },
                    }),
                    vec![],
                ),
            }),
            vec![],
        );

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "((a1 -> a2) -> (a1 -> a2))");
    }

    #[test]
    fn test2() {
        // \f.\x. f (f x) : (a -> a) -> a -> a
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("f".into()),
                body: Expr::Closure(
                    Box::new(Branch {
                        param: Pat::Ident("x".into()),
                        body: Expr::Appl {
                            fun: Box::new(Expr::Ident("f".into())),
                            arg: Box::new(Expr::Appl {
                                fun: Box::new(Expr::Ident("f".into())),
                                arg: Box::new(Expr::Ident("x".into())),
                            }),
                        },
                    }),
                    vec![],
                ),
            }),
            vec![],
        );

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "((a1 -> a1) -> (a1 -> a1))");
    }

    #[test]
    fn test_plus() {
        // \m.\n.\f.\x. m f (n f x)  :  (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("m".into()),
                body: Expr::Closure(
                    Box::new(Branch {
                        param: Pat::Ident("n".into()),
                        body: Expr::Closure(
                            Box::new(Branch {
                                param: Pat::Ident("f".into()),
                                body: Expr::Closure(
                                    Box::new(Branch {
                                        param: Pat::Ident("x".into()),
                                        body: Expr::Appl {
                                            fun: Box::new(Expr::Appl {
                                                fun: Box::new(Expr::Ident("m".into())),
                                                arg: Box::new(Expr::Ident("f".into())),
                                            }),
                                            arg: Box::new(Expr::Appl {
                                                fun: Box::new(Expr::Appl {
                                                    fun: Box::new(Expr::Ident("n".into())),
                                                    arg: Box::new(Expr::Ident("f".into())),
                                                }),
                                                arg: Box::new(Expr::Ident("x".into())),
                                            }),
                                        },
                                    }),
                                    vec![],
                                ),
                            }),
                            vec![],
                        ),
                    }),
                    vec![],
                ),
            }),
            vec![],
        );

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(
            t.to_string(),
            "((a2 -> (a6 -> a7)) -> ((a2 -> (a3 -> a6)) -> (a2 -> (a3 -> a7))))"
        );
    }

    #[test]
    fn test_succ() {
        // \n.\f.\x. f (n f x)  :  ((a -> b) -> c -> a) -> (a -> b) -> c -> b
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("n".into()),
                body: Expr::Closure(
                    Box::new(Branch {
                        param: Pat::Ident("f".into()),
                        body: Expr::Closure(
                            Box::new(Branch {
                                param: Pat::Ident("x".into()),
                                body: Expr::Appl {
                                    fun: Box::new(Expr::Ident("f".into())),
                                    arg: Box::new(Expr::Appl {
                                        fun: Box::new(Expr::Appl {
                                            fun: Box::new(Expr::Ident("n".into())),
                                            arg: Box::new(Expr::Ident("f".into())),
                                        }),
                                        arg: Box::new(Expr::Ident("x".into())),
                                    }),
                                },
                            }),
                            vec![],
                        ),
                    }),
                    vec![],
                ),
            }),
            vec![],
        );

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(
            t.to_string(),
            "(((a4 -> a5) -> (a2 -> a4)) -> ((a4 -> a5) -> (a2 -> a5)))"
        );
    }

    #[test]
    fn test_mult() {
        // \m.\n.\f.\x. m (n f) x  :  (a -> b -> c) -> (d -> a) -> d -> b -> c
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("m".into()),
                body: Expr::Closure(
                    Box::new(Branch {
                        param: Pat::Ident("n".into()),
                        body: Expr::Closure(
                            Box::new(Branch {
                                param: Pat::Ident("f".into()),
                                body: Expr::Closure(
                                    Box::new(Branch {
                                        param: Pat::Ident("x".into()),
                                        body: Expr::Appl {
                                            fun: Box::new(Expr::Appl {
                                                fun: Box::new(Expr::Ident("m".into())),
                                                arg: Box::new(Expr::Appl {
                                                    fun: Box::new(Expr::Ident("n".into())),
                                                    arg: Box::new(Expr::Ident("f".into())),
                                                }),
                                            }),
                                            arg: Box::new(Expr::Ident("x".into())),
                                        },
                                    }),
                                    vec![],
                                ),
                            }),
                            vec![],
                        ),
                    }),
                    vec![],
                ),
            }),
            vec![],
        );

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(
            t.to_string(),
            "((a4 -> (a3 -> a6)) -> ((a2 -> a4) -> (a2 -> (a3 -> a6))))"
        );
    }

    #[test]
    fn test_pred() {
        // \n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)  :  (((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("n".into()),
                body: Expr::Closure(
                    Box::new(Branch {
                        param: Pat::Ident("f".into()),
                        body: Expr::Closure(
                            Box::new(Branch {
                                param: Pat::Ident("x".into()),
                                body: Expr::Appl {
                                    fun: Box::new(Expr::Appl {
                                        fun: Box::new(Expr::Appl {
                                            fun: Box::new(Expr::Ident("n".into())),
                                            arg: Box::new(Expr::Closure(
                                                Box::new(Branch {
                                                    param: Pat::Ident("g".into()),
                                                    body: Expr::Closure(
                                                        Box::new(Branch {
                                                            param: Pat::Ident("h".into()),
                                                            body: Expr::Appl {
                                                                fun: Box::new(Expr::Ident(
                                                                    "h".into(),
                                                                )),
                                                                arg: Box::new(Expr::Appl {
                                                                    fun: Box::new(Expr::Ident(
                                                                        "g".into(),
                                                                    )),
                                                                    arg: Box::new(Expr::Ident(
                                                                        "f".into(),
                                                                    )),
                                                                }),
                                                            },
                                                        }),
                                                        vec![],
                                                    ),
                                                }),
                                                vec![],
                                            )),
                                        }),
                                        arg: Box::new(Expr::Closure(
                                            Box::new(Branch {
                                                param: Pat::Ident("u".into()),
                                                body: Expr::Ident("x".into()),
                                            }),
                                            vec![],
                                        )),
                                    }),
                                    arg: Box::new(Expr::Closure(
                                        Box::new(Branch {
                                            param: Pat::Ident("u".into()),
                                            body: Expr::Ident("u".into()),
                                        }),
                                        vec![],
                                    )),
                                },
                            }),
                            vec![],
                        ),
                    }),
                    vec![],
                ),
            }),
            vec![],
        );

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(
            t.to_string(),
            "((((a1 -> a5) -> ((a5 -> a6) -> a6)) -> ((a8 -> a2) -> ((a10 -> a10) -> a11))) -> (a1 -> (a2 -> a11)))"
        );
    }

    #[test]
    fn test_int() {
        // \f. f 5  :  (int -> a) -> a
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("f".into()),
                body: Expr::Appl {
                    fun: Box::new(Expr::Ident("f".into())),
                    arg: Box::new(Expr::Int(5)),
                },
            }),
            vec![],
        );

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "((int -> a1) -> a1)");
    }

    #[test]
    fn test_patterns() {
        // \(5, f). f  :  ((int, a) -> a)
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Tuple(vec![Pat::Int, Pat::Ident("f".into())]),
                body: Expr::Ident("f".into()),
            }),
            vec![],
        );
        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "((int, a0) -> a0)");
    }

    #[test]
    fn test_scope_error() {
        // (\x.x) x  :  Type not found
        // Test that variables introduced in closures actually go out of scope
        let expr = Expr::Appl {
            fun: Box::new(Expr::Closure(
                Box::new(Branch {
                    param: Pat::Ident("x".into()),
                    body: Expr::Ident("x".into()),
                }),
                vec![],
            )),
            arg: Box::new(Expr::Ident("x".into())),
        };

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let e = env.infer(&mut bindings, &expr).unwrap_err();
        assert_eq!(e, TypeError::NotFound);
    }

    #[test]
    fn test_cyclic_error() {
        // \f. f f  :  Cyclic type
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("f".into()),
                body: Expr::Appl {
                    fun: Box::new(Expr::Ident("f".into())),
                    arg: Box::new(Expr::Ident("f".into())),
                },
            }),
            vec![],
        );

        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let e = env.infer(&mut bindings, &expr).unwrap_err();
        // println!("{e}");
        assert!(matches!(e, TypeError::CyclicType(_, _)));
    }

    #[test]
    fn test_in() {
        // in (\x.x) 5 : int
        let expr = Expr::Appl {
            fun: Box::new(Expr::Appl {
                fun: Box::new(Expr::Ident("in".into())),
                arg: Box::new(Expr::Closure(
                    Box::new(Branch {
                        param: Pat::Ident("x".into()),
                        body: Expr::Ident("x".into()),
                    }),
                    vec![],
                )),
            }),
            arg: Box::new(Expr::Int(5)),
        };

        let env = Env::new();
        // in: (a -> b) -> (a -> b)
        let (a, a_type) = env.new_typevar();
        let (b, b_type) = env.new_typevar();

        let globals = HashMap::from([(
            "in".into(),
            Polytype {
                typevars: vec![a, b],
                typ: env.types.alloc(Type::Fun {
                    input: env.types.alloc(Type::Fun {
                        input: a_type,
                        output: b_type,
                    }),
                    output: env.types.alloc(Type::Fun {
                        input: a_type,
                        output: b_type,
                    }),
                }),
            },
        )]);

        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "int");
    }

    #[test]
    fn test_in2() {
        // (id 5, id true)
        let expr = Expr::Tuple(vec![
            Expr::Appl {
                fun: Box::new(Expr::Ident("id".into())),
                arg: Box::new(Expr::Int(5)),
            },
            Expr::Appl {
                fun: Box::new(Expr::Ident("id".into())),
                arg: Box::new(Expr::Bool(true)),
            },
        ]);

        let env = Env::new();
        // id: a -> a
        let (a, a_type) = env.new_typevar();

        let globals = HashMap::from([(
            "id".into(),
            Polytype {
                typevars: vec![a],
                typ: env.types.alloc(Type::Fun {
                    input: a_type,
                    output: a_type,
                }),
            },
        )]);

        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "(int, bool)");
    }

    #[test]
    fn test_branching_closures() {
        // \0. 0 else \n. n
        let expr = Expr::Closure(
            Box::new(Branch {
                param: Pat::Ident("n".into()),
                body: Expr::Ident("n".into()),
            }),
            vec![Branch {
                param: Pat::Int,
                body: Expr::Int(0),
            }],
        );
        let env = Env::new();
        let globals = HashMap::new();
        let mut locals = vec![];
        let mut bindings = Bindings::new(&globals, &mut locals);
        let t = env.infer(&mut bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "(int -> int)");
    }
}
