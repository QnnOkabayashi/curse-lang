//! Inspired by: https://github.com/jfecher/algorithm-j/blob/master/j.ml
#![allow(dead_code)]
#![allow(clippy::all)]
use std::{cell::Cell, collections::HashMap, fmt};
use typed_arena::Arena;

// type TypevarId = u64;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TypevarId(u64);

impl fmt::Display for TypevarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "a{}", self.0)
    }
}

#[derive(Debug)]
enum Expr {
    Unit,
    Int(i32),
    Bool(bool),
    Ident(String),
    Tuple(Vec<Expr>),
    Appl { fun: Box<Expr>, arg: Box<Expr> },
    Closure { param: String, body: Box<Expr> },
}

#[derive(Clone, PartialEq)]
enum Type<'hm> {
    Unit,
    Int,
    Bool,
    Tuple(Vec<&'hm Type<'hm>>),
    Var(Cell<Typevar<'hm>>),
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

#[derive(Copy, Clone, PartialEq)]
enum Typevar<'hm> {
    Bound(&'hm Type<'hm>),
    Unbound(TypevarId, u64),
}

impl fmt::Display for Typevar<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Typevar::Bound(t) => write!(f, "{t}"),
            Typevar::Unbound(typevar_id, _) => write!(f, "{typevar_id}"),
        }
    }
}

#[derive(Clone)]
struct Polytype<'hm> {
    typevars: Vec<TypevarId>,
    typ: &'hm Type<'hm>,
}

impl<'hm> Polytype<'hm> {
    pub fn dont_generalize(typ: &'hm Type<'hm>) -> Self {
        Polytype {
            typevars: vec![],
            typ,
        }
    }
}

struct Env<'hm> {
    current_typevar: Cell<u64>,
    current_level: Cell<u64>,
    types: Arena<Type<'hm>>,
}

impl<'hm> Env<'hm> {
    pub fn new() -> Self {
        Env {
            current_typevar: Cell::new(0),
            current_level: Cell::new(1),
            types: Arena::new(),
        }
    }

    pub fn scope<T>(&'hm self, f: impl FnOnce() -> T) -> T {
        let level = self.level();
        self.current_level.set(level + 1);
        let t = f();
        self.current_level.set(level);
        t
    }

    pub fn level(&self) -> u64 {
        self.current_level.get()
    }

    pub fn new_var(&self) -> TypevarId {
        let var = self.current_typevar.get();
        self.current_typevar.set(var + 1);
        TypevarId(var)
    }

    pub fn new_typevar(&'hm self) -> &'hm Type<'hm> {
        self.types.alloc(Type::Var(Cell::new(Typevar::Unbound(
            self.new_var(),
            self.level(),
        ))))
    }

    /// (* specializes the polytype s by copying the term and replacing the
    ///     * bound type variables consistently by new monotype variables
    ///     * E.g.   inst (forall a b. a -> b -> a) = c -> d -> c     *)
    ///    let inst (PolyType(typevars, typ)) : typ =
    ///        (* Replace any typevars found in the Hashtbl with the
    ///         * associated value in the same table, leave them otherwise *)
    ///        let rec replace_tvs tbl = function
    ///            | TUnit -> TUnit
    ///            | TVar({ contents = Bound t }) -> replace_tvs tbl t
    ///            | TVar({ contents = Unbound (n, level)}) as t ->
    ///                begin match ITbl.find_opt tbl n with
    ///                | Some t' -> t'
    ///                | None -> t
    ///                end
    ///            | Fn(a, b) -> Fn(replace_tvs tbl a, replace_tvs tbl b)
    ///        in
    ///        (* Note that the returned type is no longer a PolyType,
    ///         * this means it is now monomorphic, the 'forall' is gone. *)
    ///        let tvs_to_replace = ITbl.create 1 in
    ///        List.iter (fun tv -> ITbl.add tvs_to_replace tv (newvar_t ())) typevars;
    ///        replace_tvs tvs_to_replace typ
    pub fn monomorphize(&'hm self, polytype: &Polytype<'hm>) -> &'hm Type<'hm> {
        fn replace_tvs<'hm>(
            tbl: &HashMap<TypevarId, &'hm Type<'hm>>,
            env: &'hm Env<'hm>,
            ty: &'hm Type<'hm>,
        ) -> &'hm Type<'hm> {
            match ty {
                Type::Var(typevar) => match typevar.get() {
                    Typevar::Bound(t) => replace_tvs(tbl, env, t),
                    Typevar::Unbound(typevar_id, _) => match tbl.get(&typevar_id) {
                        Some(t) => *t,
                        None => ty,
                    },
                },
                Type::Fun { input, output } => env.types.alloc(Type::Fun {
                    input: replace_tvs(tbl, env, input),
                    output: replace_tvs(tbl, env, output),
                }),
                other => other,
            }
        }

        let tvs_to_replace = polytype
            .typevars
            .iter()
            .map(|tv| (*tv, self.new_typevar()))
            .collect();

        replace_tvs(&tvs_to_replace, self, polytype.typ)
    }

    ///     (* Can a monomorphic TVar(a) be found inside this type? *)
    /// let rec occurs a_id a_level (* in *) = function
    ///     | TUnit -> false
    ///     | TVar({ contents = Bound t }) -> occurs a_id a_level t
    ///     | TVar({ contents = Unbound(b_id, b_level)} as b_typevar) ->
    ///         let min_level = min a_level b_level in
    ///         b_typevar := Unbound (b_id, min_level);
    ///         a_id = b_id
    ///     | Fn(b, c) -> occurs a_id a_level b || occurs a_id a_level c
    pub fn occurs(&self, a_id: TypevarId, a_level: u64, typ: &'hm Type<'hm>) -> bool {
        match typ {
            Type::Var(typevar) => match typevar.get() {
                Typevar::Bound(t) => self.occurs(a_id, a_level, t),
                Typevar::Unbound(b_id, b_level) => {
                    let min_level = a_level.min(b_level);
                    typevar.set(Typevar::Unbound(b_id, min_level));
                    a_id == b_id
                }
            },
            Type::Fun { input, output } => {
                self.occurs(a_id, a_level, input) || self.occurs(a_id, a_level, output)
            }
            _ => false,
        }
    }

    /// let rec unify (t1: typ) (t2: typ) : unit =
    /// match (t1, t2) with
    /// | (TUnit, TUnit) -> ()
    /// (* These two recursive calls to the bound typevar replace
    /// * the 'find' in the union-find algorithm *)
    /// | (TVar({ contents = Bound a' }), b) -> unify a' b
    /// | (a, TVar({ contents = Bound b' })) -> unify a b'
    /// | (TVar({ contents = Unbound(a_id, a_level) } as a), b) ->
    ///     (* create binding for boundTy that is currently empty *)
    ///     if t1 = t2 then () else (* a = a, but dont create a recursive binding to itself *)
    ///     if occurs a_id a_level b then raise TypeError else
    ///     a := Bound b
    /// | (a, TVar({ contents = Unbound(b_id, b_level)} as b)) ->
    ///     (* create binding for boundTy that is currently empty *)
    ///     if t1 = t2 then () else
    ///     if occurs b_id b_level a then raise TypeError else
    ///     b := Bound a
    /// | (Fn(a, b), Fn(c, d)) ->
    ///     unify a c;
    ///     unify b d
    /// | (a, b) -> raise TypeError
    pub fn unify(&self, t1: &'hm Type<'hm>, t2: &'hm Type<'hm>) -> Result<(), Error> {
        match (t1, t2) {
            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Var(typevar), a) | (a, Type::Var(typevar)) => match typevar.get() {
                Typevar::Bound(b) => self.unify(a, b),
                Typevar::Unbound(a_id, a_level) => {
                    if t1 == t2 {
                        Ok(())
                    } else if self.occurs(a_id, a_level, a) {
                        Err(Error::TypeError(format!("a_id {a_id} occurs {a}")))
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
            _ => Err(Error::TypeError(format!(
                "types aren't even CLOSE: {t1} and {t2}"
            ))),
        }
    }

    // (* Find all typevars and wrap the type in a PolyType *)
    // (* e.g.  generalize (a -> b -> b) = forall a b. a -> b -> b  *)
    // let generalize (t: typ) : polytype =
    //     (* collect all the monomorphic typevars *)
    //     let rec find_all_tvs = function
    //         | TUnit -> []
    //         | TVar({ contents = Bound t }) -> find_all_tvs t
    //         | TVar({ contents = Unbound (n, level)}) ->
    //             if level > !current_level then [n]
    //             else []
    //         | Fn(a, b) -> find_all_tvs a @ find_all_tvs b
    //     in find_all_tvs t
    //     |> List.sort_uniq compare
    //     |> fun typevars -> PolyType(typevars, t)
    // pub fn generalize(&'hm self, typ: &'hm Type<'hm>) -> Polytype<'hm> {
    //     fn find_all_tvs<'hm>(env: &'hm Env<'hm>, typ: &'hm Type<'hm>, out: &mut Vec<TypevarId>) {
    //         match typ {
    //             Type::Var(typevar) => match typevar.get() {
    //                 Typevar::Bound(t) => find_all_tvs(env, t, out),
    //                 Typevar::Unbound(id, level) => {
    //                     if level > env.level() {
    //                         out.push(id);
    //                     }
    //                 }
    //             },
    //             Type::Fun { input, output } => {
    //                 find_all_tvs(env, *input, out);
    //                 find_all_tvs(env, *output, out);
    //             }
    //             _ => {}
    //         }
    //     }

    //     let mut typevars: Vec<TypevarId> = vec![];
    //     find_all_tvs(self, typ, &mut typevars);
    //     typevars.sort_unstable();
    //     typevars.dedup();
    //     Polytype { typevars, typ }
    // }

    /// (* The main entry point to type inference *)
    /// (* All branches (except for the trivial Unit) of the first match in this function
    ///    are translated directly from the rules for algorithm J, given in comments *)
    /// (* infer : polytype SMap.t -> Expr -> Type *)
    /// let rec infer env : expr -> typ = function
    ///     | Unit -> TUnit
    ///     (* Var
    ///      *   x : s ∊ env
    ///      *   t = inst s
    ///      *   -----------
    ///      *   infer env x = t
    ///      *)
    ///     | Identifier x ->
    ///         let s = SMap.find x env in
    ///         let t = inst s in
    ///         t
    ///     (* App
    ///      *   infer env f = t0
    ///      *   infer env x = t1
    ///      *   t' = newvar ()
    ///      *   unify t0 (t1 -> t')
    ///      *   ---------------
    ///      *   infer env (f x) = t'
    ///      *)
    ///     | FnCall(f, x) ->
    ///         let t0 = infer env f in
    ///         let t1 = infer env x in
    ///         let t' = newvar_t () in
    ///         unify t0 (Fn(t1, t'));
    ///         t'
    ///     (* Abs
    ///      *   t = newvar ()
    ///      *   infer (SMap.add x t env) e = t'
    ///      *   -------------
    ///      *   infer env (fun x -> e) = t -> t'
    ///      *)
    ///     | Lambda(x, e) ->
    ///         let t = newvar_t () in
    ///         (* t must be a polytype to go in our map, so make an empty forall *)
    ///         let env' = SMap.add x (dont_generalize t) env in
    ///         let t' = infer env' e in
    ///         Fn(t, t')
    ///     (* Let
    ///      *   infer env e0 = t
    ///      *   infer (SMap.add x (generalize t) env) e1 = t'
    ///      *   -----------------
    ///      *   infer env (let x = e0 in e1) = t'
    ///      *
    ///      * enter/exit_level optimizations are from
    ///      * http://okmij.org/ftp/ML/generalization.html
    ///      * In this implementation, they're required so we
    ///      * don't generalize types that escape into the environment.
    ///      *)
    ///     | Let(x, e0, e1) ->
    ///         enter_level ();
    ///         let t = infer env e0 in
    ///         exit_level ();
    ///         let t' = infer (SMap.add x (generalize t) env) e1 in
    ///         t'
    pub fn infer(
        &'hm self,
        bindings: &HashMap<String, Polytype<'hm>>,
        expr: &Expr,
    ) -> Result<&'hm Type<'hm>, Error> {
        match expr {
            Expr::Unit => Ok(&Type::Unit),
            Expr::Int(_) => Ok(&Type::Int),
            Expr::Bool(_) => Ok(&Type::Bool),
            Expr::Tuple(tup) => tup
                .iter()
                .map(|expr| self.infer(bindings, expr))
                .collect::<Result<_, _>>()
                .map(|types| &*self.types.alloc(Type::Tuple(types))),
            Expr::Ident(ident) => bindings
                .get(ident)
                .map(|s| self.monomorphize(s))
                .ok_or(Error::NotFound),
            Expr::Appl { fun, arg } => {
                let t0 = self.infer(bindings, fun.as_ref())?;
                let t1 = self.infer(bindings, arg.as_ref())?;
                let t = self.new_typevar();
                self.unify(
                    t0,
                    self.types.alloc(Type::Fun {
                        input: t1,
                        output: t,
                    }),
                )?;
                Ok(t)
            }
            Expr::Closure { param, body } => {
                let input = self.new_typevar();
                // THIS IS ENORMOUS PAIN
                let mut bindings = bindings.clone();
                bindings.insert(param.clone(), Polytype::dont_generalize(input));
                let output = self.infer(&bindings, body.as_ref())?;
                Ok(self.types.alloc(Type::Fun { input, output }))
            }
        }
    }
}

#[derive(Debug)]
enum Error {
    TypeError(String),
    NotFound,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        // \f.\x. f x  :  (a -> b) -> a -> b
        let expr = Expr::Closure {
            param: "f".into(),
            body: Box::new(Expr::Closure {
                param: "x".into(),
                body: Box::new(Expr::Appl {
                    fun: Box::new(Expr::Ident("f".into())),
                    arg: Box::new(Expr::Ident("x".into())),
                }),
            }),
        };

        let env = Env::new();
        let bindings = HashMap::new();
        let t = env.infer(&bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "((a1 -> a2) -> (a1 -> a2))");
    }

    #[test]
    fn test2() {
        // \f.\x. f (f x) : (a -> a) -> a -> a
        let expr = Expr::Closure {
            param: "f".into(),
            body: Box::new(Expr::Closure {
                param: "x".into(),
                body: Box::new(Expr::Appl {
                    fun: Box::new(Expr::Ident("f".into())),
                    arg: Box::new(Expr::Appl {
                        fun: Box::new(Expr::Ident("f".into())),
                        arg: Box::new(Expr::Ident("x".into())),
                    }),
                }),
            }),
        };

        let env = Env::new();
        let bindings = HashMap::new();
        let t = env.infer(&bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "((a1 -> a1) -> (a1 -> a1))");
    }

    #[test]
    fn test_plus() {
        // \m.\n.\f.\x. m f (n f x)  :  (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
        let expr = Expr::Closure {
            param: "m".into(),
            body: Box::new(Expr::Closure {
                param: "n".into(),
                body: Box::new(Expr::Closure {
                    param: "f".into(),
                    body: Box::new(Expr::Closure {
                        param: "x".into(),
                        body: Box::new(Expr::Appl {
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
                        }),
                    }),
                }),
            }),
        };

        let env = Env::new();
        let bindings = HashMap::new();
        let t = env.infer(&bindings, &expr).unwrap();
        assert_eq!(
            t.to_string(),
            "((a2 -> (a6 -> a7)) -> ((a2 -> (a3 -> a6)) -> (a2 -> (a3 -> a7))))"
        );
    }

    #[test]
    fn test_succ() {
        // \n.\f.\x. f (n f x)  :  ((a -> b) -> c -> a) -> (a -> b) -> c -> b
        let expr = Expr::Closure {
            param: "n".into(),
            body: Box::new(Expr::Closure {
                param: "f".into(),
                body: Box::new(Expr::Closure {
                    param: "x".into(),
                    body: Box::new(Expr::Appl {
                        fun: Box::new(Expr::Ident("f".into())),
                        arg: Box::new(Expr::Appl {
                            fun: Box::new(Expr::Appl {
                                fun: Box::new(Expr::Ident("n".into())),
                                arg: Box::new(Expr::Ident("f".into())),
                            }),
                            arg: Box::new(Expr::Ident("x".into())),
                        }),
                    }),
                }),
            }),
        };

        let env = Env::new();
        let bindings = HashMap::new();
        let t = env.infer(&bindings, &expr).unwrap();
        assert_eq!(
            t.to_string(),
            "(((a4 -> a5) -> (a2 -> a4)) -> ((a4 -> a5) -> (a2 -> a5)))"
        );
    }

    #[test]
    fn test_mult() {
        // \m.\n.\f.\x. m (n f) x  :  (a -> b -> c) -> (d -> a) -> d -> b -> c
        let expr = Expr::Closure {
            param: "m".into(),
            body: Box::new(Expr::Closure {
                param: "n".into(),
                body: Box::new(Expr::Closure {
                    param: "f".into(),
                    body: Box::new(Expr::Closure {
                        param: "x".into(),
                        body: Box::new(Expr::Appl {
                            fun: Box::new(Expr::Appl {
                                fun: Box::new(Expr::Ident("m".into())),
                                arg: Box::new(Expr::Appl {
                                    fun: Box::new(Expr::Ident("n".into())),
                                    arg: Box::new(Expr::Ident("f".into())),
                                }),
                            }),
                            arg: Box::new(Expr::Ident("x".into())),
                        }),
                    }),
                }),
            }),
        };

        let env = Env::new();
        let bindings = HashMap::new();
        let t = env.infer(&bindings, &expr).unwrap();
        assert_eq!(
            t.to_string(),
            "((a4 -> (a3 -> a6)) -> ((a2 -> a4) -> (a2 -> (a3 -> a6))))"
        );
    }

    #[test]
    fn test_pred() {
        // \n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)  :  (((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g
        let expr = Expr::Closure {
            param: "n".into(),
            body: Box::new(Expr::Closure {
                param: "f".into(),
                body: Box::new(Expr::Closure {
                    param: "x".into(),
                    body: Box::new(Expr::Appl {
                        fun: Box::new(Expr::Appl {
                            fun: Box::new(Expr::Appl {
                                fun: Box::new(Expr::Ident("n".into())),
                                arg: Box::new(Expr::Closure {
                                    param: "g".into(),
                                    body: Box::new(Expr::Closure {
                                        param: "h".into(),
                                        body: Box::new(Expr::Appl {
                                            fun: Box::new(Expr::Ident("h".into())),
                                            arg: Box::new(Expr::Appl {
                                                fun: Box::new(Expr::Ident("g".into())),
                                                arg: Box::new(Expr::Ident("f".into())),
                                            }),
                                        }),
                                    }),
                                }),
                            }),
                            arg: Box::new(Expr::Closure {
                                param: "u".into(),
                                body: Box::new(Expr::Ident("x".into())),
                            }),
                        }),
                        arg: Box::new(Expr::Closure {
                            param: "u".into(),
                            body: Box::new(Expr::Ident("u".into())),
                        }),
                    }),
                }),
            }),
        };

        let env = Env::new();
        let bindings = HashMap::new();
        let t = env.infer(&bindings, &expr).unwrap();
        assert_eq!(
            t.to_string(),
            "((((a1 -> a5) -> ((a5 -> a6) -> a6)) -> ((a8 -> a2) -> ((a10 -> a10) -> a11))) -> (a1 -> (a2 -> a11)))"
        );
    }

    #[test]
    fn test_int() {
        // \f. f 5  :  (int -> a) -> a
        let expr = Expr::Closure {
            param: "f".into(),
            body: Box::new(Expr::Appl {
                fun: Box::new(Expr::Ident("f".into())),
                arg: Box::new(Expr::Int(5)),
            }),
        };

        let env = Env::new();
        let bindings = HashMap::new();
        let t = env.infer(&bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "((int -> a1) -> a1)");
    }

    #[test]
    fn test_in() {
        // in (\x.x) 5 : int
        let expr = Expr::Appl {
            fun: Box::new(Expr::Appl {
                fun: Box::new(Expr::Ident("in".into())),
                arg: Box::new(Expr::Closure {
                    param: "x".into(),
                    body: Box::new(Expr::Ident("x".into())),
                }),
            }),
            arg: Box::new(Expr::Int(5)),
        };

        let env = Env::new();
        // in: (a -> b) -> (a -> b)
        let a = env.new_var();
        let b = env.new_var();
        let a_type = &*env
            .types
            .alloc(Type::Var(Cell::new(Typevar::Unbound(a, env.level()))));
        let b_type = &*env
            .types
            .alloc(Type::Var(Cell::new(Typevar::Unbound(b, env.level()))));

        let bindings = HashMap::from([(
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

        let t = env.infer(&bindings, &expr).unwrap();
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
        let a = env.new_var();
        let a_type = &*env
            .types
            .alloc(Type::Var(Cell::new(Typevar::Unbound(a, env.level()))));

        let bindings = HashMap::from([(
            "id".into(),
            Polytype {
                typevars: vec![a],
                typ: env.types.alloc(Type::Fun {
                    input: a_type,
                    output: a_type,
                }),
            },
        )]);

        let t = env.infer(&bindings, &expr).unwrap();
        assert_eq!(t.to_string(), "(int, bool)");
    }
}
