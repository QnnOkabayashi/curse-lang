use curse_interner::InternedString;

pub type Ident = InternedString;

/// Represents literal values, records and functions are created via CPSFix and CPSRecord
/// respectively then get named. As a result, everything is either an integer literal or a name.
/// Booleans can be `0` or `1`, since everything's been typechecked already.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Value {
    Var(Ident),
    Int(u32),
}

pub fn var(s: &str) -> Value {
    Value::Var(InternedString::get_or_intern(s))
}

pub fn var_from_id(id: curse_interner::Ident) -> Value {
    Value::Var(id.symbol)
}

/// Primitive operations (special things handled by the compiler).
#[derive(Debug, Eq, PartialEq)]
pub enum Primop {
    Plus,
    Times,
    Minus,
    Div,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CPSExpr {
    /// Evaluate a primitive operation.
    Primop(CPSPrimop),

    /// Construct a record out of a literal.
    Record(CPSRecord),

    /// Apply a function to its args.
    /// Note: doesn't need a continuation since functions never return.
    Appl(Appl),

    /// Define several functions. Mutually recursive functions must be defined in the
    /// same `Fix` CPSExpr.
    Fix(Fix),

    /// Useful to have an endpoint somewhere for testing purposes.
    Halt(Value),
}

#[derive(Debug, Eq, PartialEq)]
pub struct CPSPrimop {
    pub primop: Primop,
    pub left: Value,
    pub right: Value,
    pub name: Ident,
    pub continuation: Box<CPSExpr>,
}

impl CPSPrimop {
    pub fn new(
        primop: Primop,
        left: Value,
        right: Value,
        name: Ident,
        continuation: Box<CPSExpr>,
    ) -> CPSExpr {
        CPSExpr::Primop(Self {
            primop,
            left,
            right,
            name,
            continuation,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CPSRecord {
    pub values: Vec<Value>,
    pub name: Ident,
    pub continuation: Box<CPSExpr>,
}

impl CPSRecord {
    pub fn new(values: Vec<Value>, name: Ident, continuation: Box<CPSExpr>) -> CPSExpr {
        CPSExpr::Record(Self {
            values,
            name,
            continuation,
        })
    }
}

/// User created functions always take three arguments, but continuations only take one. There
/// might be a better way to handle this.
#[derive(Debug, PartialEq, Eq)]
pub struct Appl {
    function: Value,
    args: Vec<Value>,
}

impl Appl {
    pub fn new(function: Value, args: Vec<Value>) -> CPSExpr {
        CPSExpr::Appl(Self { function, args })
    }
}

/// Every function takes three args now: `left`, `right`, and a continuation.
#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    left: Value,
    name: Value,
    right: Value,
    continuation: Box<CPSExpr>,
}

impl Function {
    pub fn new(left: Value, name: Value, right: Value, continuation: Box<CPSExpr>) -> Self {
        Self {
            left,
            name,
            right,
            continuation,
        }
    }
}

/// A `Vec` of functions to define (basically always either the list of top level functions or a
/// single closure) and a continuation representing what to do next.
#[derive(Debug, PartialEq, Eq)]
pub struct Fix {
    functions: Vec<Function>,
    continuation: Box<CPSExpr>,
}

impl Fix {
    pub fn new(functions: Vec<Function>, continuation: Box<CPSExpr>) -> CPSExpr {
        CPSExpr::Fix(Self {
            functions,
            continuation,
        })
    }
}
