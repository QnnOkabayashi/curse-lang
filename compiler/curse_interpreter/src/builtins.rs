use std::rc::Rc;

use crate::{
    error::EvalError,
    value::{Value, ValueRef},
};

macro_rules! binary_operation {
    ($name:ident = $rettype:ident: $lhs:ident $op:tt $rhs:ident) => {
        pub fn $name<'hir>(
            lhs: ValueRef<'hir>,
            rhs: ValueRef<'hir>,
        ) -> Result<ValueRef<'hir>, EvalError> {
            match (lhs.as_ref(), rhs.as_ref()) {
                (Value::Integer($lhs), Value::Integer($rhs)) => Ok(Rc::new(Value::$rettype($lhs $op $rhs))),
                _ => Err(EvalError::TypeMismatch),
            }
        }
    };
}

binary_operation!(add = Integer: n + m);
binary_operation!(mul = Integer: n * m);
binary_operation!(sub = Integer: n - m);
binary_operation!(div = Integer: n / m);
binary_operation!(modulo = Integer: n % m);

binary_operation!(eq = Bool: n == m);
binary_operation!(lt = Bool: n < m);
binary_operation!(gt = Bool: n > m);
binary_operation!(le = Bool: n <= m);
binary_operation!(ge = Bool: n >= m);

pub fn semi<'hir>(_lhs: ValueRef<'hir>, rhs: ValueRef<'hir>) -> Result<ValueRef<'hir>, EvalError> {
    Ok(rhs)
}
