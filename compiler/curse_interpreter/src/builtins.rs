use crate::{error::EvalError, value::Value};

macro_rules! binary_operation {
    ($name:ident = $rettype:ident: $lhs:ident $op:tt $rhs:ident) => {
        pub fn $name<'hir>(
            lhs: Value<'hir>,
            rhs: Value<'hir>,
        ) -> Result<Value<'hir>, EvalError> {
            match (lhs, rhs) {
                (Value::Integer($lhs), Value::Integer($rhs)) => Ok(Value::$rettype($lhs $op $rhs)),
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

pub fn semi<'hir>(_lhs: Value<'hir>, rhs: Value<'hir>) -> Result<Value<'hir>, EvalError> {
    Ok(rhs)
}
