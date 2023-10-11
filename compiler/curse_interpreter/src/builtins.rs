use crate::{error::EvalError, value::Value};

#[derive(Copy, Clone, Debug)]
pub enum Builtin {
    Add,
    Mul,
    Sub,
    Div,
    Mod,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Semi,
}

impl Builtin {
    pub fn compute<'hir>(
        self,
        lhs: Value<'hir>,
        rhs: Value<'hir>,
    ) -> Result<Value<'hir>, EvalError> {
        use Builtin::*;
        match (self, lhs, rhs) {
            (Add, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs + rhs)),
            (Mul, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs * rhs)),
            (Sub, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs - rhs)),
            (Div, Value::Integer(lhs), Value::Integer(rhs)) => {
                if let Some(quotient) = lhs.checked_div(rhs) {
                    Ok(Value::Integer(quotient))
                } else {
                    Err(EvalError::DivByZero)
                }
            }
            (Mod, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs % rhs)),
            (Lt, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (Gt, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (Le, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            (Ge, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            (Eq, Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs == rhs)),
            (Eq, Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs == rhs)),
            (Semi, _lhs, rhs) => Ok(rhs),
            (builtin, l, r) => Err(EvalError::TypeMismatch(format!(
                "function call `{l:?} {builtin:?} {r:?}` is invalid"
            ))),
        }
    }
}
