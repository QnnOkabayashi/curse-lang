use crate::{tok, ty, Expr, ExprPat, Punct, Res, Span, Type};

#[derive(Clone, Debug)]
pub enum ExprClosure<'ast, 'input> {
    NonPiecewise(ExprArm<'ast, 'input>),
    Piecewise {
        lbrace: tok::LBrace,
        arm1: ExprArm<'ast, 'input>,
        arms: Vec<(tok::Comma, ExprArm<'ast, 'input>)>,
        trailing_comma: Option<tok::Comma>,
        rbrace: tok::RBrace,
    },
}

impl Span for ExprClosure<'_, '_> {
    fn span(&self) -> (usize, usize) {
        match self {
            ExprClosure::NonPiecewise(arm) => arm.span(),
            ExprClosure::Piecewise { lbrace, rbrace, .. } => lbrace.span_between(rbrace),
        }
    }
}

impl<'ast, 'input> ExprClosure<'ast, 'input> {
    pub fn nonpiecewise_from_grammar(res_arm: Res<ExprArm<'ast, 'input>>) -> Res<Self> {
        Ok(ExprClosure::NonPiecewise(res_arm?))
    }

    pub fn piecewise_from_grammar(
        lbrace: tok::LBrace,
        res_arm1: Res<ExprArm<'ast, 'input>>,
        arms: Vec<(tok::Comma, Res<ExprArm<'ast, 'input>>)>,
        trailing_comma: Option<tok::Comma>,
        rbrace: tok::RBrace,
    ) -> Res<Self> {
        let arms = arms
            .into_iter()
            .map(|(comma, res_arm)| res_arm.map(|arm| (comma, arm)))
            .collect::<Res<_>>()?;

        Ok(ExprClosure::Piecewise {
            lbrace,
            arm1: res_arm1?,
            arms,
            trailing_comma,
            rbrace,
        })
    }

    pub fn num_arms(&self) -> usize {
        match self {
            ExprClosure::NonPiecewise(_) => 1,
            ExprClosure::Piecewise { arms, .. } => 1 + arms.len(),
        }
    }

    pub fn head(&self) -> &ExprArm<'ast, 'input> {
        match self {
            ExprClosure::NonPiecewise(head) => head,
            ExprClosure::Piecewise { arm1: head, .. } => head,
        }
    }

    pub fn tail(&self) -> Option<&[(tok::Comma, ExprArm<'ast, 'input>)]> {
        match self {
            ExprClosure::NonPiecewise(_) => None,
            ExprClosure::Piecewise { arms: tail, .. } => Some(tail),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprArm<'ast, 'input> {
    pub open: tok::Pipe,
    pub params: Punct<ExprParam<'ast, 'input>, tok::Comma>,
    pub close: tok::Pipe,
    pub body: &'ast Expr<'ast, 'input>,
}

impl<'ast, 'input> ExprArm<'ast, 'input> {
    pub fn from_grammar(
        open: tok::Pipe,
        params: Vec<(Res<ExprParam<'ast, 'input>>, tok::Comma)>,
        trailing: Option<Res<ExprParam<'ast, 'input>>>,
        close: tok::Pipe,
        res_body: Res<&'ast Expr<'ast, 'input>>,
    ) -> Res<Self> {
        let elements = params
            .into_iter()
            .map(|(res_param, comma)| res_param.map(|param| (param, comma)))
            .collect::<Res<_>>()?;

        Ok(ExprArm {
            open,
            params: Punct {
                elements,
                trailing: trailing.transpose()?,
            },
            close,
            body: res_body?,
        })
    }
}

impl Span for ExprArm<'_, '_> {
    fn span(&self) -> (usize, usize) {
        self.open.span_between(self.body)
    }
}

#[derive(Clone, Debug)]
pub struct ExprParam<'ast, 'input> {
    pub pat: &'ast ExprPat<'ast, 'input>,
    pub ty: Option<(tok::Colon, &'ast ty::Type<'ast, 'input>)>,
}

impl<'ast, 'input> ExprParam<'ast, 'input> {
    pub fn from_grammar(
        pat: &'ast ExprPat<'ast, 'input>,
        opt_annotation: Option<(tok::Colon, Res<&'ast Type<'ast, 'input>>)>,
    ) -> Res<Self> {
        opt_annotation
            .map(|(colon, res_ty)| res_ty.map(|ty| (colon, ty)))
            .transpose()
            .map(|ty| ExprParam { pat, ty })
    }
}
