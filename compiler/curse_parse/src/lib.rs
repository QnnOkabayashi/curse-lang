use curse_ast::{Context, Expr, Program};
use lalrpop_util::lalrpop_mod;

mod error;
pub use error::{ParseError, SourceErrors};

mod lexer;
use lexer::{LexError, Lexer, Token};

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar
);

pub fn parse_program<'ast, 'input>(
    context: &'ast Context<'ast, 'input>,
    input: &'input str,
) -> Result<Program<'ast, 'input>, lalrpop_util::ParseError<usize, Token<'input>, LexError>> {
    let lex = Lexer::new(input);
    grammar::ProgramParser::new().parse(context, lex)
}

pub fn parse_expr<'ast, 'input>(
    context: &'ast Context<'ast, 'input>,
    input: &'input str,
) -> Result<&'ast Expr<'ast, 'input>, lalrpop_util::ParseError<usize, Token<'input>, LexError>> {
    let lex = Lexer::new(input);
    grammar::EndExprParser::new().parse(context, lex)
}
