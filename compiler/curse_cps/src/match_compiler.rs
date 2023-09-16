// big thanks to [Jules Jacobs](https://julesjacobs.com/notes/patternmatching/patternmatching.pdf)
// and [Yorick Peterse](https://github.com/yorickpeterse/pattern-matching-in-rust)

use std::collections::HashMap;

use curse_hir::hir;
use curse_interner::InternedString;

type Variable = InternedString;

pub enum Constructor<'hir> {
    Named(hir::Path<'hir>),
    Integer(u32),
}

#[derive(Clone)]
struct Row<'hir> {
    columns: Vec<Column<'hir>>,
    body: Body<'hir>,
}

impl<'hir> Row<'hir> {
    fn new(columns: Vec<Column<'hir>>, body: Body<'hir>) -> Self {
        Self { columns, body }
    }

    fn remove_column(&mut self, column: &Column) -> Option<Column> {
        self.columns
            .iter()
            .position(|c| c == column)
            .map(|idx| self.columns.remove(idx))
    }
}

#[derive(Clone)]
pub struct Body<'hir> {
    bindings: Vec<(InternedString, InternedString)>,
    value: hir::Expr<'hir>,
}

impl<'hir> Body<'hir> {
    fn new(bindings: Vec<(InternedString, InternedString)>, value: hir::Expr<'hir>) -> Self {
        Self { bindings, value }
    }
}

// TODO(william): Ideally, I'd like the constructor names and paths resolved in the hir and just
// replaced with numeric id's or something. In particular, since the types have already been
// checked, we could just number the variants, which would simplify things quite a bit,
// especially if we end up compiling this.
#[derive(PartialEq, Eq, Clone)]
struct Column<'hir> {
    variable: Variable,
    pattern: hir::PatRef<'hir>,
}

impl<'hir> Column<'hir> {
    fn new(variable: Variable, pattern: hir::PatRef<'hir>) -> Self {
        Self { variable, pattern }
    }
}

pub struct Case<'hir> {
    constructor: Constructor<'hir>,
    argument: Vec<Variable>,
    body: Decision<'hir>,
}

impl<'hir> Case<'hir> {
    pub fn new(
        constructor: Constructor<'hir>,
        argument: Vec<Variable>,
        body: Decision<'hir>,
    ) -> Self {
        Self {
            constructor,
            argument,
            body,
        }
    }
}

pub enum Decision<'hir> {
    Success(Body<'hir>),
    Failure,
    Switch(Variable, Vec<Case<'hir>>, Option<Box<Decision<'hir>>>),
}

pub struct Compiler {}

impl<'hir> Compiler {
    pub fn compile(mut self, arms: &'hir [hir::Arm<'hir>], arg: InternedString) -> Decision<'hir> {
        let rows = arms
            .iter()
            .map(|hir::Arm { params, body }| {
                Row::new(
                    params
                        .iter()
                        .map(|hir::Param { pat, .. }| Column::new(arg, pat))
                        .collect(),
                    Body::new(vec![], **body),
                )
            })
            .collect();
        self.compile_rows(rows)
    }

    fn compile_rows(&mut self, rows: Vec<Row<'hir>>) -> Decision<'hir> {
        if rows.is_empty() {
            return Decision::Failure;
        }

        let mut rows = rows
            .into_iter()
            .map(|row| self.move_variable_patterns(row))
            .collect::<Vec<_>>();

        // if the first row has no columns, then we've moved all the variables to the RHS
        // and it will always match
        if rows.first().map_or(false, |c| c.columns.is_empty()) {
            let row = rows.remove(0);

            return Decision::Success(row.body);
        }

        // find the variable in rows[0] that's referred to the most across all rows
        let branch_col = self.branch_column(&rows[0], &rows);

        match branch_col.pattern.kind {
            hir::PatKind::Lit(hir::Lit::Integer(n)) => {
            }
            hir::PatKind::Lit(hir::Lit::Bool(_)) => todo!(),
            hir::PatKind::Lit(hir::Lit::Ident(_)) => {
                unreachable!("due to `move_variable_patterns`")
            }
            hir::PatKind::Record(_) => todo!(),
            hir::PatKind::Constructor(_, _) => todo!(),
            hir::PatKind::Error => todo!(),
        }
    }

    fn move_variable_patterns(&self, row: Row<'hir>) -> Row<'hir> {
        let mut bindings = row.body.bindings;

        for col in &row.columns {
            if let hir::PatKind::Lit(hir::Lit::Ident(bind)) = col.pattern.kind {
                bindings.push((bind.symbol, col.variable));
            }
        }

        let columns = row
            .columns
            .into_iter()
            .filter(|col| !matches!(col.pattern.kind, hir::PatKind::Lit(hir::Lit::Ident(_))))
            .collect();

        Row {
            columns,
            body: Body {
                bindings,
                value: row.body.value,
            },
        }
    }

    fn branch_column<'a>(&'a self, row: &'a Row<'hir>, rows: &'a [Row<'hir>]) -> &'a Column {
        let mut counts = HashMap::new();

        for row in rows {
            for col in &row.columns {
                *counts.entry(&col.variable).or_insert(0usize) += 1;
            }
        }

        row.columns
            .iter()
            .max_by_key(|col| counts[&col.variable])
            .unwrap()
    }

    fn compile_int_cases(
        &mut self,
        rows: Vec<Row<'hir>>,
        branch_col: Column<'hir>,
    ) -> (Vec<Case<'hir>>, Box<Decision<'hir>>) {
        let mut raw_cases: Vec<(Constructor, Vec<Variable>, Vec<Row>)> = Vec::new();
        let mut fallback_rows = Vec::new();
        let mut tested: HashMap<u32, usize> = HashMap::new();

        for mut row in rows {
            if let Some(col) = row.remove_column(&branch_col) {
                let hir::PatKind::Lit(hir::Lit::Integer(val)) = col.pattern.kind else {
                    unreachable!("only called if pattern is an `int`")
                };

                if let Some(index) = tested.get(&val) {
                    raw_cases[*index].2.push(row);
                    continue;
                }

                tested.insert(val, raw_cases.len());
                raw_cases.push((Constructor::Integer(val), Vec::new(), vec![row]));
            } else {
                fallback_rows.push(row);
            }
        }

        for (_, _, rows) in &mut raw_cases {
            rows.append(&mut fallback_rows.clone());
        }

        let cases = raw_cases
            .into_iter()
            .map(|(cons, vars, rows)| Case::new(cons, vars, self.compile_rows(rows)))
            .collect();

        (cases, Box::new(self.compile_rows(fallback_rows)))
    }
}
