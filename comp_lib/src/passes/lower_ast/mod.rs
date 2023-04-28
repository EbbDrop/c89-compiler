use crate::{
    ast,
    diagnostic::{AggregateResult, DiagnosticBuilder},
    ir::{ctype::CType, stmt::Root},
};

use symbol_table::ScopedTable;

use self::util::Scope;

mod expr;
mod stmt;
mod symbol_table;
mod type_checking;
mod util;

pub fn build_ir_from_ast(ast: &ast::Ast) -> AggregateResult<Root> {
    let mut res: AggregateResult<Option<&ast::FunctionDefinition>> = AggregateResult::new_ok(None);

    for g in &ast.global_declarations {
        match &g.data {
            ast::ExternalDeclaration::FunctionDefinition(f) => {
                if f.ident.data == "main" {
                    if let Some(other_main) = res.value() {
                        if let Some(other_main) = other_main {
                            res.add_err(
                                DiagnosticBuilder::new(f.ident.span)
                                    .build_already_defined("main", other_main.ident.span),
                            )
                        } else if let Some(v) = res.value_mut() {
                            *v = Some(f);
                        }
                    }
                } else {
                    res.add_rec_diagnostic(
                        DiagnosticBuilder::new(g.span).build_unimplemented("non `main` functions"),
                    );
                }
            }
            ast::ExternalDeclaration::Declaration(ast::Declaration::Variable(_)) => {
                res.add_rec_diagnostic(
                    DiagnosticBuilder::new(g.span).build_unimplemented("global declarations"),
                );
            }
            ast::ExternalDeclaration::Declaration(ast::Declaration::FunctionDeclaration(_)) => {
                res.add_rec_diagnostic(
                    DiagnosticBuilder::new(g.span).build_unimplemented("forward declarations"),
                );
            }
        }
    }

    if let Some(include) = ast.include_stdio {
        res.add_rec_diagnostic(DiagnosticBuilder::new(include).build_unimplemented("include"));
    }

    res.and_then(|main| {
        if let Some(main) = main {
            build_ir_from_function_block(
                &main.body,
                CType::from_ast_type(&main.return_type.data.inner.data),
            )
        } else {
            AggregateResult::new_err(
                DiagnosticBuilder::new(0..0).build_unimplemented("library projects"),
            )
        }
    })
}

pub fn build_ir_from_function_block(
    block: &ast::BlockStatementNode,
    return_type: CType,
) -> AggregateResult<Root> {
    let mut function_table = ScopedTable::default();

    let res = {
        let mut scope = Scope {
            vars: function_table.get_scoped_handle(),
            func_return_type: &return_type,
            in_switch: false,
            in_loop: false,
        };

        stmt::build_ir_from_block(block, &mut scope)
    };

    res.map(|block| Root {
        global: block,
        table: function_table.into_table(),
    })
}

#[cfg(test)]
mod tests;
