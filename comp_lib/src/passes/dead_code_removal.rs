use std::collections::LinkedList;

use crate::{
    diagnostic::{Diagnostic, DiagnosticBuilder, Span},
    ir::{
        ctype::CType, BlockNode, ExprNode, IfStmtNode, LoopStmtNode, Root, Stmt, StmtNode,
        SwitchStmtNode,
    },
};

pub fn remove_dead_code(root: &mut Root) -> LinkedList<Diagnostic> {
    let mut diagnostics = LinkedList::new();
    for function in &mut root.functions.values_mut() {
        if let Some(body) = &mut function.body {
            let remover = FunctionCodeRemover {
                diagnostics: &mut diagnostics,
            };
            let res = remover.remove(body);
            if function.return_type != CType::Void
                && !matches!(
                    res,
                    StmtRes::Returns | StmtRes::Infinite(_) | StmtRes::RetOrInf
                )
            {
                let end = body.span.excl_end() - 1;
                diagnostics
                    .push_front(DiagnosticBuilder::new(end..end).build_no_return_in_value_func())
            }
        }
    }
    diagnostics
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StmtRes {
    /// The block will never diverge
    Runs,
    /// The block will always break
    Break,
    /// The block will always continue
    Continue,
    /// The block will always return
    Returns,
    /// The block will always get stuck in a infinate loop
    /// The span is the span of the loop
    Infinite(Span),
    /// The block could diverge
    Unknown,
    /// The block could return on get in a infinate lop
    RetOrInf,
}

enum IfKeep {
    All,
    IfBranch,
    ElseBranch,
}

enum LoopRes {
    Keep,
    Remove,
}

struct FunctionCodeRemover<'a> {
    diagnostics: &'a mut LinkedList<Diagnostic>,
}

impl FunctionCodeRemover<'_> {
    fn remove(mut self, block: &mut BlockNode) -> StmtRes {
        self.block(block)
    }

    fn stmt(&mut self, mut stmt: StmtNode) -> (StmtRes, Vec<StmtNode>) {
        let res = match &mut stmt.stmt {
            Stmt::Expr(_) => StmtRes::Runs,
            Stmt::IfStmt(i) => {
                let (stmt_res, if_res) = self.if_node(i);

                let new_stmts = match if_res {
                    IfKeep::All => vec![stmt],
                    IfKeep::IfBranch => std::mem::take(&mut i.if_branch.stmts),
                    IfKeep::ElseBranch => match &mut i.else_branch {
                        Some(else_branch) => std::mem::take(&mut else_branch.stmts),
                        None => Vec::new(),
                    },
                };

                return (stmt_res, new_stmts);
            }
            Stmt::SwitchStmt(i) => self.switch_node(i),
            Stmt::LoopStmt(i) => {
                let (stmt_res, loop_res) = self.loop_node(i);

                let new_stmts = match loop_res {
                    LoopRes::Keep => vec![stmt],
                    LoopRes::Remove => Vec::new(),
                };

                return (stmt_res, new_stmts);
            }
            Stmt::Break => StmtRes::Break,
            Stmt::Continue => StmtRes::Continue,
            Stmt::Return(_) => StmtRes::Returns,
        };

        (res, vec![stmt])
    }

    fn if_node(&mut self, stmt: &mut IfStmtNode) -> (StmtRes, IfKeep) {
        match extract_const(&stmt.condition) {
            Const::Truthy => {
                self.diagnostics.push_front(
                    DiagnosticBuilder::new(stmt.condition.span)
                        .build_always_true_false(true, stmt.else_branch.as_ref().map(|b| b.span)),
                );
                return (self.block(&mut stmt.if_branch), IfKeep::IfBranch);
            }
            Const::Falsy => {
                self.diagnostics.push_front(
                    DiagnosticBuilder::new(stmt.condition.span)
                        .build_always_true_false(false, Some(stmt.if_branch.span)),
                );

                return (
                    stmt.else_branch
                        .as_mut()
                        .map(|stmt| self.block(stmt))
                        .unwrap_or(StmtRes::Runs),
                    IfKeep::ElseBranch,
                );
            }
            Const::NotFolded => {}
        }

        let if_res = self.block(&mut stmt.if_branch);
        let else_res = match &mut stmt.else_branch {
            Some(else_branch) => self.block(else_branch),
            None => StmtRes::Runs,
        };

        if if_res == else_res {
            return (if_res, IfKeep::All);
        }
        let res = match (if_res, else_res) {
            (
                StmtRes::Returns | StmtRes::Infinite(_) | StmtRes::RetOrInf,
                StmtRes::Returns | StmtRes::Infinite(_) | StmtRes::RetOrInf,
            ) => StmtRes::RetOrInf,
            _ => StmtRes::Unknown,
        };
        (res, IfKeep::All)
    }

    fn switch_node(&mut self, stmt: &mut SwitchStmtNode) -> StmtRes {
        let mut common = None;

        let mut add_stmt_res = |res| match common {
            Some(ref mut common) => {
                if res != *common {
                    *common = match (res, &common) {
                        (
                            StmtRes::Returns | StmtRes::Infinite(_) | StmtRes::RetOrInf,
                            StmtRes::Returns | StmtRes::Infinite(_) | StmtRes::RetOrInf,
                        ) => StmtRes::RetOrInf,
                        _ => StmtRes::Unknown,
                    };
                }
            }
            None => {
                common = Some(res);
            }
        };

        let len = stmt.cases.len();
        for (i, case) in stmt.cases.iter_mut().enumerate() {
            let body = match &mut case.data {
                crate::ir::SwitchStmtCase::Case { body, .. } => body,
                crate::ir::SwitchStmtCase::Default { body } => body,
            };

            let res = self.block(body);
            if res == StmtRes::Runs && i + 1 != len {
                // This case falls through
                continue;
            }

            add_stmt_res(res);
        }

        if !stmt.has_default {
            // There is no default so asume we can alwys ge to the botom
            add_stmt_res(StmtRes::Runs);
        }

        match common {
            Some(res) => match res {
                StmtRes::Runs => StmtRes::Runs,
                StmtRes::Unknown => StmtRes::Unknown,
                StmtRes::Break => StmtRes::Runs,
                StmtRes::Continue => StmtRes::Continue,
                StmtRes::Returns => StmtRes::Returns,
                StmtRes::Infinite(span) => StmtRes::Infinite(span),
                StmtRes::RetOrInf => StmtRes::RetOrInf,
            },
            None => StmtRes::Runs,
        }
    }

    fn loop_node(&mut self, stmt: &mut LoopStmtNode) -> (StmtRes, LoopRes) {
        let condition = stmt.condition.as_ref().map(extract_const);

        let res = match condition {
            None | Some(Const::Truthy) => match self.block(&mut stmt.body) {
                StmtRes::Runs => StmtRes::Infinite(stmt.span),
                StmtRes::Unknown => StmtRes::Unknown,
                StmtRes::Break => StmtRes::Runs,
                StmtRes::Continue => StmtRes::Infinite(stmt.span),
                StmtRes::Returns => StmtRes::Returns,
                StmtRes::Infinite(span) => StmtRes::Infinite(span),
                StmtRes::RetOrInf => StmtRes::RetOrInf,
            },
            Some(Const::Falsy) => {
                self.diagnostics.push_front(
                    DiagnosticBuilder::new(
                        stmt.condition
                            .as_ref()
                            .expect("ICE: extracted falsy from nothing")
                            .span,
                    )
                    .build_always_true_false(false, Some(stmt.body.span)),
                );
                return (StmtRes::Runs, LoopRes::Remove);
            }
            Some(Const::NotFolded) => match self.block(&mut stmt.body) {
                StmtRes::Runs => StmtRes::Runs,
                StmtRes::Unknown
                | StmtRes::Break
                | StmtRes::Continue
                | StmtRes::Returns
                | StmtRes::Infinite(_)
                | StmtRes::RetOrInf => StmtRes::Unknown,
            },
        };
        (res, LoopRes::Keep)
    }

    fn block(&mut self, block: &mut BlockNode) -> StmtRes {
        let mut has_unknown = false;

        let stmts = std::mem::take(&mut block.stmts);
        let mut iter = stmts.into_iter();

        while let Some(stmt) = iter.next() {
            let diverges_span = stmt.span;
            let (res, new_stmts) = self.stmt(stmt);
            block.stmts.extend_from_slice(&new_stmts);

            match res {
                StmtRes::Runs => {}
                StmtRes::Unknown => {
                    has_unknown = true;
                }
                diverges => {
                    if let Some(next) = iter.next() {
                        let from = next.span.start();
                        let last_index = iter.last().map(|s| s.span.excl_end()).unwrap_or(from);

                        let infinite = match diverges {
                            StmtRes::Runs
                            | StmtRes::Unknown
                            | StmtRes::RetOrInf
                            | StmtRes::Break
                            | StmtRes::Continue
                            | StmtRes::Returns => None,
                            StmtRes::Infinite(span) => Some(span),
                        };

                        self.diagnostics.push_back(
                            DiagnosticBuilder::new(diverges_span)
                                .build_unreachable_code(Span::from(from..last_index), infinite),
                        );
                    }
                    return diverges;
                }
            }
        }

        match has_unknown {
            true => StmtRes::Unknown,
            false => StmtRes::Runs,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Const {
    NotFolded,
    Truthy,
    Falsy,
}

fn extract_const(node: &ExprNode) -> Const {
    match &node.expr {
        crate::ir::Expr::Constant(c) => match c {
            crate::ir::Constant::Integer(i) => match i {
                0 => Const::Falsy,
                _ => Const::Truthy,
            },
            crate::ir::Constant::Float(f) => {
                if *f == 0.0 {
                    Const::Falsy
                } else {
                    Const::Truthy
                }
            }
            crate::ir::Constant::String(_) => {
                // A pointer to the string will never be a null pointer so is always truthy.
                Const::Truthy
            }
        },
        _ => Const::NotFolded,
    }
}
