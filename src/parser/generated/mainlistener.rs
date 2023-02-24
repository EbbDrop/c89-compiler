#![allow(nonstandard_style)]
// Generated from main.g4 by ANTLR 4.8
use antlr_rust::tree::ParseTreeListener;
use super::mainparser::*;

pub trait mainListener<'input> : ParseTreeListener<'input,mainParserContextType>{

/**
 * Enter a parse tree produced by {@link mainParser#prog}.
 * @param ctx the parse tree
 */
fn enter_prog(&mut self, _ctx: &ProgContext<'input>) { }
/**
 * Exit a parse tree produced by {@link mainParser#prog}.
 * @param ctx the parse tree
 */
fn exit_prog(&mut self, _ctx: &ProgContext<'input>) { }

/**
 * Enter a parse tree produced by {@link mainParser#expr}.
 * @param ctx the parse tree
 */
fn enter_expr(&mut self, _ctx: &ExprContext<'input>) { }
/**
 * Exit a parse tree produced by {@link mainParser#expr}.
 * @param ctx the parse tree
 */
fn exit_expr(&mut self, _ctx: &ExprContext<'input>) { }

}
