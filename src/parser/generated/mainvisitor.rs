#![allow(nonstandard_style)]
// Generated from main.g4 by ANTLR 4.8
use antlr_rust::tree::{ParseTreeVisitor};
use super::mainparser::*;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link mainParser}.
 */
pub trait mainVisitor<'input>: ParseTreeVisitor<'input,mainParserContextType>{
	/**
	 * Visit a parse tree produced by {@link mainParser#prog}.
	 * @param ctx the parse tree
	 */
	fn visit_prog(&mut self, ctx: &ProgContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link mainParser#expr}.
	 * @param ctx the parse tree
	 */
	fn visit_expr(&mut self, ctx: &ExprContext<'input>) { self.visit_children(ctx) }


}