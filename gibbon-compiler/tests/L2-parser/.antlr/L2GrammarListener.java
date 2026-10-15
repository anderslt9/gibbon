// Generated from /home/anderslt/gibbon-compiler/gibbon-compiler/tests/L2-parser/L2Grammar.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link L2GrammarParser}.
 */
public interface L2GrammarListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(L2GrammarParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(L2GrammarParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#datatypeDecl}.
	 * @param ctx the parse tree
	 */
	void enterDatatypeDecl(L2GrammarParser.DatatypeDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#datatypeDecl}.
	 * @param ctx the parse tree
	 */
	void exitDatatypeDecl(L2GrammarParser.DatatypeDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#funcDecl}.
	 * @param ctx the parse tree
	 */
	void enterFuncDecl(L2GrammarParser.FuncDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#funcDecl}.
	 * @param ctx the parse tree
	 */
	void exitFuncDecl(L2GrammarParser.FuncDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#locatedType}.
	 * @param ctx the parse tree
	 */
	void enterLocatedType(L2GrammarParser.LocatedTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#locatedType}.
	 * @param ctx the parse tree
	 */
	void exitLocatedType(L2GrammarParser.LocatedTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#typeScheme}.
	 * @param ctx the parse tree
	 */
	void enterTypeScheme(L2GrammarParser.TypeSchemeContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#typeScheme}.
	 * @param ctx the parse tree
	 */
	void exitTypeScheme(L2GrammarParser.TypeSchemeContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#val}.
	 * @param ctx the parse tree
	 */
	void enterVal(L2GrammarParser.ValContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#val}.
	 * @param ctx the parse tree
	 */
	void exitVal(L2GrammarParser.ValContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#letExpress}.
	 * @param ctx the parse tree
	 */
	void enterLetExpress(L2GrammarParser.LetExpressContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#letExpress}.
	 * @param ctx the parse tree
	 */
	void exitLetExpress(L2GrammarParser.LetExpressContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#letLocExpress}.
	 * @param ctx the parse tree
	 */
	void enterLetLocExpress(L2GrammarParser.LetLocExpressContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#letLocExpress}.
	 * @param ctx the parse tree
	 */
	void exitLetLocExpress(L2GrammarParser.LetLocExpressContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#letRegionExpress}.
	 * @param ctx the parse tree
	 */
	void enterLetRegionExpress(L2GrammarParser.LetRegionExpressContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#letRegionExpress}.
	 * @param ctx the parse tree
	 */
	void exitLetRegionExpress(L2GrammarParser.LetRegionExpressContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#caseExpress}.
	 * @param ctx the parse tree
	 */
	void enterCaseExpress(L2GrammarParser.CaseExpressContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#caseExpress}.
	 * @param ctx the parse tree
	 */
	void exitCaseExpress(L2GrammarParser.CaseExpressContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#funcAppExpress}.
	 * @param ctx the parse tree
	 */
	void enterFuncAppExpress(L2GrammarParser.FuncAppExpressContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#funcAppExpress}.
	 * @param ctx the parse tree
	 */
	void exitFuncAppExpress(L2GrammarParser.FuncAppExpressContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#dataConAppExpress}.
	 * @param ctx the parse tree
	 */
	void enterDataConAppExpress(L2GrammarParser.DataConAppExpressContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#dataConAppExpress}.
	 * @param ctx the parse tree
	 */
	void exitDataConAppExpress(L2GrammarParser.DataConAppExpressContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(L2GrammarParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(L2GrammarParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#logicalOrExpr}.
	 * @param ctx the parse tree
	 */
	void enterLogicalOrExpr(L2GrammarParser.LogicalOrExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#logicalOrExpr}.
	 * @param ctx the parse tree
	 */
	void exitLogicalOrExpr(L2GrammarParser.LogicalOrExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#logicalAndExpr}.
	 * @param ctx the parse tree
	 */
	void enterLogicalAndExpr(L2GrammarParser.LogicalAndExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#logicalAndExpr}.
	 * @param ctx the parse tree
	 */
	void exitLogicalAndExpr(L2GrammarParser.LogicalAndExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#equalityExpr}.
	 * @param ctx the parse tree
	 */
	void enterEqualityExpr(L2GrammarParser.EqualityExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#equalityExpr}.
	 * @param ctx the parse tree
	 */
	void exitEqualityExpr(L2GrammarParser.EqualityExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#relationalExpr}.
	 * @param ctx the parse tree
	 */
	void enterRelationalExpr(L2GrammarParser.RelationalExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#relationalExpr}.
	 * @param ctx the parse tree
	 */
	void exitRelationalExpr(L2GrammarParser.RelationalExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#addExpr}.
	 * @param ctx the parse tree
	 */
	void enterAddExpr(L2GrammarParser.AddExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#addExpr}.
	 * @param ctx the parse tree
	 */
	void exitAddExpr(L2GrammarParser.AddExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#mulExpr}.
	 * @param ctx the parse tree
	 */
	void enterMulExpr(L2GrammarParser.MulExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#mulExpr}.
	 * @param ctx the parse tree
	 */
	void exitMulExpr(L2GrammarParser.MulExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#powExpr}.
	 * @param ctx the parse tree
	 */
	void enterPowExpr(L2GrammarParser.PowExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#powExpr}.
	 * @param ctx the parse tree
	 */
	void exitPowExpr(L2GrammarParser.PowExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#atom}.
	 * @param ctx the parse tree
	 */
	void enterAtom(L2GrammarParser.AtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#atom}.
	 * @param ctx the parse tree
	 */
	void exitAtom(L2GrammarParser.AtomContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#baseType}.
	 * @param ctx the parse tree
	 */
	void enterBaseType(L2GrammarParser.BaseTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#baseType}.
	 * @param ctx the parse tree
	 */
	void exitBaseType(L2GrammarParser.BaseTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#lit}.
	 * @param ctx the parse tree
	 */
	void enterLit(L2GrammarParser.LitContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#lit}.
	 * @param ctx the parse tree
	 */
	void exitLit(L2GrammarParser.LitContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#binaryOp}.
	 * @param ctx the parse tree
	 */
	void enterBinaryOp(L2GrammarParser.BinaryOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#binaryOp}.
	 * @param ctx the parse tree
	 */
	void exitBinaryOp(L2GrammarParser.BinaryOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#pat}.
	 * @param ctx the parse tree
	 */
	void enterPat(L2GrammarParser.PatContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#pat}.
	 * @param ctx the parse tree
	 */
	void exitPat(L2GrammarParser.PatContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#locExpress}.
	 * @param ctx the parse tree
	 */
	void enterLocExpress(L2GrammarParser.LocExpressContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#locExpress}.
	 * @param ctx the parse tree
	 */
	void exitLocExpress(L2GrammarParser.LocExpressContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#locRegion}.
	 * @param ctx the parse tree
	 */
	void enterLocRegion(L2GrammarParser.LocRegionContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#locRegion}.
	 * @param ctx the parse tree
	 */
	void exitLocRegion(L2GrammarParser.LocRegionContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#concreteLoc}.
	 * @param ctx the parse tree
	 */
	void enterConcreteLoc(L2GrammarParser.ConcreteLocContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#concreteLoc}.
	 * @param ctx the parse tree
	 */
	void exitConcreteLoc(L2GrammarParser.ConcreteLocContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#funcVar}.
	 * @param ctx the parse tree
	 */
	void enterFuncVar(L2GrammarParser.FuncVarContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#funcVar}.
	 * @param ctx the parse tree
	 */
	void exitFuncVar(L2GrammarParser.FuncVarContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#regionVar}.
	 * @param ctx the parse tree
	 */
	void enterRegionVar(L2GrammarParser.RegionVarContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#regionVar}.
	 * @param ctx the parse tree
	 */
	void exitRegionVar(L2GrammarParser.RegionVarContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#locVar}.
	 * @param ctx the parse tree
	 */
	void enterLocVar(L2GrammarParser.LocVarContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#locVar}.
	 * @param ctx the parse tree
	 */
	void exitLocVar(L2GrammarParser.LocVarContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#indexVar}.
	 * @param ctx the parse tree
	 */
	void enterIndexVar(L2GrammarParser.IndexVarContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#indexVar}.
	 * @param ctx the parse tree
	 */
	void exitIndexVar(L2GrammarParser.IndexVarContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#typeCon}.
	 * @param ctx the parse tree
	 */
	void enterTypeCon(L2GrammarParser.TypeConContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#typeCon}.
	 * @param ctx the parse tree
	 */
	void exitTypeCon(L2GrammarParser.TypeConContext ctx);
	/**
	 * Enter a parse tree produced by {@link L2GrammarParser#dataCon}.
	 * @param ctx the parse tree
	 */
	void enterDataCon(L2GrammarParser.DataConContext ctx);
	/**
	 * Exit a parse tree produced by {@link L2GrammarParser#dataCon}.
	 * @param ctx the parse tree
	 */
	void exitDataCon(L2GrammarParser.DataConContext ctx);
}