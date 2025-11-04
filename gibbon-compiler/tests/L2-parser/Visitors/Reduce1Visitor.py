from L2GrammarVisitor import L2GrammarVisitor
from L2GrammarParser import L2GrammarParser

class Reduce1Visitor(L2GrammarVisitor):
    def defaultResult(self):
        return []

    def aggregateResult(self, result:list, childResult):
        if type(childResult) is list:
            result.extend(childResult)
        else: result.append(childResult)
        return result
    
    # Visit a parse tree produced by L2GrammarParser#program.
    def visitProgram(self, ctx:L2GrammarParser.ProgramContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#datatypeDecl.
    def visitDatatypeDecl(self, ctx:L2GrammarParser.DatatypeDeclContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#funcDecl.
    def visitFuncDecl(self, ctx:L2GrammarParser.FuncDeclContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#locatedType.
    def visitLocatedType(self, ctx:L2GrammarParser.LocatedTypeContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#typeScheme.
    def visitTypeScheme(self, ctx:L2GrammarParser.TypeSchemeContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#val.
    def visitVal(self, ctx:L2GrammarParser.ValContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#letExpress.
    def visitLetExpress(self, ctx:L2GrammarParser.LetExpressContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#letLocExpress.
    def visitLetLocExpress(self, ctx:L2GrammarParser.LetLocExpressContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#letRegionExpress.
    def visitLetRegionExpress(self, ctx:L2GrammarParser.LetRegionExpressContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#caseExpress.
    def visitCaseExpress(self, ctx:L2GrammarParser.CaseExpressContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#funcAppExpress.
    def visitFuncAppExpress(self, ctx:L2GrammarParser.FuncAppExpressContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#dataConAppExpress.
    def visitDataConAppExpress(self, ctx:L2GrammarParser.DataConAppExpressContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#expr.
    def visitExpr(self, ctx:L2GrammarParser.ExprContext):
        # retVal = self.visitChildren(ctx)
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#logicalOrExpr.
    def visitLogicalOrExpr(self, ctx:L2GrammarParser.LogicalOrExprContext):
        retVal = self.visitChildren(ctx)
        if len(ctx.children) == 1:
            return retVal
        else: return ctx


    # Visit a parse tree produced by L2GrammarParser#logicalAndExpr.
    def visitLogicalAndExpr(self, ctx:L2GrammarParser.LogicalAndExprContext):
        retVal = self.visitChildren(ctx)
        if len(ctx.children) == 1:
            return retVal
        else: return ctx


    # Visit a parse tree produced by L2GrammarParser#equalityExpr.
    def visitEqualityExpr(self, ctx:L2GrammarParser.EqualityExprContext):
        retVal = self.visitChildren(ctx)
        if len(ctx.children) == 1:
            return retVal
        else: return ctx


    # Visit a parse tree produced by L2GrammarParser#relationalExpr.
    def visitRelationalExpr(self, ctx:L2GrammarParser.RelationalExprContext):
        retVal = self.visitChildren(ctx)
        if len(ctx.children) == 1:
            return retVal
        else: return ctx


    # Visit a parse tree produced by L2GrammarParser#addExpr.
    def visitAddExpr(self, ctx:L2GrammarParser.AddExprContext):
        retVal = self.visitChildren(ctx)
        if len(ctx.children) == 1:
            return retVal
        else: return ctx


    # Visit a parse tree produced by L2GrammarParser#mulExpr.
    def visitMulExpr(self, ctx:L2GrammarParser.MulExprContext):
        retVal = self.visitChildren(ctx)
        if len(ctx.children) == 1:
            return retVal
        else: return ctx


    # Visit a parse tree produced by L2GrammarParser#powExpr.
    def visitPowExpr(self, ctx:L2GrammarParser.PowExprContext):
        retVal = self.visitChildren(ctx)
        if len(ctx.children) == 1:
            return retVal
        else: return ctx


    # Visit a parse tree produced by L2GrammarParser#atom.
    def visitAtom(self, ctx:L2GrammarParser.AtomContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#baseType.
    def visitBaseType(self, ctx:L2GrammarParser.BaseTypeContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#lit.
    def visitLit(self, ctx:L2GrammarParser.LitContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#binaryOp.
    def visitBinaryOp(self, ctx:L2GrammarParser.BinaryOpContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#pat.
    def visitPat(self, ctx:L2GrammarParser.PatContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#locExpress.
    def visitLocExpress(self, ctx:L2GrammarParser.LocExpressContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#locRegion.
    def visitLocRegion(self, ctx:L2GrammarParser.LocRegionContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#concreteLoc.
    def visitConcreteLoc(self, ctx:L2GrammarParser.ConcreteLocContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#funcVar.
    def visitFuncVar(self, ctx:L2GrammarParser.FuncVarContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#regionVar.
    def visitRegionVar(self, ctx:L2GrammarParser.RegionVarContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#locVar.
    def visitLocVar(self, ctx:L2GrammarParser.LocVarContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#indexVar.
    def visitIndexVar(self, ctx:L2GrammarParser.IndexVarContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#typeCon.
    def visitTypeCon(self, ctx:L2GrammarParser.TypeConContext):
        ctx.children = self.visitChildren(ctx)
        return ctx


    # Visit a parse tree produced by L2GrammarParser#dataCon.
    def visitDataCon(self, ctx:L2GrammarParser.DataConContext):
        ctx.children = self.visitChildren(ctx)
        return ctx