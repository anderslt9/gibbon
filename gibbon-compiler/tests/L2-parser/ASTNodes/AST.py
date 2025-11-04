class ASTNode:
    pass

class ProgramNode(ASTNode):
    def __init__(self, datatype_decls:dict, func_decls, expr):
        self.datatype_decls = datatype_decls
        self.func_decls = func_decls
        self.expr = expr

"""
Example: 
data Maybe a = Nothing | Just a
-------------------------------
self.typeCon = 'Maybe'
self.dataTypePairs = [('Nothing', []), ('Just', ['a'])]
"""
class DatatypeDeclNode(ASTNode):
    def __init__(self, typeCon, dataTypePairs):
        self.typeCon = typeCon
        self.dataTypePairs = dataTypePairs

"""
Example:
foo : a -> Tree@(l,r)
foo = ...
-------------------------------
self.name = 'foo'
self.type_scheme = [TypeSchemeNode(...(Tree, (l,r)))]
self.locRegions = [('l', 'r')]
self.expr = ExprNode(...)"""

class FuncDeclNode(ASTNode):
    def __init__(self, name, type_scheme, locRegions, expr):
        self.name = name    # function name
        self.type_scheme = type_scheme  
        self.locRegions = locRegions
        self.expr = expr

"""
Example:
Tree@(l,r)
-------------------------------
self.typeCon = 'Tree'
self.locRegion = ('l','r')"""
class LocatedTypeNode(ASTNode):
    def __init__(self, typeCon, locRegion):
        self.typeCon = typeCon
        self.locRegion = locRegion

"""
Example:
a -> Tree@(l,r),
-------------------------------
self.types = [BaseTypeNode('a'), LocatedTypeNode('Tree', ('l','r'))]"""
class TypeSchemeNode(ASTNode):
    def __init__(self, types):
        self.types = types

class ValNode(ASTNode):
    def __init__(self, val):
        self.val = val    

"""
Example:
let x : Tree@(l,r,a) = buildTree [(l,r,a)] (n - 1)
    in ...
-------------------------------
self.boundVar = 'x'
self.locType = LocatedTypeNode('Tree', ('l','r','a'))
self.preExpr = FuncAppExpressNode(...)
self.postExpr = ExprNode(...)"""
class LetExpressNode(ASTNode):
    def __init__(self, boundVar, locType, preExpr, postExpr):
        self.boundVar = boundVar
        self.locType = locType
        self.preExpr = preExpr
        self.postExpr = postExpr

"""
Example:
letloc (l,r,a) = (l,r) + 1 in ...
-------------------------------
self.locRegion = ('l','r','a')
self.locExpr = LocExprNode(...)
self.postExpr = ExprNode(...)"""
class LetLocExpressNode(ASTNode):
    def __init__(self, locRegion, locExpr, postExpr):
        self.locRegion = locRegion
        self.locExpr = locExpr
        self.postExpr = postExpr

"""
Example:
letregion r in ...
-------------------------------
self.regionVar = 'r'
self.postExpr = ExprNode(...)"""
class LetRegionExpressNode(ASTNode):    
    def __init__(self, regionVar, postExpr):
        self.regionVar = regionVar
        self.postExpr = postExpr

"""
Example:
case matchedExpr of
    pat1 -> expr1
    pat2 -> expr2
-------------------------------
self.matchedExpr = ExprNode(...)
self.pats = [(PatNode(...), ExprNode(...)), (PatNode(...), ExprNode(...))]"""
class CaseExpressNode(ASTNode):
    def __init__(self, matchedExpr, pats):
        self.matchedExpr = matchedExpr
        self.pats = pats                # list of (PatNode, ExprNode) pairs

"""
Example:
AddTree [(l,r)] t1 4
-------------------------------
self.funcVar = 'AddTree'
self.locRegions = [('l','r')]
self.vals = [ValNode(...), ValNode(...)]"""
class FuncAppExpressNode(ASTNode):
    def __init__(self, funcVar, locRegions, vals):
        self.funcVar = funcVar
        self.locRegions = locRegions
        self.vals = vals

"""
Example:
Tree (l,r) leftSubtree rightSubtree
-------------------------------
self.typeCon = 'Tree'
self.locRegion = ('l','r')
self.vals = [ValNode(...), ValNode(...)]"""
class DataConAppExpressNode(ASTNode):
    def __init__(self, dataCon, locRegion, vals):
        self.dataCon = dataCon
        self.locRegion = locRegion
        self.vals = vals

class ExprNode(ASTNode):
    def __init__(self, value):
        self.value = value



# class AtomNode(ASTNode):
#     def __init__(self, value):
#         self.value = value



