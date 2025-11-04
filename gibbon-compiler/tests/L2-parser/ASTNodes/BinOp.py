from enum import Enum
from AST import ASTNode, ExprNode

class BinOp(Enum):
    pass

class OpLogic(BinOp):
    OR = '||'
    AND = '&&'

class OpRel(BinOp):
    IEQ = '=='       # integer equality
    FEQ = '.==.'     # float equality
    CEQ = '*==*'     # char equality
    NEQ = '/='       # not equal
    LT = '<'         
    LTE = '<='
    GT = '>'
    GTE = '>='
    FLT = '.<.'
    FGT = '.>.'
    FLTE = '.<=.'
    FGTE = '.>=.'

class OpAdd(BinOp):
    ADD = '+'
    SUB = '-'
    FADD = '.+.'
    FSUB = '.-.'

class OpMul(BinOp):
    MUL = '*'
    DIV = '/'
    FMUL = '.*.'
    FDIV = './.'
    MOD = 'mod'

class OpPow(BinOp):
    POW = '^'


class BinOpNode(ASTNode):
    def __init__(self, op:BinOp, left:ExprNode, right:ExprNode):
        self.op = op
        self.left = left
        self.right = right
    
    def __str__(self):
        return f"({str(self.left)} {self.op.value} {str(self.right)})"

class LogicalExprNode(BinOpNode):
    def __init__(self, op:OpLogic, left:ExprNode, right:ExprNode):
        super().__init__(op, left, right)

class RelationalExprNode(BinOpNode):
    def __init__(self, op:OpRel, left:ExprNode, right:ExprNode):
        super().__init__(op, left, right)

class AddExprNode(BinOpNode):
    def __init__(self, op:OpAdd, left:ExprNode, right:ExprNode):
        super().__init__(op, left, right)

class MulExprNode(BinOpNode):
    def __init__(self, op:OpMul, left:ExprNode, right:ExprNode):
        super().__init__(op, left, right)

class PowExprNode(BinOpNode):
    def __init__(self, op:OpPow, left:ExprNode, right:ExprNode):
        super().__init__(op, left, right)

