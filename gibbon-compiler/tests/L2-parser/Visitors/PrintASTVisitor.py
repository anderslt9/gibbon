from L2GrammarVisitor import L2GrammarVisitor
from L2GrammarParser import L2GrammarParser


class PrintASTVisitor(L2GrammarVisitor):
    def __init__(self, indent_spaces=2):
        super().__init__()
        self.indent_level = 0
        self.indent_spaces = indent_spaces
    
    def __indentation(self):
        return " " * self.indent_spaces * self.indent_level

    # def visitProgram(self, ctx):
    #     print(f"{self.__indentation()}Program (")
    #     self.indent_level += 1
    #     return super().visitProgram(ctx)

    def visitChildren(self, node):
        if node.getChildCount() == 0:
            print(self.__indentation() + node.getText())
            return node.getText()
            
        else: print(self.__indentation() + node.__class__.__name__[:-7] + '(')
        self.indent_level += 1
        result = super().visitChildren(node)
        self.indent_level -= 1
        print(self.__indentation() + ')')

        return result

    def visitTerminal(self, node):
        print(self.__indentation() + node.getText())
        return super().visitTerminal(node)
    # def visitASTNode(self, node):
    #     print(self.__indentation() + node.__class__.__name__)
    #     num_children = node.getChildCount()
    #     if num_children == 0:
    #         print(" " * self.indent_spaces * self.indent_level + node.getText())
    #     else:
    #         children = []
    #         for i in range(num_children):
    #             child = node.getChild(i)
    #             child_result = child.accept(self)
    #             children.append(child_result)
    #         return (node.__class__.__name__, children)