from antlr4 import *
import subprocess

testFile = "gibbon-compiler/tests/L2-parser/tests/1-Add1.hs"

# regenerate grammar files
print("Regenerating grammar...")
try:
    result = subprocess.run(['antlr4', '-Dlanguage=Python3', './gibbon-compiler/tests/L2-parser/L2Grammar.g4', '-visitor'], capture_output=True, text=True)
    print("Succeeded with return code:", result.returncode)
except subprocess.CalledProcessError as e:
    print(f"Regenerate grammar failed with error: {e}")
    print(f"Stderr: {e.stderr}")
    exit()


from L2GrammarLexer import L2GrammarLexer
from L2GrammarParser import L2GrammarParser
import pprint
from L2GrammarVisitor import L2GrammarVisitor
from Visitors.Reduce1Visitor import Reduce1Visitor
from Visitors.PrintASTVisitor import PrintASTVisitor

# read file
with open(testFile, 'r') as f: 
    file_contents = f.read()

# setup ANTLR
input_stream = InputStream(file_contents)
lexer = L2GrammarLexer(input_stream)
stream = CommonTokenStream(lexer)
stream.fill()
print([token.text for token in stream.tokens])
parser = L2GrammarParser(stream)

tree = parser.program()
# pprint.pprint(tree.toStringTree(recog=parser))


reduceVisitor = Reduce1Visitor()
printVisitor = PrintASTVisitor()
reducedTree = reduceVisitor.visit(tree)
printVisitor.visit(reducedTree)





# printTree = 