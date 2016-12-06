class Node:
    def __init__(self):
        print("Node")
    def evaluate(self):
        print("Evaluate")
        return 0
    def execute(self):
        print("Execute")
   
class NumberNode(Node):
    def __init__(self, v):
        self.value = int(v)
        print("NumberNode")
    def evaluate(self):
        print("Evaluate NumberNode")
        return self.value
    def execute(self):
        print("Execute NumberNode")
 
class StringNode(Node):
    def __init__(self, v):
        self.value = str(v)
        self.value = self.value[1:-1] # to eliminate the left and right double quotes
        print("StringNode")
    def evaluate(self):
        print("Evaluate StringNode")
        return self.value
    def execute(self):
        print("Execute StringNode")
 
class PrintNode(Node):
    def __init__(self, v):
        self.value = v
        print("PrintNode")
    def evaluate(self):
        print("Evaluate PrintNode")
        return 0
    def execute(self):
        print("Execute NumberNode")
        print(self.value.evaluate())
 
class IfNode(Node):
	def __init__(self, c, t, e):
		self.condition = c
		self.thenBlock = t
		self.elseBlock= e
		print("IfNode")
	def evaluate(self):
		print("Evaluate IfNode")
		return 0
	def execute(self):
		print("Execute IfNode")
		if(self.condition.evaluate()):
			self.thenBlock.execute()
		else:
			self.elseBlock.execute()
class BlockNode(Node):
    def __init__(self, sl):
        self.statementNodes = sl
        print("BlockNode")
    def evaluate(self):
        print("Evaluate BlockNode")
        return 0
    def execute(self):
        print("Execute BlockNode")
        for statement in self.statementNodes:
            statement.execute()
 
# sample parse rules:
def p_statement_print(p):
    ''' statement : PRINT LBRACE expression RBRACE SEMI '''
    p[0] = PrintNode(p[3]) # create nodes in the tree instead of executing the current expression
 
def p_statement_block(p):
    ''' statement : LCBRACE block RCBRACE '''
    p[0] = BlockNode(p[2])
 
# execute the abstract syntax tree for the whole program that you read from the file
ast = yacc.parse(allFileCode)
ast.execute();