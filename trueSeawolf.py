# Robert Trujillo 110668776

import ply.lex as lex
import ply.yacc as yacc
import sys

# @author - ROBERT TRUJILLO
# CSE 307 HW 4
global variables
variables = {}

class Node:
	def __init__(self):
		self.value = 0
	def evaluate(self):
		#print("Evaluate")
		return 0
	def execute(self):
		self.value=0
	def type(self):
		return "Node"
	
class IntNode(Node):
	def __init__(self, v):
		self.value = int(v)
		#print("IntNode")
	def evaluate(self):
		#print("Evaluate IntNode")
		return self.value
	def execute(self):
		return self.evaluate()
	def type(self):
		return "IntNode"

class RealNode(Node):
	def __init__(self, v):
		self.value = float(v)
		#print("RealNode")
	def evaluate(self):
		#print("Evaluate RealNode")
		return self.value
	def execute(self):
		return self.evaluate()
	def type(self):
		return "RealNode"

class StringNode(Node):
	def __init__(self, v):
		self.value = str(v)
		self.value = self.value[1:-1] # to eliminate the left and right double quotes
		#print("StringNode")
	def evaluate(self):
		#print("Evaluate StringNode")
		return self.value
	def execute(self):
		return self.evaluate()
	def type(self):
		return "StringNode"

class PrintNode(Node):
	def __init__(self, v):
		self.value = v
		#print("PrintNode")
	def evaluate(self):
		#print("Evaluate PrintNode")
		return 0
	def execute(self):
		#print("Execute NumberNode")
		print(self.value.evaluate())
	def type(self):
		return "PrintNode"

class IfNode(Node):
	def __init__(self, c, t, e):
		self.condition = c
		self.thenBlock = t
		self.elseBlock= e
	def evaluate(self):
		if self.condition.evaluate():
			return self.thenBlock.execute()
		else:
			return self.elseBlock.execute()
	def execute(self):
		self.evaluate()
	def type(self):
		return "IfNode"

class BlockNode(Node):
	def __init__(self, sl):
		self.statementNodes = sl
		#print("BlockNode")
	def evaluate(self):
		#print("Evaluate BlockNode")
		return 0
	def execute(self):
		#print("Execute BlockNode")
		for statement in self.statementNodes:
			statement.execute()
	def type(self):
		return "BlockNode"

class IndexNode(Node):
	def __init__(self, list, index):
		self.list = list
		self.index = index
	def evaluate(self):
		if self.list.type() == "ListNode":
			list = self.list.evaluate()
		elif self.list.type() == "StringNode":
			list = self.list.evaluate()
		else:
			list = self.list.evaluate()
			index = self.index.evaluate()
		return list[index]
	def execute(self):
		#print("Execute NumberNode")
		return self.evaluate()
	def type(self):
		return "IndexNode"

class ListNode(Node):
	def __init__(self, list):
		self.list = list
	def evaluate(self):
		i = 0
		for element in self.list:
			self.list[i] = element.evaluate()
			i += 1
		return self.list
	def execute(self):
		return self.evaluate()
	def type(self):
		return "ListNode"

class WhileNode(Node):
	def __init__(self, condition, block):
		self.condition = condition
		self.block = block
	def evaluate(self):
		while(self.condition.evaluate()):
			self.block.execute()
	def execute(self):
		return self.evaluate()
	def type(self):
		return "WhileNode"
	
class VariableNode(Node):
	def __init__(self, name, val):
		self.name = name
		self.val = val
	def evaluate(self):
		return self.val.evaluate()
	def execute(self):
		variables[self.name] = self.evaluate()
	def type(self):
		return "VariableNode"
		
class NameNode(Node):
	def __init__(self, var):
		self.var = var
	def evaluate(self):
		if self.var in variables:
			return variables[self.var]
		else:
			print("SEMANTIC ERROR")
	def execute(self):
		return self.evaluate()
	def type(self):
		return "NameNode"

class VarIndexNode(Node):
	def __init__(self, var, index, val):
		self.var = var
		self.index = index
		self.val = val
	def evaluate(self):
		#print("here")
		var = self.var.evaluate()
		index = self.index.evaluate()
		val = self.val.evaluate()
		if checkList(var):
			variabes[self.var] = var[0:index] + [val] + var[index:len(var)]
		else:
			variabes[self.var] = var[0:index] + val + var[index:len(var)]
	def execute(self):
		return self.evaluate()
	def type(self):
		return "VarIndexNode"
		
class OpNode(Node):
	def __init__(self, op, var1, var2):
		self.op = op
		self.var1 = var1
		self.var2 = var2
	def evaluate(self):
		#store the values in temp variables so that if in loop you can reevaluate them
		val1 = self.var1.evaluate()
		val2 = self.var2.evaluate()
		try:
			if self.op == '+':
				return val1 + val2
			elif self.op == '-':
				return val1 - val2
			elif self.op == '*':
				return val1 * val2
			elif self.op == '/':
				return val1 / val2
			elif self.op == '%':
				return val1 % val2
			elif self.op == '**':
				return val1 ** val2
			elif self.op == '//':
				return val1 // val2
			elif not (checkInt(val1) and checkInt(val2)):
				raise Exception("Semantic Error")
			elif self.op == '<' or self.op == '<=' or self.op == '>' or self.op == '>=' or self.op == '==' or self.op == '<>':
				return compare(self.op, val1, val2)
			elif self.op == 'and' or self.op == 'or':
				return compareBool(self.op, val1, val2)
			elif self.op == 'not':
				bool = not val1
				if(bool == True):
					return 1
				return 0
		except:
			print("SEMANTIC ERROR")
	def execute(self):
		return self.evaluate()
	def type(self):
		return "OpNode"
			

keywords = {
	'and'   : 'AND',
	'or'    : 'OR',
	'in'    : 'IN',
	'not'   : 'NOT',
	'print' : 'PRINT',
	'if'    : 'IF',
	'else'  : 'ELSE',
	'while' : 'WHILE'
}

operators = [
	"PLUS",
	"MINUS",
	"MULT",
	"DIVIDE",
	"MOD",
	"FLOOR_DIV",
	"EXPONENT",
	"LESS_THAN",
	"GREATER_THAN",
	"LESS_THAN_EQUAL",
	"GREATER_THAN_EQUAL",
	"EQUALS",
	"EQUALS_EQUALS",
	"NOT_EQUAL",
	"IN",
	"AND",
	"OR",
	"NOT"
]

special_tokens = operators + [
	"IF",
	"ELSE",
	"PRINT",
	"WHILE"
]

tokens = special_tokens + [
	"REAL",
	"INT",
	"STRING",
	"COMMA",
	"OPEN_BRACKET",
	"CLOSE_BRACKET",
	"OPEN_PARENTHESIS",
	"CLOSE_PARENTHESIS",
	"OPEN_CURLY",
	"CLOSE_CURLY",
	"SEMI",
	"NAME"
]


t_COMMA = r'\,'
t_OPEN_BRACKET = r'\['
t_CLOSE_BRACKET = r'\]'
t_OPEN_PARENTHESIS = r'\('
t_CLOSE_PARENTHESIS = r'\)'
t_OPEN_CURLY = r'\{'
t_CLOSE_CURLY = r'\}'
t_SEMI = r'\;'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULT = r'\*'
t_DIVIDE = r'\/'
t_MOD = r'\%'
t_FLOOR_DIV = r'\/\/'
t_EXPONENT = r'\*\*'
t_LESS_THAN = r'\<'
t_GREATER_THAN = r'\>'
t_LESS_THAN_EQUAL = r'\<='
t_GREATER_THAN_EQUAL = r'\>='
t_EQUALS = r'\='
t_EQUALS_EQUALS = r'\=\='
t_NOT_EQUAL = r'\<\>'


t_ignore = '\t\n '

def t_ID(t):
	r'[a-zA-Z_][a-zA-Z0-9_]*'
	if t.value in keywords:
		t.type = keywords.get(t.value, 'ID')
		return t
	else:
		return t_NAME(t)

def t_REAL(t):
	r'\d+\.\d+'
	t.value = float(t.value)
	return t
	
def t_INT(t):
	r'\d+'
	t.value = int(t.value)
	return t

def t_STRING(t):
	r'\".*?\"'
	t.type = 'STRING'
	t.value = t.value
	return t

def t_NAME(t):
	r'[a-zA-Z_][a-zA-Z0-9_]*'
	t.type = 'NAME'
	return t
	
def t_error(t):
	if t.value != '\n':
		print("Illegal characters!")
	t.lexer.skip(1)
	
lexer = lex.lex()

precedence = (
	('left', 'EQUALS'),
	('left', 'OR'),
	('left', 'AND'),
	('left', 'NOT'),
	('left', 'LESS_THAN', 'LESS_THAN_EQUAL', 'EQUALS_EQUALS', 'NOT_EQUAL', 'GREATER_THAN', 'GREATER_THAN_EQUAL'),
	('left', 'IN'),
	('left', 'PLUS', 'MINUS'),
	('left', 'FLOOR_DIV'),
	('left', 'EXPONENT'),
	('left', 'MOD'),
	('left', 'MULT', 'DIVIDE'),
	('left', 'OPEN_BRACKET', 'CLOSE_BRACKET'),
	('left', 'OPEN_PARENTHESIS', 'CLOSE_PARENTHESIS')

)

def p_statement_block(p):
	''' block : OPEN_CURLY statement_list CLOSE_CURLY '''
	#print('here')
	p[0] = BlockNode(p[2])

def p_statement_list(p):
	''' statement_list :  statement SEMI statement_tail'''
	#print('here')
	p[0] = [p[1]] + p[3]

def p_statement_tail(p):
	''' statement_tail :  statement SEMI statement_tail'''
	#print('here')
	p[0] = [p[1]] + p[3]
	
def p_special_list(p):
	'''
	statement_list : special statement_tail
	'''
	p[0] = [p[1]] + p[2]

def p_special_tail(p):
	'''
	statement_tail : special statement_tail
	'''
	p[0] = [p[1]] + p[2]
	
def p_statement_list_empty(p):
	''' statement_tail : '''
	#print('here')
	p[0] = []

def p_statement(p):
	''' statement : expression '''
	#print('here')
	p[0] = p[1]

def p_expression(p):
	'''
	expression : expression DIVIDE expression
	           | expression MULT expression
	           | expression PLUS expression
	           | expression MINUS expression
			   | expression LESS_THAN expression
	           | expression LESS_THAN_EQUAL expression
	           | expression GREATER_THAN expression
	           | expression GREATER_THAN_EQUAL expression
	           | expression EQUALS_EQUALS expression
	           | expression NOT_EQUAL expression
			   | expression MOD expression
			   | expression FLOOR_DIV expression
			   | expression EXPONENT expression
			   | expression AND expression
			   | expression OR expression
	'''
	#print("here")
	p[0] = OpNode(p[2], p[1], p[3])
	
def p_expression_not(p):
	'''
	expression : NOT expression
	'''
	p[0] = OpNode(p[1], p[2], Node())

def p_expression_parenthesis(p):
	'''
	expression : OPEN_PARENTHESIS expression CLOSE_PARENTHESIS
	'''
	p[0] = p[2]
	
def p_expression_int(p):
	'''
	expression : INT
	          
	'''
	#print("Here!")
	p[0] = IntNode(p[1])

def p_expression_real(p):
	'''
	expression : REAL
	          
	'''
	#print("Here!")
	p[0] = RealNode(p[1])
	
def p_expression_string(p):
	'''
	expression : STRING
	          
	'''
	#print("Here!")
	p[0] = StringNode(p[1])

def p_expression_list(p):
	'''
	expression : list
	'''
	#print("here")
	p[0] = ListNode(p[1])
	
def p_expression_list_def(p):
	'''
	list : OPEN_BRACKET inner CLOSE_BRACKET
	'''
	p[0] = p[2]
	
def p_expression_inner(p):
	'''
	inner : expression COMMA expression
	'''
	p[0] = [p[1]] + [p[3]]
		
def p_expression_inner2(p):
	'''
	inner : inner COMMA expression
	'''
	p[0] = p[1] + [p[3]]

def p_inner(p):
	'''
	inner : expression
	'''
	p[0] = [p[1]]

def p_expression_index(p):
	'''
	expression : expression OPEN_BRACKET expression CLOSE_BRACKET
			   
	'''
	p[0] = IndexNode(p[1], p[3])

def p_var_index(p):
	'''
	expression : NAME OPEN_BRACKET expression CLOSE_BRACKET EQUALS expression
	'''
	print("here")
	p[0] = VarIndexNode(p[1], p[3], p[6])
	
def p_var_assign(p):
	'''
	expression : NAME EQUALS expression
	'''
	p[0] = VariableNode(p[1], p[3])

def p_expression_var(p):
	'''
	expression : NAME
	'''
	p[0] = NameNode(p[1])

def p_expression_if(p):
	'''
	special : condition thenBlock elseBlock
	             
	'''
	p[0] = IfNode(p[1], p[2], p[3])
	
def p_condition(p):
	'''
	condition : IF OPEN_PARENTHESIS expression CLOSE_PARENTHESIS
	'''
	p[0] = p[3]
	
def p_thenBlock(p):
	'''
	thenBlock : block
	'''
	p[0] = p[1]

def p_elseBlock1(p):
	'''
	elseBlock : ELSE block
	'''
	p[0] = p[2]

def p_elseBlock2(p):
	'''
	elseBlock : 
	'''
	p[0] = Node()
	
def p_while(p):
	'''
	special : while_condition block
	'''
	p[0] = WhileNode(p[1], p[2])

def p_while_condition(p):
	'''
	while_condition : WHILE OPEN_PARENTHESIS expression CLOSE_PARENTHESIS
	'''
	p[0] = p[3]

def p_print(p):
	'''
	statement : PRINT OPEN_PARENTHESIS expression CLOSE_PARENTHESIS
	'''
	p[0] = PrintNode(p[3])
	
def p_error(p):
	print("SYNTAX ERROR1")
	raise Exception
	
def p_empty(p):
	'''
	empty :
	'''
	p[0] = None


def checkNum(p):
	return checkInt(p) or checkReal(p)
	
def checkInt(p):
	return isinstance(p, int)
	
def checkReal(p):
	return isinstance(p, float)

def checkString(p):
	return isinstance(p, str)

def checkList(p):
	return isinstance(p, list)

def checkCompatible(p1, p2):
	if checkNum(p1) and checkNum(p2):
		return True
	elif checkString(p1) and checkString(p2):
		return True
	elif checkList(p1) and checkList(p2):
		return True
	return False

def compare(op, val1, val2):
	if op == '<=':
		bool = val1 <= val2
	elif op == '>=':
		bool = val1 >= val2
	elif op == '==':
		bool = val1 == val2
	elif op == '<>':
		bool = not (val1 == val2)
	elif op == '<':
		bool = val1 < val2
	elif op == '>':
		bool = val1 > val2
	else:
		return 0
	if(bool > 0):
		return 1
	else:
		return 0
		
def compareBool(op, val1, val2):
	if op == 'and':
		bool = val1 and val2
	elif op == 'or':
		bool = val1 or val2
	if bool:
		return 1
	return 0

parser = yacc.yacc()



try:	
	file = open(sys.argv[1], "r")
except:
	print("File not found")
	while(1):
		try:
			error = False
			s = "{" + input('>> ') + "}"
		except EOFError:
			break
		ast = parser.parse(s)
		ast.execute();
	
try:
	ast = parser.parse(file.read())
	ast.execute();
except:
	print("SYNTAX ERROR")
#print("lalala\n")

	
	
	
	

	
	
	
	
	
	
	