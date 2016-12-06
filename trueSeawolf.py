import ply.lex as lex
import ply.yacc as yacc
import sys

class Node:
	def __init__(self):
		print("Node")
	def evaluate(self):
		print("Evaluate")
		return 0
	def execute(self):
		print("Execute")
	
class IntNode(Node):
	def __init__(self, v):
		self.value = int(v)
		print("IntNode")
	def evaluate(self):
		print("Evaluate IntNode")
		return self.value
	def execute(self):
		print("Execute IntNode")

class RealNode(Node):
	def __init__(self, v):
		self.value = int(v)
		print("RealNode")
	def evaluate(self):
		print("Evaluate RealNode")
		return self.value
	def execute(self):
		print("Execute RealNode")

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

class OpNode(Node):
	def __init__(self, op, var1, var2):
		self.op = op
		self.var1 = var1
		self.var2 = var2
	def evaluate(self):
		if()
# @author - ROBERT TRUJILLO
# CSE 307 HW 4

global error
error = False

keywords = {
	'and' : 'AND',
	'or'  : 'OR',
	'in'  : 'IN',
	'not' : 'NOT',
	'print' : 'PRINT'
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

tokens = operators + [
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
	"LIST",
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


t_ignore = r' '

def t_ID(t):
	r'[a-zA-Z]+'
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
	t.value = t.value[1:len(t.value)-1]
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
	('right', 'UMINUS'),
	('left', 'OPEN_BRACKET', 'CLOSE_BRACKET'),
	('left', 'OPEN_PARENTHESIS', 'CLOSE_PARENTHESIS')

)

def p_calc(p):
	'''
	calc : expression
	     | var_assign
	     | empty	
	'''
	val = run(p[1])
	if val != None and not error:
		print(val)
		
def p_var_assign(p):
	'''
	var_assign : NAME EQUALS expression
	'''
	p[0] = ('=', p[1], p[3])

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
	p[0] = (p[2], p[1], p[3])

def p_expression_not(p):
	'''
	expression : NOT expression
	'''
	p[0] = (p[1], p[2])
	
def p_expression_uminus(p):
	"expression : '-' expression %prec UMINUS"
	p[0] = -p[2]

def p_expression_var(p):
	'''
	expression : NAME
	'''
	p[0] = ('var', p[1])

def p_expression_parenthesis(p):
	'''
	expression : OPEN_PARENTHESIS expression CLOSE_PARENTHESIS
	'''
	p[0] = run(p[2])
	
def p_expression_int(p):
	'''
	expression : INT
	          
	'''
	print("Here!")
	p[0] = IntNode(p[1])

def p_expression_real(p):
	'''
	expression : REAL
	          
	'''
	#print("Here!")
	p[0] = RealNode(p[1])
	
def p_expression_int(p):
	'''
	expression : STRING
	          
	'''
	#print("Here!")
	p[0] = StringNode(p[1])

def p_expression_index(p):
	'''
	expression : expression OPEN_BRACKET expression CLOSE_BRACKET
	'''
	index = run(p[3])
	val = run(p[1])
	if not isinstance(index, int):
		print("SEMANTIC ERROR")
		return
	if isinstance(val, str) or isinstance(val, list):
		p[0] = val[index]

def p_expression_list(p):
	'''
	list : OPEN_BRACKET inner CLOSE_BRACKET
	'''
	p[0] = run(p[2])
	
def p_expression_inner(p):
	'''
	inner : expression COMMA expression
	'''
	p[0] = [run(p[1])] + [run(p[3])]
		
def p_expression_inner2(p):
	'''
	inner : inner COMMA expression
	'''
	p[0] = run(p[1]) + [run(p[3])]

def p_inner(p):
	'''
	inner : expression
	'''
	p[0] = [run(p[1])]



def p_statement_block(p):
	''' block : OPEN_CURLY statement_list CLOSE_CURLY '''
	p[0] = BlockNode(p[2])

def p_statement_list(p):
	''' statement_list :  statement statement_list'''
	p[0] = [p[1]] + p[2]
	
def p_statement_list(p):
	''' statement_list : '''
	p[0] = []

def p_statement(p):
	''' statement : expression '''
	p[0] = p[1]
	
def p_error(p):
	global error
	error = True
	print("SYNTAX ERROR")
	
	
def p_empty(p):
	'''
	empty :
	'''
	p[0] = None

def compare(p):
	if p[0] == '<=':
		bool = run(p[1]) <= run(p[2])
	elif p[0] == '>=':
		bool = run(p[1]) >= run(p[2])
	elif p[0] == '==':
		bool = run(p[1]) == run(p[2])
	elif p[0] == '<>':
		bool = not (run(p[1]) == run(p[2]))
	elif p[0] == '<':
		bool = run(p[1]) < run(p[2])
	elif p[0] == '>':
		bool = run(p[1]) > run(p[2])
	else:
		return 0
	if(bool > 0):
		return 1
	else:
		return 0

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
	
def compareBool(p):
	if p[0] == 'and':
		bool = run(p[1]) and run(p[2])
	elif p[0] == 'or':
		bool = run(p[1]) or run(p[2])
	if bool:
		return 1
	return 0

parser = yacc.yacc()
env = {}
def run(p):
	global env
	if type(p) == tuple:
		if p[0] == '+':
			if checkCompatible(run(p[1]), run(p[2])):
				return run(p[1]) + run(p[2])
			return "SEMANTIC ERROR"
		elif p[0] == '-':
			if checkNum(run(p[1])) and checkNum(run(p[2])):
				return run(p[1]) - run(p[2])
			return "SEMANTIC ERROR"
		elif p[0] == '*':
			if checkNum(run(p[1])) and checkNum(run(p[2])):
				return run(p[1]) * run(p[2])
			return "SEMANTIC ERROR"
		elif p[0] == '/':
			if checkNum(run(p[1])) and checkNum(run(p[2])) and run(p[2]) != 0:
				return run(p[1]) / run(p[2])
			return "SEMANTIC ERROR"
		elif p[0] == '<' or p[0] == '<=' or p[0] == '>' or p[0] == '>=' or p[0] == '==' or p[0] == '<>':
			if checkInt(run(p[1])) and checkInt(run(p[2])):
				return compare(p)
			return "SEMANTIC ERROR"
		elif p[0] == 'and' or p[0] == 'or':
			if checkInt(run(p[1])) and checkInt(run(p[2])):
				return compareBool(p)
			return "SEMANTIC ERROR"
		elif p[0] == 'not':
			if checkInt(run(p[1])):
				if not run(p[1]):
					return 1
				return 0
			return "SEMANTIC ERROR"
		elif p[0] == '%':
			if checkNum(run(p[1])) and checkNum(run(p[2])):
				return run(p[1]) % run(p[2])
			return "SEMANTIC ERROR"
		elif p[0] == '**':
			if checkNum(run(p[1])) and checkNum(run(p[2])):
				return run(p[1]) ** run(p[2])
			return "SEMANTIC ERROR"
		elif p[0] == '//':
			if checkNum(run(p[1])) and checkNum(run(p[2])) and run(p[2]) != 0: 
				return run(p[1]) // run(p[2])
			return "SEMANTIC ERROR"
		elif p[0] == '=':
			val = run(p[2])
			if(val != "SEMANTIC ERROR"):
				env[p[1]] = val
			else:
				print("SEMANTIC ERROR")
			
		elif p[0] == 'var':
			if p[1] not in env:
				return "Undeclared Variable"
			else:
				return env[run(p[1])]
		
	else:
		return p

try:	
	file = open(sys.argv[1], "r")
	code = ''
	for line in file:
		error = False
		code = code + line
		#parser.parse(line)
	ast = parser.parse(file.read())
	ast.execute();
except:
	while True:
		#tok = lexer.token()
		#if not tok:
		#	break
		#print(tok)
		
		try:
			error = False
			s = input('>> ')
		except EOFError:
			break
		ast = parser.parse(s)
		ast.execute();
	
	
	
	

	
	
	
	
	
	
	