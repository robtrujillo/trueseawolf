# Robert Trujillo 110668776

import ply.lex as lex
import ply.yacc as yacc
import sys

# @author - ROBERT TRUJILLO
# CSE 307 HW 4-6
global variables
variables = {}

global methods
methods = {}

class Stack:
	def __init__(self):
		self.items = []
	def isEmpty(self):
		return self.items == []
	def push(self, item):
		self.items.append(item)
	def pop(self):
		return self.items.pop()
	def peek(self):
		return self.items[len(self.items)-1]
	def size(self):
		return len(self.items)
	def peekMain(self):
		return self.items[0]
	def printStack(self):
		for item in self.items:
			print(item)

global scope
scope = Stack()
scope.push(variables)
		
class Node:
	def __init__(self):
		self.value = 0
	def evaluate(self):
		return 0
	def execute(self):
		self.value=0
	def type(self):
		return "Node"
	
class IntNode(Node):
	def __init__(self, v):
		self.value = int(v)
	def evaluate(self):
		return self.value
	def execute(self):
		return self.evaluate()
	def type(self):
		return "IntNode"

class RealNode(Node):
	def __init__(self, v):
		self.value = float(v)
	def evaluate(self):
		return self.value
	def execute(self):
		return self.evaluate()
	def type(self):
		return "RealNode"

class StringNode(Node):
	def __init__(self, v):
		self.value = str(v)
		self.value = self.value[1:-1] # to eliminate the left and right double quotes
	def evaluate(self):
		return self.value
	def execute(self):
		return self.evaluate()
	def type(self):
		return "StringNode"

class PrintNode(Node):
	def __init__(self, v):
		self.value = v
	def evaluate(self):
		return 0
	def execute(self):
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
		return self.evaluate()
	def type(self):
		return "IfNode"

class BlockNode(Node):
	def __init__(self, sl):
		self.statementNodes = sl
	def evaluate(self):
		return 0
	def execute(self):
		for statement in self.statementNodes:
			if statement.type() == "ReturnNode":
				return statement.execute()
			else:
				ret = statement.execute()
				if ret != None:
					return ret
		
	def type(self):
		return "BlockNode"

class ReturnNode(Node):
	def __init__(self, val):
		self.val = val
	def evaluate(self):
		return self.val.evaluate()
	def execute(self):
		return self.evaluate()
	def type(self):
		return "ReturnNode"

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
		return self.evaluate()
	def type(self):
		return "IndexNode"
		
class VariableNode(Node):
	def __init__(self, name, val):
		self.name = name
		self.val = val
	def evaluate(self):
		return self.val.evaluate()
	def execute(self):
		if self.name.type() == "IndexNode":
			varName = self.name.list.var
			varIndex = self.name.index.evaluate();
			if varName in scope.peek():
				scope.peek()[varName][varIndex] = self.val.evaluate()
			elif varName in scope.peekMain():
				scope.peekMain()[varName][varIndex] = self.val.evaluate()
			else:
				print("SEMANTIC ERROR - VARIABLE DOES NOT EXIST")
		else:
			scope.peek()[self.name.var] = self.evaluate()
	def type(self):
		return "VariableNode"		

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
		
class NameNode(Node):
	def __init__(self, var):
		self.var = var
	def evaluate(self):
		
		if self.var in scope.peek():
			return scope.peek()[self.var]
		elif self.var in scope.peekMain():
			return scope.peekMain()[self.var]
		else:
			print("SEMANTIC ERROR - NAME")
	def execute(self):
		return self.evaluate()
	def type(self):
		return "NameNode"

class ProgramNode(Node):
	def __init__(self, stmts):
		self.stmts = stmts
	def evaluate(self):
		for stmt in self.stmts:
			stmt.execute()
	def execute(self):
		return self.evaluate()
	def type(self):
		return "ProgramNode"
	
class MethodNode(Node):
	def __init__(self, name, parameters, block):
		self.name = name
		self.params = parameters
		self.block = block
	def evaluate(self):
		return 0
	def execute(self):
		methods[self.name] = self

class MethodCallNode(Node):
	def __init__(self, name, parameters):
		self.name = name
		self.params = parameters
	def evaluate(self):
		self.dictionary = {}
		# check that method exists
		if not self.name in methods:
			print("METHOD DOES NOT EXIST")
			return 0
		method = methods[self.name]
		
		# check that parameters are good
		numParams = len(method.params)
		if numParams != len(self.params):
			print("NUM PARAMS DONT MATCH")
			return 0
		
		# store paramter values in environment
		i = 0
		#print(self.params)
		for param in method.params:
			self.dictionary[param.var] = self.params[i].evaluate()
			i+=1
		scope.push(self.dictionary)
		
		# execute the function
		ret = method.block.execute()
		
		# remove the function environment from the stack
		scope.pop()
		
		# return value if there is something to return
		if ret != None:
			return ret
	
	def execute(self):
		return self.evaluate()
	
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
				raise Exception("SEMANTIC ERROR - OPNODE NOT INTS")
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
			print("SEMANTIC ERROR - OPNODE NOT INTS")
	def execute(self):
		return self.evaluate()
	def type(self):
		return "OpNode"
			

keywords = {
	'and'    : 'AND',
	'or'     : 'OR',
	'not'    : 'NOT',
	'print'  : 'PRINT',
	'if'     : 'IF',
	'else'   : 'ELSE',
	'while'  : 'WHILE',
	'return' : 'RETURN'
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
	"AND",
	"OR",
	"NOT"
]

special_tokens = operators + [
	"IF",
	"ELSE",
	"PRINT",
	"WHILE",
	"RETURN"
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
	('left', 'PLUS', 'MINUS'),
	('left', 'FLOOR_DIV'),
	('left', 'EXPONENT'),
	('left', 'MOD'),
	('left', 'MULT', 'DIVIDE'),
	('left', 'OPEN_BRACKET', 'CLOSE_BRACKET'),
	('left', 'OPEN_PARENTHESIS', 'CLOSE_PARENTHESIS')

)

def p_program(p):
	'''
	program : pgrm_tail
	'''
	p[0] = ProgramNode(p[1])
	
def p_pgrm_tail1(p):
	'''
	pgrm_tail : var_assign SEMI pgrm_tail
			  | block pgrm_tail
			  |
	'''
	if len(p) > 3:
		p[0] = [p[1]] + p[3]
	elif len(p) > 2:
		p[0] = [p[1]] + p[2]
	else:
		p[0] = []

def p_pgrm_tail2(p):
	'''
	pgrm_tail : function_def pgrm_tail
	'''
	p[0] = [p[1]] + p[2]


def p_function_def(p):
	'''
	function_def : NAME OPEN_PARENTHESIS parameters CLOSE_PARENTHESIS block
	'''
	p[0] = MethodNode(p[1], p[3], p[5])

def p_function_def1(p):
	'''
	function_def : NAME OPEN_PARENTHESIS CLOSE_PARENTHESIS block
	'''
	p[0] = MethodNode(p[1], [], p[4])
	
def p_function_call(p):
	'''
	function_call : NAME OPEN_PARENTHESIS parameters CLOSE_PARENTHESIS
	'''
	p[0] = MethodCallNode(p[1], p[3])

def p_function_call1(p):
	'''
	function_call : NAME OPEN_PARENTHESIS CLOSE_PARENTHESIS
	'''
	p[0] = MethodCallNode(p[1], [])
	


def p_expression_function(p):
	'''
	expression : function_call
	'''
	p[0] = p[1]

	
def p_parameter1(p):
	'''
	parameters : expression COMMA expression
	'''
	p[0] = [p[1]] + [p[3]]
		
def p_parameter2(p):
	'''
	parameters : parameters COMMA expression
	'''
	p[0] = p[1] + [p[3]]

def p_parameter3(p):
	'''
	parameters : expression
	'''
	p[0] = [p[1]]


	
def p_expression_return(p):
	'''
	expression : return_stmt
	'''
	p[0] = p[1]
	
def p_return(p):
	'''
	return_stmt : RETURN expression
	'''
	p[0] = ReturnNode(p[2])
		
def p_statement_block(p):
	'''
	block : OPEN_CURLY statement_list CLOSE_CURLY
	'''
	#print('here')
	p[0] = BlockNode(p[2])

def p_statement_list(p):
	'''
	statement_list :  statement SEMI statement_tail
	'''
	#print('here')
	p[0] = [p[1]] + p[3]

def p_statement_tail(p):
	'''
	statement_tail :  statement SEMI statement_tail
	'''
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
	''' 
	statement_tail : 
	'''
	p[0] = []

def p_statement(p):
	'''
	statement : expression
	'''
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
	p[0] = IntNode(p[1])

def p_expression_real(p):
	'''
	expression : REAL
	          
	'''
	p[0] = RealNode(p[1])
	
def p_expression_string(p):
	'''
	expression : STRING
	          
	'''
	p[0] = StringNode(p[1])

def p_expression_list(p):
	'''
	expression : list
	'''
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

def p_var(p):
	'''
	expression : var_assign
	'''
	
	p[0] = p[1]
	
def p_var_assign(p):
	'''
	var_assign : expression EQUALS expression
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
	print("SYNTAX ERROR - P_ERROR")
	raise Exception

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
	

ast = parser.parse(file.read())
ast.execute();


	
	
	
	

	
	
	
	
	
	
	