#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author D. Ham Imperial College

# A simple Fortran expression parser. Note that this does not parse Fortran,
# only legal Fortran expressions.
#

from pyparsing import *
# Enable the packrat optimisation. This seems to be performance-critical.
ParserElement.enablePackrat()

class ExpressionNode(object):
    '''Base class for all expression tree nodes'''
    def __init__(self, toks):
        ''' The recursive collection of names enables the dependencies of
    expressions to be analysed. ''' 
        self.names=set()
        for t in toks:
            if isinstance(t, ExpressionNode):
                self.names.update(t.names)

        # Keep the list of toks for future reference.
        self.toks = toks

    def walk(self):
        # Generator for depth-first walk of this expression.
        for t in self.toks:
            if isinstance(t, ExpressionNode):
                for i in t.walk():
                    yield(i)

        yield(self)

    def walk_skipping_name(self):
        # Generator for depth-first walk of this expression, skipping the name part of the walk.
        for t in self.toks:
            if isinstance(t, ExpressionNode):
                for index,i in enumerate(t.walk_skipping_name()):
                    if index>0:
                        yield(i)
        yield(self)


class Grouping(ExpressionNode):
    """Expression node for a parenthesised expression."""
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)
        
        self.expr=toks[1]

    def __repr__(self):
        return "Grouping(['(',"+ repr(self.expr)+",')'])"

    def __str__(self):
        return "("+str(self.expr)+")"
        

class BinaryOperator(ExpressionNode):
    """Expression node for one or more binary operators with the same precedence.

    For some reason, operator tokens come in a list of lists.
    """
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks[0])

        self.operands = [toks[0][0]]
        self.symbols = []
        i=iter(toks[0][1:])
        try:
            while True:
                t=i.next()
                self.symbols.append(t)
                t=i.next()                
                self.operands.append(t)
        except StopIteration:
            pass
                         
    def __repr__(self):
        _str = "BinaryOperator([["+repr(self.operands[0])
        for s, o in zip(self.symbols, self.operands[1:]):
            _str += ", "+repr(s)+", "+repr(o)
        _str += "]])"
        return _str

    def __str__(self):
        return str(self.operands[0])+\
            "".join([" "+str(s)+" "+str(o) for s, o in zip(self.symbols, self.operands[1:])])
    

class Slicing(ExpressionNode):
    """Expression node for Fortran colon array slicings."""
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)

        self.start = ""
        self.stop = ""
        self.stride = ""

        tokiter = iter(toks)
        t=tokiter.next()
        if t!=":":
            self.start=t
            t=tokiter.next()

        try:
            t=tokiter.next()
            if t!=":":
                self.stop=t
                t=tokiter.next()
            
            t=tokiter.next()
            self.stride=t
        except StopIteration:
            pass

        
    def __repr__(self):
        _str = "Slicing(["
        if self.start:
            _str += repr(self.start)+", "
        _str += "':'"
        if self.stop:
            _str += ", "+repr(self.stop)
        if self.stride:
            _str += ", ':', "+repr(self.stride)
        _str += "])"
        return _str

    def __str__(self):
        _str = str(self.start)+":"+str(self.stop)
        if self.stride:
            _str += ":"+str(self.stride)
        return _str
        

class FunctionVar(ExpressionNode):
    """Expression node for a Fortran variable or function call."""
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)

        self.name = toks[0]
        self.names.update([self.name])

        if len(toks)>1:
            self.args = toks[2:-1]
        else:
            self.args=None

    def __repr__(self):
        _str = "FunctionVar(['"+self.name+"'"
        if self.args is not None:
            _str += ",'(',"+", ".join([repr(a) for a in self.args])+",')'"
        _str += "])"
        return _str

    def __str__(self):
        _str=str(self.name)
        if self.args is not None:
            _str += '('+", ".join([str(a) for a in self.args])+')'
        return _str


class LiteralArray(ExpressionNode):
    """Expression node for a Fortran literal array."""
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)
        self.expr=toks[1:-1]    # first and last are delimiters

    def __getitem__(self, idx):
        return self.expr[idx]

    def __len__(self):
        return len(self.expr)

    def __repr__(self):
        return "LiteralArray(['(',"+ repr(self.expr)+",')'])"

    def __str__(self):
        return "["+str(self.expr)+"]"

# Let's start with integers.
integer = Optional(Word("+-", exact=1)) + Word(nums)
unsigned = Word(nums)
point = Literal(".")
real = Combine(
    (Word("+-"+nums, nums)+point+Optional(unsigned)|point+unsigned)
    +Optional(Word("dDeE",exact=1)
              +Optional(integer)
              )
    )

# Literal brackets.
lpar  = Literal( "(" )
rpar  = Literal( ")" )

lit_array_start = Literal( "[" ) | Literal( "(/" )
lit_array_end = Literal( "]" ) | Literal( "/)" )

# A Fortran name starts with a letter and continues with letters, numbers
# and _. Can you start a name with _?
name = Word(alphas, alphanums+"_") | Literal(".false.") | Literal(".true.")

expr = Forward()

colon = Literal( ":" )
slicing = Optional(expr)+colon+Optional(expr)+Optional(colon+Optional(expr))
slicing.setParseAction(lambda strg, loc, toks: [Slicing(toks)])

var_or_function = name + Optional(lpar + delimitedList(slicing | expr) + rpar)
var_or_function.setParseAction(lambda strg, loc, toks: [FunctionVar(toks)])

literal_array = lit_array_start + delimitedList(expr) + lit_array_end
literal_array.setParseAction(lambda strg, loc, toks: [LiteralArray(toks)])

group = lpar+expr+rpar
group.setParseAction(lambda strg, loc, toks: [Grouping(toks)])

operand = (group | var_or_function | real | integer | literal_array)

# Cause the binary operators to work.
operator = operatorPrecedence\
    ( operand,
      (
            (Literal("**"), 2, opAssoc.RIGHT, lambda strg, loc, toks: [BinaryOperator(toks)]),
            (Literal("*")|Literal("/"), 2, opAssoc.LEFT, lambda strg, loc,
    toks: [BinaryOperator(toks)]),
            (Literal("+")|Literal("-"), 2, opAssoc.LEFT, lambda strg, loc, toks: [BinaryOperator(toks)]),
            )
      )

expr << (operator | operand)

expression = StringStart() + expr + StringEnd()

def my_test(name, parser, test_string, names=None):
    '''This function ensures that the parse-unparse and parse-repr-unparse
    operations are equivalent to the identity. Note that whitespace is not
    preserved by the parsing operation, so the test_string must conform to
    the whitespace conventions of the unparser for the test to succeed.'''

    s=parser.parseString(test_string)
    assert (str(s[0])==test_string), "Failed to parse "+name+"."
    exec("s="+repr(s[0]))
    assert (str(s)==test_string), "Error in repr for "+name+"."
    if names:
        assert s.names==set(names), "Names do not match for "+name+"."


if __name__=="__main__":
    my_test("function calls", var_or_function, "foo(bar(baz, bam), wibble(wub))", 
         names=["foo", "bar", "baz", "bam", "wibble", "wub"])
    my_test("trivial slice", slicing, ":")
    my_test("simple slice", slicing, "1:2:3")
    my_test("stride slice", slicing, "::3")
    my_test("exponent", expression, "2 ** 3 ** 4")
    my_test("plus mult", expression, "f(x) + g(x, y) * 2", names=["f", "g", "x", "y"])
    my_test("group", expression, "(x)", names="x")
    my_test("real", expression, "-.5e-200")
    my_test("group operations", expression, "(f(x + 2 * y, z:z + 2 + -.5) + (g + h) ** (z - 2))", 
         names=["f", "g", "h", "x", "y", "z"])
