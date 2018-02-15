# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author D. Ham Imperial College
# Modified by R. Ford and A. R. Porter, STFC Daresbury Laboratory

''' A simple Fortran expression parser. Note that this does not parse Fortran,
only legal Fortran expressions. '''

import pyparsing as pparse
# Enable the packrat optimisation. This seems to be performance-critical.
pparse.ParserElement.enablePackrat()


class ExpressionNode(object):
    '''Base class for all expression tree nodes'''
    def __init__(self, toks):
        ''' The recursive collection of names enables the dependencies of
        expressions to be analysed. '''
        self.names = set()
        for tok in toks:
            if isinstance(tok, ExpressionNode):
                self.names.update(tok.names)

        # Keep the list of toks for future reference.
        self.toks = toks

    def walk_skipping_name(self):
        ''' Generator for depth-first walk of this expression, skipping
        the name part of the walk. '''
        for tok in self.toks:
            if isinstance(tok, ExpressionNode):
                for index, i in enumerate(tok.walk_skipping_name()):
                    if index > 0:
                        yield i
        yield self


class Grouping(ExpressionNode):
    '''Expression node for a parenthesised expression.'''
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)

        self.expr = toks[1]

    def __repr__(self):
        return "Grouping(['('," + repr(self.expr) + ",')'])"

    def __str__(self):
        return "(" + str(self.expr) + ")"


class BinaryOperator(ExpressionNode):
    '''Expression node for one or more binary operators with the same
    precedence.

    For some reason, operator tokens come in a list of lists.'''
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks[0])

        self.operands = [toks[0][0]]
        self.symbols = []
        i = iter(toks[0][1:])
        try:
            while True:
                tok = i.next()
                self.symbols.append(tok)
                tok = i.next()
                self.operands.append(tok)
        except StopIteration:
            pass

    def __repr__(self):
        _str = "BinaryOperator([["+repr(self.operands[0])
        for sym, opd in zip(self.symbols, self.operands[1:]):
            _str += ", " + repr(sym) + ", " + repr(opd)
        _str += "]])"
        return _str

    def __str__(self):
        return str(self.operands[0]) + \
            "".join([" "+str(sym)+" "+str(opd) for sym, opd in
                     zip(self.symbols, self.operands[1:])])


class Slicing(ExpressionNode):
    """Expression node for Fortran colon array slicings."""
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)

        self.start = ""
        self.stop = ""
        self.stride = ""

        tokiter = iter(toks)
        tok = tokiter.next()
        if tok != ":":
            self.start = tok
            tok = tokiter.next()

        try:
            tok = tokiter.next()
            if tok != ":":
                self.stop = tok
                tok = tokiter.next()

            tok = tokiter.next()
            self.stride = tok
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
    '''Expression node for a Fortran variable or function call.'''
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)

        self.name = toks[0]
        self.names.update([self.name])

        if len(toks) > 1:
            # No. of args is not sufficient to determine whether this
            # is a function call since a function may have zero
            # args.
            self._is_fn = True
            self.args = toks[2:-1]
        else:
            self._is_fn = False
            self.args = None

    def __repr__(self):
        _str = "FunctionVar(['" + self.name + "'"
        if self._is_fn:
            _str += ",'(',"
            if self.args:
                _str += ", ".join([repr(a) for a in self.args]) + ","
            _str += "')'"
        _str += "])"
        return _str

    def __str__(self):
        _str = str(self.name)
        if self._is_fn:
            _str += '('
            if self.args is not None:
                _str += ", ".join([str(a) for a in self.args])
            _str += ')'
        return _str


class LiteralArray(ExpressionNode):
    '''Expression node for a Fortran literal array.'''
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)
        self.expr = toks[1:-1]    # first and last are delimiters

    def __getitem__(self, idx):
        return self.expr[idx]

    def __len__(self):
        return len(self.expr)

    def __repr__(self):
        _str = "LiteralArray(['[',"
        if self.expr:
            _str += ", ".join([repr(a) for a in self.expr])
        _str += ", ']'])"
        return _str

    def __str__(self):
        _str = "[" + str(self.expr[0])
        for tok in self.expr[1:]:
            _str += ", "+tok
        _str += "]"
        return _str


class NamedArg(ExpressionNode):
    ''' Expression node for a Fortran named argument. '''
    def __init__(self, toks):
        ExpressionNode.__init__(self, toks)

        # First token is the name of the argument
        self._name = toks[0]
        self.names.update([self._name])

        # The second token is the '=' so we ignore that and skip to
        # the third token which contains the value assigned to the
        # argument...
        # The named variable can be assigned a character string. We've
        # told the parser not to remove the delimiters so that we can
        # see whether they are single or double quotes.
        first_char = toks[2][0]
        if first_char in ["'", '"']:
            # Store the quotation character and the content of the string
            # excluding the quotation marks
            self._quote = toks[2][0]
            self._value = toks[2][1:-1]
        else:
            # The quantity being assigned is not a string
            self._quote = None
            self._value = toks[2]

    def __repr__(self):
        if self._quote:
            if self._quote == "'":
                _str = "NamedArg(['{0}', '=', \"'{1}'\"])".format(self._name,
                                                                  self._value)
            else:
                _str = 'NamedArg(["{0}", "=", \'"{1}"\'])'.format(self._name,
                                                                  self._value)
        else:
            _str = "NamedArg(['{0}', '=', '{1}'])".format(self._name,
                                                          self._value)
        return _str

    def __str__(self):
        _str = str(self._name) + "="
        if self._quote:
            _str += self._quote + str(self._value) + self._quote
        else:
            _str += str(self._value)
        return _str

    @property
    def name(self):
        ''' Returns the name of the variable (LHS) involved in a
        named argument. '''
        return self._name

    @property
    def value(self):
        ''' Returns the value (RHS) of the named argument '''
        return self._value

    @property
    def is_string(self):
        ''' Returns True if the RHS of the named argument is a string '''
        return self._quote is not None


# Construct a grammar using PyParsing

# A Fortran variable name starts with a letter and continues with
# letters, numbers and _. Can you start a name with _?
VAR_NAME = pparse.Word(pparse.alphas, pparse.alphanums+"_")
NAME = VAR_NAME | pparse.Literal(".false.") | pparse.Literal(".true.")

# Reference to a component of a derived type
DERIVED_TYPE_COMPONENT = pparse.Combine(VAR_NAME + "%" + VAR_NAME)

# An unsigned integer
UNSIGNED = pparse.Word(pparse.nums)

# In Fortran, a numerical constant can have its kind appended after an
# underscore. The kind can be a 'name' or just digits.
KIND = pparse.Word("_", exact=1) + (VAR_NAME | UNSIGNED)

# First arg to Word gives allowed initial chars, 2nd arg gives allowed
# body characters
SIGNED = pparse.Word("+-"+pparse.nums, pparse.nums)
INTEGER = pparse.Combine(SIGNED + pparse.Optional(KIND))

POINT = pparse.Literal(".")

# A floating point number
REAL = pparse.Combine(
    (SIGNED + POINT + pparse.Optional(UNSIGNED) | POINT + UNSIGNED) +
    pparse.Optional(pparse.Word("dDeE", exact=1) + SIGNED) +
    pparse.Optional(KIND))

# Literal brackets.
LPAR = pparse.Literal("(")
RPAR = pparse.Literal(")")

LIT_ARRAY_START = pparse.Literal("[") | pparse.Literal("(/")
LIT_ARRAY_END = pparse.Literal("]") | pparse.Literal("/)")

EXPR = pparse.Forward()

# Array slicing
COLON = pparse.Literal(":")
SLICING = pparse.Optional(EXPR) + COLON + pparse.Optional(EXPR) + \
    pparse.Optional(COLON+pparse.Optional(EXPR))
SLICING.setParseAction(lambda strg, loc, toks: [Slicing(toks)])

VAR_OR_FUNCTION = (DERIVED_TYPE_COMPONENT | NAME) + pparse.Optional(
    LPAR + pparse.Optional(pparse.delimitedList(SLICING | EXPR)) + RPAR)
VAR_OR_FUNCTION.setParseAction(lambda strg, loc, toks: [FunctionVar(toks)])

LITERAL_ARRAY = LIT_ARRAY_START + pparse.delimitedList(EXPR) + LIT_ARRAY_END
LITERAL_ARRAY.setParseAction(lambda strg, loc, toks: [LiteralArray(toks)])

# An optional/named argument. We use QuotedString here to avoid versioning
# problems with the interface to {sgl,dbl}QuotedString in pyparsing.
OPTIONAL_VAR = VAR_NAME + "=" + ((NAME | REAL | INTEGER) |
                                 pparse.QuotedString("'",
                                                     unquoteResults=False) |
                                 pparse.QuotedString('"',
                                                     unquoteResults=False))
# lambda creates a temporary function which, in this case, takes three
# arguments and creates a NamedArg object.
OPTIONAL_VAR.setParseAction(lambda strg, loc, toks: [NamedArg(toks)])

GROUP = LPAR + EXPR + RPAR
GROUP.setParseAction(lambda strg, loc, toks: [Grouping(toks)])

# Parser will attempt to match with the expressions in the order they
# are specified here. Therefore must list them in order of decreasing
# generality
OPERAND = (GROUP | OPTIONAL_VAR | VAR_OR_FUNCTION | REAL | INTEGER |
           LITERAL_ARRAY)

# Cause the binary operators to work.
OPERATOR = pparse.operatorPrecedence(
    OPERAND,
    ((pparse.Literal("**"), 2, pparse.opAssoc.RIGHT,
      lambda strg, loc, toks: [BinaryOperator(toks)]),
     (pparse.Literal("*") | pparse.Literal("/"), 2, pparse.opAssoc.LEFT,
      lambda strg, loc, toks: [BinaryOperator(toks)]),
     (pparse.Literal("+") | pparse.Literal("-"), 2, pparse.opAssoc.LEFT,
      lambda strg, loc, toks: [BinaryOperator(toks)]),))

EXPR << (OPERATOR | OPERAND)

FORT_EXPRESSION = pparse.StringStart() + EXPR + pparse.StringEnd()
