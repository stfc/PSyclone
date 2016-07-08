from expression import var_or_function, expression, slicing

''' Module containing tests for the Fortran expression parser '''


def my_test(name, parser, test_string, names=None):
    '''This function ensures that the parse-unparse and parse-repr-unparse
    operations are equivalent to the identity. Note that whitespace is not
    preserved by the parsing operation, so the test_string must conform to
    the whitespace conventions of the unparser for the test to succeed.'''
    from expression import Grouping, BinaryOperator, FunctionVar, Slicing

    s = parser.parseString(test_string)
    assert (str(s[0]) == test_string), "Failed to parse " + name + "."
    exec("s="+repr(s[0]))
    assert (str(s) == test_string), "Error in repr for " + name + "."
    if names:
        assert s.names == set(names), "Names do not match for " + name + "."


def test_function_calls():
    ''' Test parsing of expressions containing function calls '''
    my_test("function calls",
            var_or_function, "foo(bar(baz, bam), wibble(wub))",
            names=["foo", "bar", "baz", "bam", "wibble", "wub"])


def test_trivial_slice():
    ''' Test parsing of simplest possible array slice '''
    my_test("trivial slice", slicing, ":")


def test_simple_slice():
    ''' Test parsing of array slice with range and stride '''
    my_test("simple slice", slicing, "1:2:3")


def test_stride_slice():
    ''' Test parsing of slice consisting of stride only '''
    my_test("stride slice", slicing, "::3")


def test_exponent():
    ''' Test parsing of exponents '''
    my_test("exponent", expression, "2 ** 3 ** 4")


def test_plus_mult():
    ''' Test parsing of expression containing addition and multiplication '''
    my_test("plus mult",
            expression,
            "f(x) + g(x, y) * 2",
            names=["f", "g", "x", "y"])


def test_group():
    ''' Test parsing of a grouped expression (i.e. within parentheses) '''
    my_test("group", expression, "(x)", names="x")


def test_real():
    ''' Test parsing of a real scalar '''
    my_test("real", expression, "-.5e-200")


def test_group_operations():
    ''' Test parsing of a group of operations '''
    my_test("group operations",
            expression, "(f(x + 2 * y, z:z + 2 + -.5) + (g + h) ** (z - 2))",
            names=["f", "g", "h", "x", "y", "z"])
