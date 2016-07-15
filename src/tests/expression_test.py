
''' Module containing tests for the Fortran expression parser '''

from expression import VAR_OR_FUNCTION, FORT_EXPRESSION, SLICING


def my_test(name, parser, test_string, names=None):
    '''This function ensures that the parse-unparse and parse-repr-unparse
    operations are equivalent to the identity. Note that whitespace is not
    preserved by the parsing operation, so the test_string must conform to
    the whitespace conventions of the unparser for the test to succeed.'''
    from expression import Grouping, BinaryOperator, FunctionVar, Slicing

    pstr = parser.parseString(test_string)
    for item in pstr:
        print str(item)
    assert (str(pstr[0]) == test_string), "Failed to parse " + name + "."
    exec("pstr="+repr(pstr[0]))
    assert (str(pstr) == test_string), "Error in repr for " + name + "."
    if names:
        assert pstr.names == set(names), "Names do not match for " + name + "."


def test_function_calls():
    ''' Test parsing of expressions containing function calls '''
    my_test("function calls",
            VAR_OR_FUNCTION, "foo(bar(baz, bam), wibble(wub))",
            names=["foo", "bar", "baz", "bam", "wibble", "wub"])


def test_trivial_slice():
    ''' Test parsing of simplest possible array slice '''
    my_test("trivial slice", SLICING, ":")


def test_simple_slice():
    ''' Test parsing of array slice with range and stride '''
    my_test("simple slice", SLICING, "1:2:3")


def test_stride_slice():
    ''' Test parsing of slice consisting of stride only '''
    my_test("stride slice", SLICING, "::3")


def test_exponent():
    ''' Test parsing of exponents '''
    my_test("exponent", FORT_EXPRESSION, "2 ** 3 ** 4")


def test_plus_mult():
    ''' Test parsing of expression containing addition and multiplication '''
    my_test("plus mult",
            FORT_EXPRESSION,
            "f(x) + g(x, y) * 2",
            names=["f", "g", "x", "y"])


def test_group():
    ''' Test parsing of a grouped expression (i.e. within parentheses) '''
    my_test("group", FORT_EXPRESSION, "(x)", names="x")


def test_integer():
    ''' Test parsing of an integer '''
    my_test("integer with kind", FORT_EXPRESSION, "-500")


def test_integer_kind():
    ''' Test parsing of an integer with kind specified '''
    my_test("integer with kind", FORT_EXPRESSION, "5_i_def")


def test_integer_kind_digits():
    ''' Test parsing of an integer with kind specified using digits only'''
    my_test("integer with kind digits", FORT_EXPRESSION, "5_16")


def test_real():
    ''' Test parsing of a real scalar '''
    my_test("real", FORT_EXPRESSION, "-.5e-200")


def test_real_kind():
    ''' Test parsing of a real scalar with kind specified'''
    my_test("real with kind", FORT_EXPRESSION, "-.5e-200_r_def")


def test_real_no_exp_kind():
    ''' Test parsing of a real scalar with no exponent but with kind
    specified '''
    my_test("no exponent real with kind", FORT_EXPRESSION, "-.5_r_def")


def test_real_kind_digits():
    ''' Test parsing of a real scalar with kind specified using digits only'''
    my_test("real with kind", FORT_EXPRESSION, "-.5e-200_32")


def test_group_operations():
    ''' Test parsing of a group of operations '''
    my_test("group operations",
            FORT_EXPRESSION,
            "(f(x + 2 * y, z:z + 2 + -.5) + (g + h) ** (z - 2))",
            names=["f", "g", "h", "x", "y", "z"])
