
''' Module containing tests for the Fortran expression parser '''

from __future__ import absolute_import
from psyclone.expression import VAR_OR_FUNCTION, FORT_EXPRESSION, SLICING


def my_test(name, parser, test_string, names=None):
    '''This function ensures that the parse-unparse and parse-repr-unparse
    operations are equivalent to the identity. Note that whitespace is not
    preserved by the parsing operation, so the test_string must conform to
    the whitespace conventions of the unparser for the test to succeed.'''
    # These imports are required in order for the exec in the code below
    # to work. Pylint complains about those as unused variables
    # pylint: disable=unused-variable
    from psyclone.expression import BinaryOperator, FunctionVar, Grouping, \
        LiteralArray, NamedArg, Slicing
    # pylint: enable=unused-variable
    pstr = parser.parseString(test_string)
    assert (str(pstr[0]) == test_string), "Failed to parse " + name + "."
    # ast.literal_eval can't be used here as the generated expression
    # calls constructors of user-defined objects
    # pylint: disable=exec-used
    exec("pstr="+repr(pstr[0]))
    # pylint: enable=exec-used
    assert (str(pstr) == test_string), "Error in repr for " + name + "."
    if names:
        assert pstr.names == set(names), "Names do not match for " + name + "."
    # Return the object created by the parser so that more specific checks
    # can be performed
    return pstr


def test_function_calls():
    ''' Test parsing of expressions containing function calls '''
    my_test("function calls",
            VAR_OR_FUNCTION, "foo(bar(baz, bam), wibble(wub))",
            names=["foo", "bar", "baz", "bam", "wibble", "wub"])


def test_no_args_function_call():
    ''' Test parsing of a function call with no arguments '''
    my_test("function call without args",
            VAR_OR_FUNCTION, "get_something()",
            names=["get_something"])


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


def test_literal_array():
    ''' Test parsing of a literal array '''
    my_test("literal array",
            FORT_EXPRESSION,
            "[1, 2, 3]")


def test_named_int_arg():
    ''' Test parsing of a named, integer argument '''
    my_test("named int arg", FORT_EXPRESSION, "my_arg=2")


def test_named_logical_arg():
    ''' Test parsing of a named, logical argument '''
    named_arg = my_test("named logical arg", FORT_EXPRESSION,
                        "my_arg=.true.", names=["my_arg"])
    assert named_arg.value == ".true."


def test_named_real_arg():
    ''' Test parsing of a named, real argument '''
    named_arg = my_test("named real arg", FORT_EXPRESSION, "my_arg=-2.0E3",
                        names=["my_arg"])
    assert named_arg.value == "-2.0E3"


def test_named_str_arg_single_quotes():
    ''' Test parsing a named string argument specified using single
    quotes '''
    named_arg = my_test("named arg single quotes", FORT_EXPRESSION,
                        "my_arg='funny'", names=["my_arg"])
    assert named_arg.value == "funny"


def test_named_str_arg_dble_quotes():
    ''' Test parsing a named string argument specified using double
    quotes '''
    named_arg = my_test("named arg double quotes", FORT_EXPRESSION,
                        'my_arg="funny"', names=["my_arg"])
    assert named_arg.value == "funny"


def test_named_str_arg_spaces():
    ''' Test parsing a named string argument where the value contains a
    space '''
    named_arg = my_test("named arg string with space", FORT_EXPRESSION,
                        'my_arg="very funny"', names=["my_arg"])
    assert named_arg.value == "very funny"


def test_fn_call_named_arg():
    ''' Test parsing of an expression containing a function call with a
    named argument '''
    info = my_test("Fn call with named arg", FORT_EXPRESSION,
                   "f(x, y=3, z='hello')", names=["f", "x", "y", "z"])
    assert info.args[1].value == "3"
    assert info.args[2].value == "hello"


def test_named_arg_variable():
    ''' Test the parsing of a named argument where the value is the name
    of a variable rather than a constant. '''
    named_arg = my_test("named arg variable", FORT_EXPRESSION,
                        "name=this_invoke", names=["name"])
    assert named_arg.value == "this_invoke"


def test_derived_type_deref():
    ''' Test parsing of reference to a component of a derived type '''
    my_test("ref. to derived-type component",
            FORT_EXPRESSION,
            "field%ndf")


def test_type_bound_call_no_args():
    ''' Test parsing of call to a type-bound routine '''
    my_test("call to type-bound routine",
            FORT_EXPRESSION,
            "field%get_ndf()")


def test_type_bound_call():
    ''' Test parsing of call to a type-bound routine which takes arguments '''
    my_test("call to type-bound routine",
            FORT_EXPRESSION,
            "field%get_ndf(a, b)")


def test_type_bound_call_deref_arg():
    ''' Test parsing of call to a type-bound routine which takes arguments,
    one of which is specified as the component of a derived type. '''
    my_test("call to type-bound routine with derived-type arg",
            FORT_EXPRESSION,
            "field%get_ndf(a, b%c)")


def test_type_bound_call_type_bound_arg():
    ''' Test parsing of call to a type-bound routine which takes arguments,
    one of which is specified as the result of a type-bound procedure call '''
    my_test("call to type-bound routine with type-bound arg",
            FORT_EXPRESSION,
            "field%get_ndf(a, b%c())")


def test_derived_type_deref_arg():
    ''' Test parsing of reference to a component of a derived type passed
    as an argument to a function call '''
    my_test("ref. to derived-type component",
            FORT_EXPRESSION,
            "get_colour_map(a, field%ndf)")


def test_type_bound_call_as_arg():
    ''' Test parsing of function call where one of the arguments is
    obtained from a call to a type-bound procedure '''
    my_test("ref. to derived-type component",
            FORT_EXPRESSION,
            "get_colour_map(a, field%get_ndf())")
