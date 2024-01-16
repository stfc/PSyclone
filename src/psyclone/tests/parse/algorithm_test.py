# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''A module to perform pytest unit tests on the parse/algorithm.py
file. Some tests for this file are in parse_test.py. This file adds
tests for code that is not covered there.'''

import os

import pytest

from fparser.two.Fortran2003 import Part_Ref, Structure_Constructor, \
    Data_Ref, Proc_Component_Ref, Name, Call_Stmt, Use_Stmt, \
    Actual_Arg_Spec, Program
from fparser.two.parser import ParserFactory
from psyclone.parse.algorithm import Parser, get_invoke_label, \
    get_kernel, create_var_name, KernelCall, BuiltInCall, Arg, \
    parse, FileInfo
from psyclone.parse.utils import ParseError, parse_fp2
from psyclone.errors import InternalError


LFRIC_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               os.path.pardir, "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.path.pardir, "test_files", "gocean1p0")

# This ParserFactory call needs to happen at the top-level in order for the
# fparser.two.Fortran2003 import to work as expected.
ParserFactory().create(std="f2008")

# function parse() tests


def test_parse_kernel_paths():
    '''Check that the parse function behaves as expected with the
    kernel_paths argument.

    '''
    alg_name = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    # No argument
    parse(alg_name, api="dynamo0.3")
    # None argument
    parse(alg_name, api="dynamo0.3", kernel_paths=None)
    # Empty list
    parse(alg_name, api="dynamo0.3", kernel_paths=[])
    # Invalid path
    with pytest.raises(ParseError) as info:
        parse(alg_name, api="dynamo0.3", kernel_paths=["invalid"])
    assert ("Supplied kernel search path does not exist or cannot be read"
            in str(info.value))
    # Multiple kernel paths
    parse(alg_name, api="dynamo0.3", kernel_paths=[
        LFRIC_BASE_PATH, GOCEAN_BASE_PATH])

# class Parser() tests


def test_parser_init_kernel_paths():
    '''Check that the Parser class stores the kernel_paths optional
    argument as expected.

    '''
    # No argument
    parser = Parser()
    assert parser._kernel_paths == []
    # None argument
    parser = Parser(kernel_paths=None)
    assert parser._kernel_paths == []
    # Empty list
    parser = Parser(kernel_paths=[])
    assert parser._kernel_paths == []
    # Multiple kernel paths
    paths = [LFRIC_BASE_PATH, GOCEAN_BASE_PATH]
    parser = Parser(kernel_paths=paths)
    assert parser._kernel_paths == paths


# Parser.parse() method tests


def test_parser_parse_linelength():
    '''Check that the parse() method in the Parser() class raises an
    exception if one or more of the lines is too long (>132
    characters) in the supplied input file when _line_length is set to
    True and does not raise an exception by default.

    '''
    parser = Parser()
    parser.parse(os.path.join(LFRIC_BASE_PATH, "13_alg_long_line.f90"))

    parser = Parser(line_length=True)
    with pytest.raises(ParseError) as info:
        parser.parse(os.path.join(LFRIC_BASE_PATH, "13_alg_long_line.f90"))
    assert ("the file does not conform to the specified 132 line length "
            "limit" in str(info.value))


def test_parser_parse_nemo():
    '''Check that the parse() method in the Parser() class returns the
    expected results (None, and an fparser2 ast) when using the NEMO
    API. We actually use an LFRic algorithm file here but it does not
    matter as we are only parsing the code.

    '''
    parser = Parser(api="nemo")
    res1, res2 = parser.parse(os.path.join(
        LFRIC_BASE_PATH, "1_single_invoke.f90"))
    assert res1 is None
    assert isinstance(res2, Program)
    assert "PROGRAM single_invoke" in str(res2)


def test_parser_parse():
    '''Check that the parse() method in the Parser() class returns the
    expected results (fparser2 ast and a FileInfo instance) when using
    an API other than the NEMO API. Also test that the filename is
    stored in _alg_filename.

    '''
    parser = Parser(api="dynamo0.3")
    assert parser._alg_filename is None
    res1, res2 = parser.parse(os.path.join(
        LFRIC_BASE_PATH, "1_single_invoke.f90"))
    assert "1_single_invoke.f90" in parser._alg_filename
    assert isinstance(res1, Program)
    assert "PROGRAM single_invoke" in str(res1)
    assert isinstance(res2, FileInfo)
    assert res2.name == "single_invoke"

# Parser.invoke_info() method tests


def test_parser_invokeinfo_nocode(tmpdir):
    '''Check that the invoke_info() method in the Parser() class raises
    the expected exception if no relevant code (subroutine, module
    etc.) is found in the supplied fparser2 tree.

    '''
    parser = Parser()
    alg_filename = str(tmpdir.join("empty.f90"))
    with open(alg_filename, "w", encoding="utf-8") as ffile:
        ffile.write("")
    alg_parse_tree = parse_fp2(alg_filename)
    with pytest.raises(ParseError) as info:
        parser.invoke_info(alg_parse_tree)
    assert ("Program, module, function or subroutine not found in fparser2 "
            "parse tree.") in str(info.value)


def test_parser_invokeinfo_first(tmpdir):
    '''Check that the invoke_info() method in the Parser() class evaluates
    the first subroutine, module, etc if more than one exist in the
    supplied fparser2 tree.

    '''
    parser = Parser()
    alg_filename = str(tmpdir.join("two_routines.f90"))
    with open(alg_filename, "w", encoding="utf-8") as ffile:
        ffile.write(
            "subroutine first()\n"
            "end subroutine first\n"
            "subroutine second()\n"
            "end subroutine second\n")
    alg_parse_tree = parse_fp2(alg_filename)
    res = parser.invoke_info(alg_parse_tree)
    assert isinstance(res, FileInfo)
    assert res.name == "first"


@pytest.mark.parametrize("code,name", [
    ("program prog\nend program prog\n", "prog"),
    ("module mod\nend module mod\n", "mod"),
    ("subroutine sub()\nend subroutine sub\n", "sub"),
    ("function func()\nend function func\n", "func")])
def test_parser_invokeinfo_containers(tmpdir, code, name):
    '''Check that the invoke_info() method in the Parser() class works
    with program, module, subroutine and function.

    '''
    parser = Parser()
    alg_filename = str(tmpdir.join("container.f90"))
    with open(alg_filename, "w", encoding="utf-8") as ffile:
        ffile.write(code)
    alg_parse_tree = parse_fp2(alg_filename)
    res = parser.invoke_info(alg_parse_tree)
    assert isinstance(res, FileInfo)
    assert res.name == name


def test_parser_invokeinfo_datatypes():
    '''Test that the invoke_info method in the Parser class captures the
    required datatype information for "standard" fields, operators and
    scalars i.e. defined as field_type, operator_type and i_def
    respectively. We also capture the datatype of quadrature but don't
    care. field_type is actually a vector which shows that the code
    works with arrays as well as individual types.

    '''
    alg_filename = os.path.join(LFRIC_BASE_PATH, "10_operator.f90")
    parser = Parser(kernel_paths=[LFRIC_BASE_PATH])
    alg_parse_tree = parse_fp2(alg_filename)
    info = parser.invoke_info(alg_parse_tree)
    args = info.calls[0].kcalls[0].args
    assert args[0]._datatype == ("operator_type", None)
    assert args[1]._datatype == ("field_type", None)
    assert args[2]._datatype == ("integer", "i_def")
    assert args[3]._datatype == ("quadrature_xyoz_type", None)


def test_parser_invokeinfo_datatypes_mixed():
    '''Test that the 'invoke_info' method in the Parser class captures the
    required datatype information with mixed-precision fields, scalars
    and operators, e.g. defined as 'r_solver_field_type', 'r_solver'
    and 'r_solver_operator_type' respectively.

    Also tests that the datatype information is always lower case
    irrespective of the case of the declaration and argument. This
    covers the situation where the variable is declared and used with
    different case e.g. 'real a\n call invoke(kern(A))'.

    '''
    alg_filename = os.path.join(
        LFRIC_BASE_PATH, "26.8_mixed_precision_args.f90")
    parser = Parser(kernel_paths=[LFRIC_BASE_PATH])
    alg_parse_tree = parse_fp2(alg_filename)
    info = parser.invoke_info(alg_parse_tree)
    args0 = info.calls[0].kcalls[0].args
    args1 = info.calls[0].kcalls[1].args
    args2 = info.calls[0].kcalls[2].args
    args3 = info.calls[0].kcalls[3].args
    args4 = info.calls[0].kcalls[4].args
    assert args0[0]._datatype == ("real", "r_def")
    assert args0[1]._datatype == ("field_type", None)
    assert args0[2]._datatype == ("operator_type", None)
    assert args1[0]._datatype == ("real", "r_solver")
    assert args1[1]._datatype == ("r_solver_field_type", None)
    assert args1[2]._datatype == ("r_solver_operator_type", None)
    assert args2[0]._datatype == ("real", "r_tran")
    assert args2[1]._datatype == ("r_tran_field_type", None)
    assert args2[2]._datatype == ("r_tran_operator_type", None)
    assert args3[0]._datatype == ("real", "r_bl")
    assert args3[1]._datatype == ("r_bl_field_type", None)
    assert args4[0]._datatype == ("real", "r_phys")
    assert args4[1]._datatype == ("r_phys_field_type", None)


def test_parser_invokeinfo_datatypes_self():
    '''Test that the invoke_info method in the Parser class captures the
    required datatype information when the argument is part of a class
    and is referenced via self.

    '''
    alg_filename = os.path.join(
        LFRIC_BASE_PATH, "26.2_mixed_precision_self.f90")
    parser = Parser(kernel_paths=[LFRIC_BASE_PATH])
    alg_parse_tree = parse_fp2(alg_filename)
    info = parser.invoke_info(alg_parse_tree)
    args = info.calls[0].kcalls[0].args
    assert args[0]._datatype == ("r_solver_operator_type", None)
    assert args[1]._datatype == ("r_solver_field_type", None)
    assert args[2]._datatype == ("real", "r_solver")
    assert args[3]._datatype == ("quadrature_xyoz_type", None)


def test_parser_invokeinfo_use_error():
    '''Test that the invoke_info method in the Parser class provides None
    as the datatype to the associated Arg class if an argument to an
    invoke comes from a use statement (as we then do not know its
    datatype). Also check for the same behaviour if the variable is
    not declared at all i.e. is included via a wildcard use statement,
    or implicit none is not specified.

    '''
    alg_filename = os.path.join(
        LFRIC_BASE_PATH, "26.4_mixed_precision_use.f90")
    parser = Parser(kernel_paths=[LFRIC_BASE_PATH])
    alg_parse_tree = parse_fp2(alg_filename)
    info = parser.invoke_info(alg_parse_tree)
    args = info.calls[0].kcalls[0].args
    assert args[0]._datatype is None
    assert args[1]._datatype == ("r_solver_field_type", None)
    assert args[2]._datatype is None
    assert args[3]._datatype == ("quadrature_xyoz_type", None)


def test_parser_invokeinfo_structure_error():
    '''Test that the invoke_info method in the Parser class provides None
    as the datatype to the associated Arg class if an argument to an
    invoke is a structure that comes from a use statement (as we then
    do not know its datatype), but that the datatype for a structure
    is found if the structure is declared within the code.

    '''
    alg_filename = os.path.join(
        LFRIC_BASE_PATH, "26.5_mixed_precision_structure.f90")
    parser = Parser(kernel_paths=[LFRIC_BASE_PATH])
    alg_parse_tree = parse_fp2(alg_filename)
    info = parser.invoke_info(alg_parse_tree)
    args = info.calls[0].kcalls[0].args
    assert args[0]._datatype is None
    assert args[1]._datatype == ("r_solver_field_type", None)
    assert args[2]._datatype == ("real", "r_def")
    assert args[3]._datatype == ("quadrature_xyoz_type", None)


def test_parser_invokeinfo_internalerror():
    '''Test that the invoke_info method in the Parser class raises the
    expected exception if an unexpected child of Type_Declaration_Stmt
    or Data_Component_Def_Stmt is found.

    '''
    alg_filename = os.path.join(
        LFRIC_BASE_PATH, "26.1_mixed_precision.f90")
    parser = Parser(kernel_paths=[LFRIC_BASE_PATH])
    alg_parse_tree = parse_fp2(alg_filename)
    # Modify parse tree to make it invalid
    alg_parse_tree.children[0].children[1].children[5].items = ["hello"]
    with pytest.raises(InternalError) as info:
        parser.invoke_info(alg_parse_tree)
    assert (
        "Expected first child of Type_Declaration_Stmt or "
        "Data_Component_Def_Stmt to be Declaration_Type_Spec or "
        "Intrinsic_Type_Spec but found 'str'" in str(info.value))


def test_parser_invokeinfo_datatypes_clash():
    '''Test that the invoke_info method in the Parser class allows
    multiple symbols with the same name and type but raises an
    exception if a symbol has the same name but a different type. This
    is simply a limitation of the current implementation as we do not
    capture the context of a symbol so do not deal with variable
    scope. This limitation will disapear when the PSyIR is used to
    determine datatypes, see issue #753.

    '''
    alg_filename = os.path.join(
        LFRIC_BASE_PATH, "26.3_mixed_precision_error.f90")
    parser = Parser(kernel_paths=[LFRIC_BASE_PATH])
    alg_parse_tree = parse_fp2(alg_filename)
    with pytest.raises(NotImplementedError) as info:
        parser.invoke_info(alg_parse_tree)
    assert ("The same symbol 'a' is used for different datatypes, 'real, "
            "r_solver' and 'real, None'. This is not currently supported."
            in str(info.value))


def test_parser_createinvokecall():
    '''Test that valid invoke calls are created without an
    exception. Limit this to builtins as kernel calls fail as there is
    no associated use statement declared. Test with names, real
    scalars, integer scalars and structure references, including ones
    to self, as the parser represents these in different ways so the
    create_invoke_call needs to deal with the different
    representations.

    '''
    statement = Call_Stmt(
        "call invoke(name=\"dummy\", setval_c(a,1.0), setval_c(a,1), "
        "setval_c(a,b), setval_c(a%c, b), setval_c(self%a, 1.0), "
        "setval_c(self%a, b))")
    parser = Parser()
    _ = parser.create_invoke_call(statement)


def test_parser_createinvokecall_error():
    '''Test that if an argument to an invoke call is not what is expected
    then the appropriate exception is raised.

    '''
    statement = Call_Stmt("call invoke(0.0)")
    tmp = Parser()
    with pytest.raises(ParseError) as excinfo:
        _ = tmp.create_invoke_call(statement)
    assert (
        "Expecting argument to be of the form 'name=xxx' or a Kernel call "
        "but found '0.0' in file 'None'.") in str(excinfo.value)

# create_coded_kernel_call tests


def test_parser_codedkernelcall_kernel_paths():
    '''Check that the Parser class passes the kernel_paths information
    through to the get_kernel_ast() function from the
    coded_kernel_call() method.

    '''
    alg_filename = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    parse_tree = parse_fp2(alg_filename)
    invoke_call = parse_tree.children[0].children[2].children[0]
    invoke_argument = invoke_call.children[1].children[0]
    kernel_name, args = get_kernel(invoke_argument, alg_filename, {})
    parser = Parser()
    parser._alg_filename = alg_filename
    use_statement = parse_tree.children[0].children[1].children[2]
    parser.update_arg_to_module_map(use_statement)
    # No paths
    parser.create_coded_kernel_call(kernel_name, args)
    # Invalid kernel path
    parser._kernel_paths = ["invalid"]
    with pytest.raises(ParseError) as info:
        parser.create_coded_kernel_call(kernel_name, args)
    assert ("Supplied kernel search path does not exist or cannot be read"
            in str(info.value))
    # Multiple kernel paths
    paths = [LFRIC_BASE_PATH, GOCEAN_BASE_PATH]
    parser._kernel_paths = paths
    parser.create_coded_kernel_call(kernel_name, args)


def test_parser_updateargtomodulemap_invalid():
    '''Test that if the statement argument to the update_arg_to_module_map
    method is not a use statement that the appropriate exception is
    raised.

    '''
    tmp = Parser()
    with pytest.raises(InternalError) as excinfo:
        tmp.update_arg_to_module_map("invalid")
    assert "Expected a use statement but found instance of" \
        in str(excinfo.value)


def test_parser_caseinsensitive1():
    '''Check that the test for the existance of a builtin call in a use
    statement is case insensitive.

    '''
    parser = Parser()
    use = Use_Stmt("use my_mod, only : SETVAL_X")
    parser.update_arg_to_module_map(use)
    with pytest.raises(ParseError) as excinfo:
        parser.create_builtin_kernel_call("SetVal_X", None)
    assert "A built-in cannot be named in a use statement" \
        in str(excinfo.value)


def test_parser_caseinsensitive2(monkeypatch):
    '''Check that the test for the existance of a kernel call in a use
    statement is case insensitive.

    '''
    parser = Parser()
    use = Use_Stmt("use testkern_mod, only : TESTKERN_TYPE")
    parser.update_arg_to_module_map(use)

    def dummy_func(arg1, arg2, arg3, arg4):
        '''A dummy function used by monkeypatch to override the get_kernel_ast
        function. We don't care about the arguments as we just want to
        raise an exception.

        '''
        raise NotImplementedError("test_parser_caseinsensitive2")
    monkeypatch.setattr("psyclone.parse.algorithm.get_kernel_ast", dummy_func)
    with pytest.raises(NotImplementedError) as excinfo:
        # We have monkeypatched the function 'get_kernel_ast' to
        # return 'NotImplementedError' with a string associated with
        # this test so we know that we have got to this function if
        # this exception is raised. The case insensitive test we
        # really care about is before this function is called (and it
        # raises a ParseError) so we know that if we don't get a
        # ParseError then all is well.
        parser.create_coded_kernel_call("TestKern_Type", None)
    # Sanity check that the exception is the monkeypatched one.
    assert str(excinfo.value) == "test_parser_caseinsensitive2"

# function get_invoke_label() tests


def test_getinvokelabel_lowercase():
    '''Test that 'get_invoke_label' converts to lowercase.'''
    parse_tree = Actual_Arg_Spec("name='HeartOfGold'")
    label = get_invoke_label(parse_tree, "dummy.f90")
    assert label == "heartofgold"


def test_getinvokelabel_invalid_tree():
    '''Test that if the parse tree argument is not an Actual_Arg_Spec then
    an exception is raised in the expected way.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = get_invoke_label("invalid", "dummy.f90")
    assert (
        "Expected a Fortran argument of the form name=xxx but found instance "
        "of") in str(excinfo.value)


def test_getinvokelabel_invalid_items(monkeypatch):
    '''Test that if the parse tree argument is an Actual_Arg_Spec but does
    not contain two items then an exception is raised in the expected
    way. Create the parse_tree in-place rather than running
    PSyclone. Once created make the parse_tree content invalid using
    monkeypatch.

    '''
    parse_tree = Actual_Arg_Spec("name='myname'")
    monkeypatch.setattr(parse_tree, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        _ = get_invoke_label(parse_tree, "dummy.f90")
    assert (
        "Expected the Fortran argument to have two items but found "
        "'3'.") in str(excinfo.value)


def test_getinvokelabel_whitespace():
    '''Test that an invoke label containing whitespace is rejected.'''
    parse_tree = Actual_Arg_Spec("name='my name'")
    with pytest.raises(ParseError) as err:
        _ = get_invoke_label(parse_tree, "dummy.f90")
    assert ("get_invoke_label the (optional) name of an invoke must be a "
            "string containing a valid Fortran name (with no whitespace) but "
            "got 'my name' in file dummy.f90" in str(err.value))

# function get_kernel() tests


def test_getkernel_invalid_tree():
    '''Test that if the get_kernel function is passed an invalid parse
    tree argument, then it raises an exception in the expected way.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = get_kernel("invalid", "dummy.f90", None)
    assert (
        "Expected a parse tree (type Part_Ref or Structure_Constructor) but "
        "found instance of ") in str(excinfo.value)


@pytest.mark.parametrize("cls", [Part_Ref, Structure_Constructor])
def test_getkernel_invalid_children(cls, monkeypatch):
    '''Test that if the get_kernel function finds Part_Ref or
    Structure_Constructor as the top level of the parse tree but this
    does not have two children then it raises an exception in the
    expected way. Create the parse_tree in-place rather than running
    PSyclone. Once created make the parse_tree content invalid using
    monkeypatch.

    '''
    parse_tree = cls("kernel(arg)")
    monkeypatch.setattr(parse_tree, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        _ = get_kernel(parse_tree, "dummy.f90", None)
    assert ("Expected Part_Ref or Structure_Constructor to have 2 children "
            "but found 3.") in str(excinfo.value)


def test_getkernel_invalid_arg(monkeypatch):
    '''Test that if the get_kernel function does not recognise the type of
    argument inside a kernel passed to it, then it raises an exception
    in the expected way. Create the parse_tree in-place rather than
    running PSyclone. Once created make the parse_tree content invalid
    using monkeypatch.

    '''
    parse_tree = Part_Ref("kernel(arg)")
    monkeypatch.setattr(parse_tree, "items", [None, "invalid"])
    with pytest.raises(InternalError) as excinfo:
        _ = get_kernel(parse_tree, "dummy.f90", None)
    assert (
        "Unsupported argument structure") in str(excinfo.value)
    assert (
        "value 'invalid', kernel 'None(invalid)' in file 'dummy.f90'.") \
        in str(excinfo.value)


@pytest.mark.parametrize('content,datatype',
                         [("1.0", ("real", None)),
                          ("1.0_r_def", ("real", "r_def")),
                          ("1", ("integer", None)),
                          ("1_i_def", ("integer", "i_def"))])
def test_getkernel_isliteral(content, datatype):
    '''Test that the get_kernel function recognises the possible forms of
    literal argument and returns them correctly.

    '''
    tree = Structure_Constructor(f"sub({content})")
    kern_name, args = get_kernel(tree, "dummy.f90", {})
    assert kern_name == "sub"
    assert len(args) == 1
    arg = args[0]
    assert isinstance(arg, Arg)
    assert arg.is_literal()
    assert arg.text == content
    assert arg.varname is None
    assert arg._datatype == datatype


@pytest.mark.parametrize('content,datatype', [
    ("- 1.0", ("real", None)), ("- 1", ("integer", None)),
    ("1.0 * 1.0", ("real", None)),
    ("(1_i_def * 1_i_def)", ("integer", "i_def")),
    ("(1.0_r_solver * 1.0_r_solver)", ("real", "r_solver")),
    ("(1.0_r_tran * 1.0_r_tran)", ("real", "r_tran")),
    ("(1.0_r_def + 2.0_r_def) * 2.0_r_def", ("real", "r_def"))])
def test_getkernel_isliteral_expr(content, datatype):
    '''Test that the get_kernel function recognises the possible forms of
    literal expression and returns them correctly.

    '''
    tree = Structure_Constructor(f"sub({content})")
    kern_name, args = get_kernel(tree, "dummy.f90", {})
    assert kern_name == "sub"
    assert len(args) == 1
    arg = args[0]
    assert isinstance(arg, Arg)
    assert arg.is_literal()
    assert arg.text == content
    assert arg.varname is None
    assert arg._datatype == datatype


def test_getkernel_isliteral_expr_error():
    '''Test that the get_kernel function raises the expected exception if
    the literal expression contains a mix of different literal
    datatypes.

    '''
    tree = Structure_Constructor("sub(1.0_r_def*1.0_r_solver)")
    with pytest.raises(NotImplementedError) as info:
        _, _ = get_kernel(tree, "dummy.f90", {})
    assert ("Found two non-matching literals within an expression "
            "('1.0_r_def * 1.0_r_solver') passed into an invoke from the "
            "algorithm layer. '('real', 'r_solver')' and '('real', 'r_def')' "
            "do not match. This is not supported in PSyclone."
            in str(info.value))


@pytest.mark.parametrize('content',
                         ["a_rg", "a_rg(n)", "a % rg", "a % rg(n)",
                          "a % rg()"])
def test_getkernel_isarg(content):
    '''Test that the get_kernel function recognises standard arguments and
    returns them correctly. Tests for Name, Part_Ref, Data_Ref and
    Function_Reference, but does not include Proc_Component_Ref
    (i.e. an argument to a Structure Constructor) which is tested
    separately in test_getkernel_proc_component.

    '''
    tree = Part_Ref(f"sub({content})")
    kern_name, args = get_kernel(tree, "dummy.f90", {})
    assert kern_name == "sub"
    assert len(args) == 1
    arg = args[0]
    assert isinstance(arg, Arg)
    assert not arg.is_literal()
    assert arg.text == content
    assert arg.varname == "a_rg"
    assert arg._datatype is None
    kern_name, args = get_kernel(
        tree, "dummy.f90", {"a_rg": ("info"), "rg": ("info")})
    arg = args[0]
    if content == "a % rg()":
        # Datatype information is not captured for function references
        assert arg._datatype is None
    else:
        assert arg._datatype == ("info")


@pytest.mark.parametrize('content', ["self % a_b", "self % a % b"])
def test_getkernel_proc_component(content):
    '''Test that the get_kernel function recognises procedure components -
    Proc_Component_Ref - (within a structure constructor) and returns
    them correctly. The real literal in the call forces fparser to
    treat the argument to the call as a structure constructor. Note,
    if we make the rhs an array access (e.g. self%a(1)), fparser no
    longer treats it as a structure constructor.

    '''
    tree = Call_Stmt(f"call x(y({content}, 1.0))")
    kernel = tree.children[1].children[0]
    kern_name, args = get_kernel(kernel, "dummy.f90", {})
    assert kern_name == "y"
    assert len(args) == 2
    arg = args[0]
    assert isinstance(arg, Arg)
    assert not arg.is_literal()
    assert arg.text == content
    assert arg.varname == "self_a_b"
    assert arg._datatype is None
    kern_name, args = get_kernel(
        kernel, "dummy.f90", {"a_b": ("info"), "b": ("info")})
    arg = args[0]
    assert arg._datatype == ("info")


def test_getkernel_proc_component_data_ref():
    '''Test that the get_kernel function recognises a complex datatype
    that could potentially be a collection (but is not).

    '''
    tree = Call_Stmt("call x(y(self%vec_type(1)%vector(1)))")
    kernel = tree.children[1].children[0]
    kern_name, args = get_kernel(kernel, "dummy.f90", {})
    assert kern_name == "y"
    assert len(args) == 1
    assert isinstance(args[0], Arg)
    assert args[0].form == "variable"
    assert args[0].is_literal() is False
    assert args[0].text == "self % vec_type(1) % vector(1)"
    assert args[0].varname == "self_vec_type_vector"


def test_getkernel_proc_component_collection():
    '''Test that the get_kernel function recognises a collection and
    returns it in the expected way.

    '''
    parser = Parser(api="dynamo0.3")
    _, info = parser.parse(os.path.join(
        LFRIC_BASE_PATH, "1.6.2_single_invoke_1_int_from_derived_type.f90"))
    collection_arg = info.calls[0].kcalls[0].args[1]
    assert isinstance(collection_arg, Arg)
    assert collection_arg.form == "collection"
    assert collection_arg.is_literal() is False
    assert collection_arg.text == "my_obj % iflag"
    assert collection_arg.varname == "my_obj_iflag"


def test_getkernel_proccomponent_error():
    '''Check that the expected exception is raised if the 3rd child of a
    Proc_Component_Ref is not of type Name. In theory this should be
    possible but in practice fparser2 does not generate a parse tree
    in this form. We therefore need to manually modify the tree to
    raise this exception.

    '''
    tree = Call_Stmt("call x(y(self%a, 1.0))")
    kernel = tree.children[1].children[0]
    proc_comp_ref = kernel.children[1].children[0]
    # Modify tree
    proc_comp_ref.items = (proc_comp_ref.items[0],
                           proc_comp_ref.items[1], "hello")
    with pytest.raises(InternalError) as info:
        get_kernel(kernel, "dummy.f90", {})
    assert ("The third argument to to a Proc_Component_Ref is expected "
            "to be a Name, but found 'str'." in str(info.value))


def test_getkernel_dataref_error():
    '''Check that the expected exception is raised if the last child of a
    Data_Ref is not a Name or a Part_Ref whose first child is not a
    Name. fparser2 does not generate a parse tree in this form. We
    therefore need to manually modify the tree to raise this
    exception.

    '''
    tree = Call_Stmt("call x(y(self%a))")
    kernel = tree.children[1].children[0]
    data_ref = kernel.children[1].children[0]
    # Modify tree
    data_ref.items = (data_ref.items[0], "hello")
    with pytest.raises(InternalError) as info:
        get_kernel(kernel, "dummy.f90", {})
    assert ("The last child of a Data_Ref is expected to be a Name or a "
            "Part_Ref whose first child is a Name, but found 'str'."
            in str(info.value))


@pytest.mark.parametrize('content',
                         ["- arg", "1.0 * arg", "(1.0 * arg)",
                          "1.0 * (1.0 * arg)", "arg1*arg2"])
def test_getkernel_noexpr(content):
    '''Test that the get_kernel function recognises an expression
    containing a variable and raises an exception (as this is not
    currently supported).

    '''
    tree = Part_Ref(f"sub({content})")
    with pytest.raises(NotImplementedError) as excinfo:
        _, _ = get_kernel(tree, "dummy.f90", None)
    assert "Expressions containing variables are not yet supported" \
        in str(excinfo.value)


def test_getkernel_argerror(monkeypatch):
    '''Test that the get_kernel function raises an exception if it does
    not recognise the fparser2 parse tree for an argument.

    '''
    tree = Part_Ref("sub(dummy)")
    monkeypatch.setattr(tree, "items", ["sub", None])
    with pytest.raises(InternalError) as excinfo:
        _, _ = get_kernel(tree, "dummy.f90", None)
    assert "Unsupported argument structure " in str(excinfo.value)

# function create_var_name() tests


def test_createvarname_error1():
    '''Test that if the create_var_name function does not recognise
    content within the parse tree passed to it, then it raises an
    exception in the expected way.

    '''
    name = "class"
    with pytest.raises(InternalError) as excinfo:
        _ = create_var_name("invalid")
    assert (f"algorithm.py:create_var_name unrecognised structure "
            f"'<{name} 'str'>'" in str(excinfo.value))


def test_createvarname_error2(monkeypatch):
    '''Test that if the create_var_name function does not recognise
    content within a Data_Ref passed to it, then it raises an
    exception in the expected way.

    '''
    name = "class"
    content = Data_Ref("a%b")
    monkeypatch.setattr(content, "items", ["invalid", "invalid"])
    with pytest.raises(InternalError) as excinfo:
        _ = create_var_name(content)
    assert (f"algorithm.py:create_var_name unrecognised structure "
            f"'<{name} 'str'>' in '<class 'fparser.two.Fortran2003."
            f"Data_Ref'>'" in str(excinfo.value))


@pytest.mark.parametrize("expression,expected", [
    (Name("a"), "a"), (Part_Ref("a"), "a"), (Part_Ref("a(1,n)"), "a"),
    (Data_Ref("c%b%a"), "c_b_a"), (Data_Ref("c(0)%b%a(1,n)"), "c_b_a"),
    (Proc_Component_Ref("self%a"), "self_a")])
def test_createvarname(expression, expected):
    '''Test that create var name works as expected (for cases with Name,
    Part_Ref, Data_Ref and Proc_Component_Ref).

    '''
    assert create_var_name(expression) == expected

# class KernelCall() tests


def test_kernelcall_repr():
    '''Test that the __repr__ method in KernelCall() behaves as
    expected.

    '''

    class KtypeDummy():
        '''A fake KernelType class which provides the required variables to
        allow the BuiltInCall class to be instantiated and __repr__
        called.

        '''
        def __init__(self):
            self.nargs = 2
            self.name = "dummy"

    tmp = KernelCall("module_name", KtypeDummy(), ["a", "b"])
    assert repr(tmp) == "KernelCall('dummy', ['a', 'b'])"


# Class BuiltInCall() tests


def test_builtincall_repr():
    '''Test that the __repr__ method in BuiltInCall() behaves as
    expected.

    '''

    class KtypeDummy():
        '''A fake KernelType class which provides the required variables to
        allow the BuiltInCall class to be instantiated and __repr__
        called.

        '''
        def __init__(self):
            self.nargs = 2
            self.name = "dummy"

    tmp = BuiltInCall(KtypeDummy(), ["a", "b"])
    assert repr(tmp) == "BuiltInCall('dummy', ['a', 'b'])"

# Class Arg() tests


def test_arg_unknown():
    '''Test that an exception is raised correctly if an invalid arg type
    is provided to the Arg() form argument.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = Arg("invalid", "0.0")
    assert (
        "Unknown arg type provided. Expected one of ['literal', 'variable', "
        "'indexed_variable', 'collection'] but found 'invalid'."
        in str(excinfo.value))


def test_arg_str():
    '''Test that the __str__ method in Arg() behaves as expected.'''

    # without the optional varname argument
    tmp = Arg("literal", "0.0")
    assert str(tmp) == "Arg(form='literal',text='0.0',varname='None')"

    # with the optional varname argument
    tmp = Arg("indexed_variable", "my_arg(2)", "my_arg")
    assert str(tmp) == ("Arg(form='indexed_variable',text='my_arg(2)',"
                        "varname='my_arg')")


def test_arg_datatype():
    '''Check that the datatype argument defaults to None and that any
    datatype content is stored correctly.

    '''
    tmp = Arg("literal", "0.0")
    assert tmp._datatype is None
    tmp = Arg("variable", "var", varname="var", datatype=("info"))
    assert tmp._datatype == ("info")
