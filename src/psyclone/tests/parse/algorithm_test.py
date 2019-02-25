# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A module to perform pytest unit tests on the parse/algorithm.py
file. Some tests for this file are in parse_test.py. This file adds
tests for code that is not covered there.'''

import pytest
from psyclone.parse.algorithm import Parser, parse_fp2, get_invoke_label, \
    get_kernel, create_var_name, KernelCall, BuiltInCall, Arg
from psyclone.parse.utils import ParseError
from psyclone.psyGen import InternalError

# class parser() tests


def test_parser_parse(tmpdir):
    '''Test that if no relevant code is found in the algorithm file then
    the appropriate exception is raised.

    '''
    tmp = Parser()
    filename = str(tmpdir.join("empty.f90"))
    ffile = open(filename, "w")
    ffile.write("")
    ffile.close()
    with pytest.raises(ParseError) as excinfo:
        _ = tmp.parse(filename)
    assert ("Program, module, function or subroutine not found in parse tree "
            "for file") in str(excinfo.value)


def test_parser_createinvokecall():
    '''Test that if an argument to an invoke call is not what is expected
    then the appropriate exception is raised.

    '''
    from fparser.two.Fortran2003 import Call_Stmt
    from fparser.two.parser import ParserFactory
    _ = ParserFactory().create(std="f2003")
    statement = Call_Stmt("call invoke(0.0)")
    tmp = Parser()
    with pytest.raises(ParseError) as excinfo:
        _ = tmp.create_invoke_call(statement)
    assert (
        "Expecting argument to be of the form 'name=xxx' or a Kernel call "
        "but found '0.0' in file 'None'.") in str(excinfo.value)


def test_parser_updateargtomodulemap_invalid():
    '''Test that if the statement argument to the
    update_arg_to_module_map method is not a use statement that the
    appropriate exception is raised.'''
    tmp = Parser()
    with pytest.raises(InternalError) as excinfo:
        tmp.update_arg_to_module_map("invalid")
    assert ("Expected a use statement but found instance of '<type 'str'>"
            "'.") in str(excinfo.value)

# function parse_fp2() tests


def test_parsefp2_invalid_file(tmpdir):
    '''Test that if there is an error finding the input file specified in
    filename argument then an exception is raised in the expected way.

    '''
    with pytest.raises(ParseError) as excinfo:
        _ = parse_fp2(str(tmpdir.join("does_not_exist.f90")))
    assert ("Failed to parse file") in str(excinfo.value)
    assert ("Error returned was ' [Errno 2] No such file or directory") \
        in str(excinfo.value)


def test_parsefp2_invalid_fortran(tmpdir):
    '''Test that if the Fortran contained in the file specified in the
    filename argument then an exception is raised in the expected
    way. *** Create the parse_tree in-place rather than running
    PSyclone. Once created make the parse_tree content invalid using
    monkeypatch.

    '''
    my_file = str(tmpdir.join("invalid.f90"))
    ffile = open(my_file, "w")
    ffile.write("invalid Fortran code")
    ffile.close()
    with pytest.raises(ParseError) as excinfo:
        _ = parse_fp2(my_file)
    assert "Syntax error in file" in str(excinfo.value)


# function get_invoke_label() tests


def test_getinvokelabel_invalid_tree():
    '''Test that if the parse tree argument is not an Actual_Arg_Spec
    then an exception is raised in the expected
    way.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = get_invoke_label("invalid", "dummy.f90")
    assert (
        "Expected a Fortran argument of the form name=xxx but found instance "
        "of '<type 'str'>'.") in str(excinfo.value)


def test_getinvokelabel_invalid_items(monkeypatch):
    '''Test that if the parse tree argument is an Actual_Arg_Spec but does
    not contain two items then an exception is raised in the expected
    way. Create the parse_tree in-place rather than running
    PSyclone. Once created make the parse_tree content invalid using
    monkeypatch.

    '''
    from fparser.two.Fortran2003 import Actual_Arg_Spec
    from fparser.two.parser import ParserFactory
    _ = ParserFactory().create(std="f2003")
    parse_tree = Actual_Arg_Spec("name='myname'")
    monkeypatch.setattr(parse_tree, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        _ = get_invoke_label(parse_tree, "dummy.f90")
    assert (
        "Expected the Fortran argument to have two items but found "
        "'3'.") in str(excinfo.value)

# function get_kernel() tests


def test_getkernel_invalid_tree():
    '''Test that if the get_kernel function is passed an invalid
    parse tree argument, then it raises an exception
    in the expected way.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = get_kernel("invalid", "dummy.f90")
    assert (
        "Expected a parse tree (type Part_Ref) but found instance of "
        "'<type 'str'>'.") in str(excinfo.value)


def test_getkernel_invalid_arg(monkeypatch):
    '''Test that if the get_kernel function does not recognise the type of
    argument inside a kernel passed to it, then it raises an exception
    in the expected way. Create the parse_tree in-place rather than
    running PSyclone. Once created make the parse_tree content invalid
    using monkeypatch.

    '''
    from fparser.two.Fortran2003 import Part_Ref
    from fparser.two.parser import ParserFactory
    _ = ParserFactory().create(std="f2003")
    parse_tree = Part_Ref("kernel(arg)")
    monkeypatch.setattr(parse_tree, "items", [None, "invalid"])
    with pytest.raises(InternalError) as excinfo:
        _ = get_kernel(parse_tree, "dummy.f90")
    assert (
        "Unsupported argument structure '<type 'str'>', value 'invalid', "
        "kernel 'None(invalid)' in file 'dummy.f90'.") in str(excinfo.value)

# function create_var_name() tests


def test_createvarname_unknown_content():
    '''Test that if the create_var_name function does not recognise
    content within the parse tree passed to it, then it raises an
    exception in the expected way.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = create_var_name("invalid")
    assert "unrecognised structure '<type 'str'>'" in str(excinfo.value)
    
# class KernelCall() tests


def test_kernelcall_repr():
    '''Test that the __repr__ method in KernelCall() behaves as expected.'''

    class ktype_dummy():
        '''A fake KernelType class which provides the required variables to
        allow the BuiltInCall class to be instantiated and __repr__
        called.

        '''
        def __init__(self):
            self.nargs = 2
            self.name = "dummy"

    tmp = KernelCall("module_name", ktype_dummy(), ["a", "b"])
    assert repr(tmp) == "KernelCall('dummy', ['a', 'b'])"


# Class BuiltInCall() tests


def test_builtincall_repr():
    '''Test that the __repr__ method in BuiltInCall() behaves as expected.'''

    class ktype_dummy():
        '''A fake KernelType class which provides the required variables to
        allow the BuiltInCall class to be instantiated and __repr__
        called.

        '''
        def __init__(self):
            self.nargs = 2
            self.name = "dummy"

    tmp = BuiltInCall(ktype_dummy(), ["a", "b"])
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
        "'indexed_variable'] but found 'invalid'.") in str(excinfo.value)


def test_arg_str():
    '''Test that the __str__ method in Arg() behaves as expected'''

    # without the optional varname argument
    tmp = Arg("literal", "0.0")
    assert str(tmp) == "Arg(form='literal',text='0.0',varname='None')"
    
    # with the optional varname argument
    tmp = Arg("indexed_variable", "my_arg(2)", "my_arg")
    assert str(tmp) == ("Arg(form='indexed_variable',text='my_arg(2)',"
                        "varname='my_arg')")
