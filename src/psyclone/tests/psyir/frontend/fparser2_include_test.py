# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab


''' Performs py.test tests on the handling of INCLUDE statements in the
    fparser2 PSyIR front-end. '''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.common.sourceinfo import FortranFormat
from fparser.two import Fortran2003, C99Preprocessor
from fparser.two.utils import walk

from psyclone.configuration import Config
from psyclone.errors import GenerationError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


@pytest.mark.parametrize("unit", ["program", "module"])
@pytest.mark.parametrize("incl_files", [['some_header.fzz'],
                                        ['some_header.fzz', 'header2.fzz']])
def test_include_declns_abort(incl_files, parser, monkeypatch, unit):
    '''
    Check that we abort if one or more INCLUDEs are found in the specification
    part of a parse tree.

    '''
    processor = Fparser2Reader()
    code = f"{unit} my_prog\n"
    for incl_file in incl_files:
        code += f"include '{incl_file}'\n"
    code += (f"  integer :: i\n"
             f"end {unit} my_prog\n")
    reader = FortranStringReader(code)
    prog = parser(reader)
    # Double check that the parse tree is what we expect
    incl_list = walk(prog, Fortran2003.Include_Stmt)
    assert len(incl_list) == len(incl_files)
    # Now check that the frontend raises the expected error
    with pytest.raises(GenerationError) as err:
        processor.generate_psyir(prog)
    assert (f"Found an unresolved Fortran INCLUDE file '{incl_files[0]}' "
            f"while processing {unit} 'my_prog'. This "
            f"file must be made available by specifying its location "
            f"with a -I flag. (The list of directories to search is currently"
            f" set to: [].)" in str(err.value))
    # Check that any include path is correctly reported in the error message
    monkeypatch.setattr(Config.get(), "_include_paths", ["/road/to/nowhere"])
    with pytest.raises(GenerationError) as err:
        processor.generate_psyir(prog)
    assert ("with a -I flag. (The list of directories to search is currently"
            " set to: ['/road/to/nowhere'].)" in str(err.value))


@pytest.mark.parametrize("routine_type", ["subroutine", "program"])
def test_include_exec_part_abort(parser, monkeypatch, routine_type):
    '''
    Check that we abort if we encounter an INCLUDE statement in the execution
    part of the parse tree.

    '''
    processor = Fparser2Reader()
    code = (f"{routine_type} my_prog\n"
            f"  integer :: i\n"
            f"  i = 2\n"
            f"  include 'trouble.h'\n"
            f"end {routine_type} my_prog\n")
    reader = FortranStringReader(code)
    prog = parser(reader)
    # Double check that the parse tree is what we expect
    incl_list = walk(prog, Fortran2003.Include_Stmt)
    assert len(incl_list) == 1
    # Now check that the frontend raises the expected error
    with pytest.raises(GenerationError) as err:
        processor.generate_psyir(prog)
    rtype = "routine" if routine_type == "subroutine" else routine_type
    assert (f"Found an unresolved Fortran INCLUDE file 'trouble.h' while "
            f"processing {rtype} 'my_prog'. This file must be made available "
            f"by specifying its location with a -I flag. (The list of "
            f"directories to search is currently set to: [].)"
            in str(err.value))
    # Check that any include path is correctly reported in the error message
    monkeypatch.setattr(Config.get(), "_include_paths", ["/road/to",
                                                         "/nowhere"])
    with pytest.raises(GenerationError) as err:
        processor.generate_psyir(prog)
    assert ("This file must be made available by specifying its location with "
            "a -I flag. (The list of directories"
            " to search is currently set to: ['/road/to', '/nowhere'].)"
            in str(err.value))


def test_include_before_prog_abort(parser):
    '''
    Check that we abort with a suitable message when an INCLUDE is found
    outside a program unit.

    '''
    processor = Fparser2Reader()
    code = ("include 'trouble.h'\n"
            "program my_prog\n"
            "  integer :: i\n"
            "  i = 2\n"
            "end program my_prog\n")
    reader = FortranStringReader(code)
    prog = parser(reader)
    # Double check that the parse tree is what we expect
    incl_list = walk(prog, Fortran2003.Include_Stmt)
    assert len(incl_list) == 1
    with pytest.raises(GenerationError) as err:
        processor.generate_psyir(prog)
    assert ("Found an unresolved Fortran INCLUDE file 'trouble.h' while "
            "processing code:\n"
            "INCLUDE 'trouble.h'\n"
            "PROGRAM my_prog\n" in str(err.value))


@pytest.mark.parametrize("routine_type", ["program", "subroutine"])
def test_cpp_include_abort(parser, routine_type):
    '''
    Check that we abort if a CPP #include is encountered anywhere in the
    input code.

    '''
    unit_txt = "routine" if routine_type == "subroutine" else routine_type
    processor = Fparser2Reader()
    include = '#include "trouble.h"'
    code_lines = [f'{routine_type} my_prog',
                  '  integer :: i',
                  '  i = 2',
                  f'end {routine_type} my_prog']
    for idx in range(len(code_lines)):
        new_code = code_lines[:]
        new_code.insert(idx, include)
        full_txt = "\n".join(new_code)
        reader = FortranStringReader(full_txt)
        reader.set_format(FortranFormat(True, False))
        prog = parser(reader)
        # Double check that the parse tree is what we expect
        incl_list = walk(prog, C99Preprocessor.Cpp_Include_Stmt)
        assert len(incl_list) == 1
        with pytest.raises(GenerationError) as err:
            processor.generate_psyir(prog)
        if idx in [1, 2, 3]:
            assert (
                f"but found a #include of file 'trouble.h' while processing"
                f" {unit_txt} 'my_prog'" in str(err.value))
        else:
            assert ("but found a #include of file 'trouble.h' while processing"
                    " code:" in str(err.value))
