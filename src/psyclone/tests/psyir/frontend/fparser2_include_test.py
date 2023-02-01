# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
    fparser2 PSyIR front-end '''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.configuration import Config
from psyclone.errors import GenerationError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


@pytest.mark.parametrize("incl_files", [['some_header.fzz'],
                                        ['some_header.fzz', 'header2.fzz']])
def test_include_declns_abort(incl_files, parser, monkeypatch):
    '''
    Check that we abort if one or more INCLUDEs are found in the specification
    part of a parse tree.

    '''
    processor = Fparser2Reader()
    code = "program my_prog\n"
    for incl_file in incl_files:
        code += f"include '{incl_file}'\n"
    code += ("  integer :: i\n"
             "  i = 2\n"
             "end program my_prog\n")
    reader = FortranStringReader(code)
    prog = parser(reader)
    # Double check that the parse tree is what we expect
    incl_list = walk(prog, Fortran2003.Include_Stmt)
    assert len(incl_list) == len(incl_files)
    # Now check that the frontend raises the expected error
    with pytest.raises(GenerationError) as err:
        processor.generate_psyir(prog)
    assert (f"Fortran INCLUDE statements are not supported but found "
            f"include(s) for file(s): {incl_files}. These must be made "
            f"available to the Fortran parser by specifying their location(s) "
            f"via the -I flag. (The list of directories to search is currently"
            f" set to: [].)" in str(err.value))
    # Check that any include path is correctly reported in the error message
    monkeypatch.setattr(Config.get(), "_include_paths", ["/road/to/nowhere"])
    with pytest.raises(GenerationError) as err:
        processor.generate_psyir(prog)
    assert (f"Fortran INCLUDE statements are not supported but found "
            f"include(s) for file(s): {incl_files}. These must be made "
            f"available to the Fortran parser by specifying their location(s) "
            f"via the -I flag. (The list of directories to search is currently"
            f" set to: ['/road/to/nowhere'].)" in str(err.value))
