# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council
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
# Modified: I. Kavcic, Met Office

''' Tests for the gen_kernel_stub module. '''

import os
import pytest

import fparser

from psyclone.errors import GenerationError
from psyclone.gen_kernel_stub import generate
from psyclone.parse.algorithm import ParseError


def test_failures():
    '''Tests various failures of the generate() call.
    '''
    # Test empty API (and file not found)
    with pytest.raises(IOError) as err:
        generate("/does_not_exist", api="")
    assert "File '/does_not_exist' not found" in str(err.value)

    # Check invalid API
    with pytest.raises(GenerationError) as err:
        generate("filename", api="invalid")
    assert ("Error: Kernel stub generator: Unsupported API 'invalid' "
            "specified." in str(err.value))

    # Trapping Fortran errors:
    with pytest.raises(ParseError) as err:
        # Use this python file to trigger invalid Fortran
        generate(__file__, api="dynamo0.3")
    assert ("Kernel stub generator: Code appears to be invalid "
            "Fortran" in str(err.value))


def test_gen_success():
    ''' Test for successful completion of the generate() function. '''
    base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3")
    ptree = generate(os.path.join(base_path, "testkern_mod.F90"),
                     api="dynamo0.3")
    assert isinstance(ptree, fparser.one.block_statements.Module)
