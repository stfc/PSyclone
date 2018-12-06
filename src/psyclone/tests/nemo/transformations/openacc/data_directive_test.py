# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Module containing py.test tests for the transformation of the PSy
    representation of NEMO code using teh OpenACC data directive.

'''

from __future__ import print_function, absolute_import
import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, TransInfo, \
    GenerationError

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../../test_files")

def test_explicit():
    ''' Check code generation for a single explicit loop containing
    a kernel. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')

    schedule, _ = acc_trans.apply(schedule.children)
    gen_code = str(psy.gen)
    print (gen_code)
    assert ("  REAL, DIMENSION(jpi, jpj, jpk) :: umask\n"
            "  !$ACC DATA\n"
            "  DO jk = 1, jpk") in gen_code
    assert ("  END DO\n"
            "  !$ACC END DATA\n"
            "END PROGRAM explicit_do") in gen_code


# data directive correct location for directives round 1: single loop, multiple loops, not loops, ifs ...
# data directive child is a) directive b) loop c) not loop
# data directive parent is a) directive b) loop c) not loop
# data directive correct data captured
