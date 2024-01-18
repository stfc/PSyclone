# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Modified: S. Siso and R. W. Ford, STFC Daresbury Lab
#           I. Kavcic, Met Office

'''Module containing pytest tests of the LFRicSetvalRandomKern
built-in.

'''

import os
from psyclone.domain.lfric.kernel import LFRicKernelMetadata
from psyclone.domain.lfric.lfric_builtins import LFRicSetvalRandomKern
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import IntrinsicCall, Loop
from psyclone.tests.lfric_build import LFRicBuild


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))),
    "test_files", "dynamo0p3")

# The PSyclone API under test
API = "dynamo0.3"


def test_setval_random(tmpdir):
    '''Test the 'str()' and 'metadata()' methods and generated code for
    the 'LFRicSetvalRandomKern' built-in.

    '''
    metadata = LFRicSetvalRandomKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.4_setval_random_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=True).create(invoke_info)

    # Test 'str()' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert isinstance(kern, LFRicSetvalRandomKern)
    assert (str(kern) ==
            "Built-in: setval_random (fill a real-valued field with "
            "pseudo-random numbers)")

    # Test code generation
    code = str(psy.gen)
    output = (
        "      DO df=loop0_start,loop0_stop\n"
        "        CALL RANDOM_NUMBER(f1_data(df))\n"
        "      END DO\n")
    assert output in code

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_setval_random_lowering(fortran_writer):
    '''Test that the 'lower_to_language_level()' method works as
    expected.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.4_setval_random_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    parent = kern.parent
    lowered = kern.lower_to_language_level()
    assert parent.children[0] is lowered
    assert isinstance(parent.children[0], IntrinsicCall)
    assert parent.children[0].routine.name == "RANDOM_NUMBER"

    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    assert ("do df = loop0_start, loop0_stop, 1\n"
            "  call RANDOM_NUMBER(f1_data(df))\n"
            "enddo") in code
