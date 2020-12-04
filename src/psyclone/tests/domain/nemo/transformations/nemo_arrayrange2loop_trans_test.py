# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the NemoArrayRange2LoopTrans
transformation.'''

from __future__ import absolute_import

import pytest

import os
from psyclone.psyir.nodes import Literal, BinaryOperation, Reference, \
    Range, Array, Assignment, Node, DataNode, KernelSchedule
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ArrayType, \
    INTEGER_TYPE, REAL_TYPE
from psyclone.psyir.transformations import TransformationError
from psyclone.domain.nemo.transformations import NemoArrayRange2LoopTrans
from psyclone.domain.nemo.transformations.nemo_arrayrange2loop_trans import _get_outer_index
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import Compile

from psyclone.tests.utilities import get_invoke
from psyclone.nemo import NemoKern, NemoLoop
from psyclone.psyir.nodes import Schedule

# Constants
API = "nemo"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, "test_files")
TEST_CONFIG = os.path.join(BASE_PATH, "nemo_test.cfg")


def test_transform():
    '''Check that it is possible to create an instance of
    NemoArrayRange2LoopTrans and that it is a Transformation.

    '''
    assert NemoArrayRange2LoopTrans()
    assert isinstance(NemoArrayRange2LoopTrans(), Transformation)


@pytest.mark.parametrize("loop_vars_defined", [False, True])
def test_transform_apply_implicit_do(loop_vars_defined):
    '''Check that the PSyIR is transformed as expected for a lat,lon,levs
    loop with all of its indices accessed using array notation. Check
    that NemoLoops are created successfully and that they contain the
    appropriate loop_type information. Test with and without the loop
    variables (ji, jj, jk) being declared. Also check that a NemoKern
    node is added once the final array notation index is
    transformed. The resultant Fortran code is also checked to confirm
    that the transformation has worked correctly.

    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    if loop_vars_defined:
        # Declare the loop variables
        symbol_table = schedule.symbol_table
        for symbol_name in ["ji", "jj", "jk"]:
            symbol_table.add(DataSymbol(symbol_name, INTEGER_TYPE))
    assignment = schedule[0]
    trans = NemoArrayRange2LoopTrans()
    # Apply transformation 3 times, once for each implicit
    # dimension
    assert not schedule.walk(NemoKern)
    trans.apply(assignment)
    assert not schedule.walk(NemoKern)
    assert assignment.parent.parent.loop_type == "levels"
    trans.apply(assignment)
    assert not schedule.walk(NemoKern)
    assert assignment.parent.parent.loop_type == "lat"
    trans.apply(assignment)
    assert schedule.walk(NemoKern)
    assert isinstance(assignment.parent, Schedule)
    assert isinstance(assignment.parent.parent, NemoKern)
    assert isinstance(assignment.parent.parent.parent, Schedule)
    assert isinstance(assignment.parent.parent.parent.parent, NemoLoop)
    assert assignment.parent.parent.parent.parent.loop_type == "lon"
    writer = FortranWriter()
    result = writer(schedule)
    expected = (
        "do jk = 1, jpk, 1\n"
        "  do jj = 1, jpj, 1\n"
        "    do ji = 1, jpi, 1\n"
        "      umask(ji,jj,jk)=0.0e0\n"
        "    enddo\n"
        "  enddo\n"
        "enddo")
    assert expected in result


def test_transform_apply_mixed_implicit_do():
    '''Check that the PSyIR is transformed as expected for a lat,lon,levs
    loop with some of its indices accessed using array notation and
    some using explicit loops.  The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    _, invoke_info = get_invoke("explicit_over_implicit.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0].loop_body[0]
    trans = NemoArrayRange2LoopTrans()
    # Apply transformation 2 times, once for each implicit
    # dimension
    trans.apply(assignment)
    trans.apply(assignment)
    writer = FortranWriter()
    result = writer(schedule)
    expected = (
        "do jk = 1, jpk, 1\n"
        "  do jj = 1, jpj, 1\n"
        "    do ji = 1, jpi, 1\n"
        "      umask(ji,jj,jk)=vmask(ji,jj,jk) + 1.0\n"
        "    enddo\n"
        "  enddo\n"
        "enddo")
    assert expected in result


def test_implicit_do_many_dims():
    '''Check that the PSyIR is transformed as expected for a
    lat,lon,levs,tracer,unknown implicit loop. We expect the outermost
    loop to be ignored as it is not defined in the config file and the
    tracer loop to be ignored as it does not have upper bound
    information defined in the config file.  The resultant Fortran
    code is used to confirm the transformation has worked correctly.

    '''
    _, invoke_info = get_invoke("implicit_many_dims.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    trans = NemoArrayRange2LoopTrans()
    # Apply transformation 3 times, once for each dimension with valid
    # config information.
    trans.apply(assignment)
    trans.apply(assignment)
    trans.apply(assignment)
    writer = FortranWriter()
    result = writer(schedule)
    expected = (
        "do jk = 1, jpk, 1\n"
        "  do jj = 1, jpj, 1\n"
        "    do ji = 1, jpi, 1\n"
        "      umask(ji,jj,jk,:,:)=vmask(ji,jj,jk,:,:) + 1.0\n"
        "    enddo\n"
        "  enddo\n"
        "enddo")
    assert expected in result


def test_transform_apply_bound_names():
    '''Check that the PSyIR is transformed as expected for a lat,lon,levs
    loop with all of its indices accessed using array notation and
    with some of the lower bounds being symbols and some of the upper
    bounds being integers. The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    # Load a modified config file. This reverses the bounds
    # information in the i and k dimensions. Remove any existing
    # instance first.
    from psyclone.configuration import Config
    Config._instance = None
    _config = Config.get()
    _config.load(config_file=TEST_CONFIG)

    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    trans = NemoArrayRange2LoopTrans()
    # Apply transformation 3 times, once for each implicit
    # dimension
    trans.apply(assignment)
    trans.apply(assignment)
    trans.apply(assignment)

    # Remove the config instance so that its contents are not
    # accidentally used in other tests.
    Config._instance = None

    writer = FortranWriter()
    result = writer(schedule)
    expected = (
        "do jk = jpk, 1, 1\n"
        "  do jj = 1, jpj, 1\n"
        "    do ji = jpi, 1, 1\n"
        "      umask(ji,jj,jk)=0.0e0\n"
        "    enddo\n"
        "  enddo\n"
        "enddo")
    assert expected in result


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert("Error in NemoArrayRange2LoopTrans transformation. The supplied node "
           "argument should be a PSyIR Assignment, but found 'NoneType'."
           in str(info.value))


def test_str():
    '''Test that the str of an instance of the NemoArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert (str(NemoArrayRange2LoopTrans()) == "Convert a PSyIR assignment to "
            "an Array Range into a PSyIR NemoLoop.")


def test_name():
    '''Check that the name property of the ArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert NemoArrayRange2LoopTrans().name == "NemoArrayRange2LoopTrans"


def test_no_valid_index():
    '''Check that the validate method raises an exception if no valid array
    assignment is found.

    '''
    _, invoke_info = get_invoke("implicit_many_dims.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    trans = NemoArrayRange2LoopTrans()
    # Apply transformation 3 times, once for each implicit
    # dimension
    trans.apply(assignment)
    trans.apply(assignment)
    trans.apply(assignment)
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert ("Error in NemoArrayRange2LoopTrans: The lhs of the "
            "supplied Assignment node should be a PSyIR Array "
            "with at least one of its dimensions being a Range "
            "with a valid configuration file loop bound "
            "specification, but found None." in str(info.value))


def test_no_bounds_symbol_error():
    '''Check that the validate method raises an exception if the loop
    bounds are not already declared as symbols in the symbol table.

    '''
    _, invoke_info = get_invoke("implicit_do_undefined.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert ("Error in NemoArrayRange2LoopTrans: Loop bound symbol 'jpk' is "
            "not explicitly declared in the code." in str(info.value))


def test_get_outer_index():
    '''Check that _get_outer_index() returns the appropriate index of the
    array when a valid index is found and raises the apropriate
    exception when one is not.

    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    assert _get_outer_index(assignment) == 2
    trans = NemoArrayRange2LoopTrans()
    trans.apply(assignment)
    trans.apply(assignment)
    trans.apply(assignment)
    with pytest.raises(IndexError):
        _ = _get_outer_index(assignment)

