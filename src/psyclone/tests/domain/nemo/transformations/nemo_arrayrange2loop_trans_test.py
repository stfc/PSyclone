# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab

'''Module containing tests for the NemoArrayRange2LoopTrans
transformation.'''

from __future__ import absolute_import

import os
import pytest

from psyclone.psyir.nodes import Assignment
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE, \
    ArrayType, DeferredType
from psyclone.psyir.transformations import TransformationError
from psyclone.domain.nemo.transformations import NemoArrayRange2LoopTrans
from psyclone.domain.nemo.transformations.nemo_arrayrange2loop_trans \
    import get_outer_index
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import get_invoke, Compile
from psyclone.nemo import NemoKern, NemoLoop
from psyclone.psyir.nodes import Schedule
from psyclone.errors import InternalError
from psyclone.configuration import Config

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


def test_apply_bounds(tmpdir):
    '''Check that the apply method uses a) configuration bounds if they
    are provided or b) lbound and ubound intrinsics when no bounds
    information is available. Also check that a NemoKern is added
    between the assignment and the innermost enclosing loop after the
    last range has been transformed into an explicit loop.

    '''
    _, invoke_info = get_invoke("implicit_many_dims.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    for index in range(4, -1, -1):
        assert not schedule.walk(NemoKern)
        range_node = array_ref.children[index]
        trans.apply(range_node)
    assert schedule.walk(NemoKern)
    assert isinstance(assignment.parent, Schedule)
    assert isinstance(assignment.parent.parent, NemoKern)
    assert isinstance(assignment.parent.parent.parent, Schedule)
    assert isinstance(assignment.parent.parent.parent.parent, NemoLoop)
    assert assignment.parent.parent.parent.parent.loop_type == "lon"
    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  do idx = LBOUND(umask, 5), UBOUND(umask, 5), 1\n"
        "    do jt = 1, UBOUND(umask, 4), 1\n"
        "      do jk = 1, jpk, 1\n"
        "        do jj = 1, jpj, 1\n"
        "          do ji = 1, jpi, 1\n"
        "            umask(ji,jj,jk,jt,idx) = vmask(ji,jj,jk,jt,idx) + 1.0\n"
        "          enddo\n"
        "        enddo\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_fixed_bounds(tmpdir):
    '''Check that the apply method uses bounds information from the range
    node if it is supplied.

    '''
    _, invoke_info = get_invoke("implicit_do_slice.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    for index in range(2, -1, -1):
        range_node = array_ref.children[index]
        trans.apply(range_node)
    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  do jk = 1, jpk, 1\n"
        "    do jj = 2, 4, 1\n"
        "      do ji = 1, jpi, 1\n"
        "        umask(ji,jj,jk) = 0.0d0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_reference_literal(tmpdir):
    '''Check that the apply method add bounds appropriately when the
    config file specifies a lower bound as a reference and an upper
    bound as a literal.

    '''
    _, invoke_info = get_invoke("implicit_many_dims.f90", api=API, idx=0)
    # Create a new config instance and load a test config file with
    # the bounds information set the way we want.
    config = Config.get(do_not_load_file=True)
    config.load(config_file=TEST_CONFIG)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    for index in range(4, -1, -1):
        range_node = array_ref.children[index]
        trans.apply(range_node)
    # Remove this config file so the next time the default one will be
    # loaded (in case we affect other tests)
    Config._instance = None
    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  do idx = LBOUND(umask, 5), UBOUND(umask, 5), 1\n"
        "    do jt = 1, UBOUND(umask, 4), 1\n"
        "      do jk = jpk, 1, 1\n"
        "        do jj = 1, jpj, 1\n"
        "          do ji = jpi, 1, 1\n"
        "            umask(ji,jj,jk,jt,idx) = vmask(ji,jj,jk,jt,idx) + 1.0\n"
        "          enddo\n"
        "        enddo\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_different_dims(tmpdir):
    '''Check that the apply method adds loop iterators appropriately when
    the range dimensions differ in different arrays.

    '''
    _, invoke_info = get_invoke("implicit_different_dims.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    for index in [4, 2, 0]:
        range_node = array_ref.children[index]
        trans.apply(range_node)
    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  do idx = LBOUND(umask, 5), UBOUND(umask, 5), 1\n"
        "    do jk = 1, jpk, 1\n"
        "      do ji = 1, jpi, 1\n"
        "        umask(ji,jpj,jk,ndim,idx) = vmask(jpi,ji,jk,idx,ndim) + 1.0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_var_name(tmpdir):
    '''Check that the variable name that is used when no names are
    specified in the config file does not clash with an existing symbol.

    '''
    _, invoke_info = get_invoke("implicit_many_dims.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    symbol_table = schedule.symbol_table
    symbol_table.add(DataSymbol("idx", INTEGER_TYPE))
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    range_node = array_ref.children[4]
    trans.apply(range_node)
    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  do idx_1 = LBOUND(umask, 5), UBOUND(umask, 5), 1\n"
        "    umask(:,:,:,:,idx_1) = vmask(:,:,:,:,idx_1) + 1.0\n"
        "  enddo" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_existing_names(tmpdir):
    '''Check that the apply method uses existing iterators appropriately
    when their symbols are already defined.

    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    symbol_table = schedule.symbol_table
    symbol_table.add(DataSymbol("ji", INTEGER_TYPE))
    symbol_table.add(DataSymbol("jj", INTEGER_TYPE))
    symbol_table.add(DataSymbol("jk", INTEGER_TYPE))

    trans.apply(array_ref.children[2])
    trans.apply(array_ref.children[1])
    trans.apply(array_ref.children[0])

    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  do jk = 1, jpk, 1\n"
        "    do jj = 1, jpj, 1\n"
        "      do ji = 1, jpi, 1\n"
        "        umask(ji,jj,jk) = 0.0d0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_different_num_dims():
    '''Check that the apply method raises an exception when the number of
    range dimensions differ in different arrays. This should never
    happen as it is invalid PSyIR.

    '''
    _, invoke_info = get_invoke("implicit_mismatch_error.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(InternalError) as info:
        trans.apply(array_ref.children[1])
    assert ("The number of ranges in the arrays within this "
            "assignment are not equal. This is invalid PSyIR and "
            "should never happen." in str(info.value))


def test_apply_array_valued_function():
    '''Check that the apply method does not modify range nodes when they are
    used to specify the part of an array to pass into an array valued
    function.

    '''
    _, invoke_info = get_invoke("array_valued_function.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[1]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    trans.apply(array_ref.children[2])
    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  jn = 2\n"
        "  do jk = 1, jpk, 1\n"
        "    z3d(1,:,jk) = ptr_sjk(pvtr(:,:,:),btmsk(:,:,jn) * btm30(:,:))\n"
        "  enddo" in result)


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert("Error in NemoArrayRange2LoopTrans transformation. The supplied "
           "node argument should be a PSyIR Range, but found 'NoneType'."
           in str(info.value))


def test_str():
    '''Test that the str of an instance of the NemoArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert (str(NemoArrayRange2LoopTrans()) == "Convert the PSyIR assignment "
            "for a specified ArrayReference Range into a PSyIR NemoLoop.")


def test_name():
    '''Check that the name property of the ArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert NemoArrayRange2LoopTrans().name == "NemoArrayRange2LoopTrans"


def test_valid_node():
    '''Check that the validate() method raises the expected exception if
    the supplied node is invalid.

    '''
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert("Error in NemoArrayRange2LoopTrans transformation. The supplied "
           "node argument should be a PSyIR Range, but found 'NoneType'."
           in str(info.value))


def test_within_array_reference():
    '''Check that the validate() method raises the expected exception if
    the supplied node is not within an array reference.

    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    my_range = array_ref.children[2]
    for parent, result in [(assignment, "Assignment"), (None, "NoneType")]:
        my_range._parent = parent
        with pytest.raises(TransformationError) as info:
            trans.validate(my_range)
        assert("Error in NemoArrayRange2LoopTrans transformation. The "
               "supplied node argument should be within an "
               "ArrayReference node, but found '{0}'.".format(result)
               in str(info.value))


def test_within_assignment():
    '''Check that the validate() method raises the expected exception if
    the supplied node is not within an array reference that is within
    an assignment.

    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    my_range = array_ref.children[2]
    for parent, result in [(schedule, "NemoInvokeSchedule"),
                           (None, "NoneType")]:
        array_ref._parent = parent
        with pytest.raises(TransformationError) as info:
            trans.validate(my_range)
        assert("Error in NemoArrayRange2LoopTrans transformation. The "
               "supplied node argument should be within an ArrayReference "
               "node that is within an Assignment node, but found '{0}'."
               "".format(result) in str(info.value))


def test_within_lhs_assignment():
    '''Check that the validate() method raises the expected exception if
    the supplied node is not within an array reference that is within
    the lhs of an assignment (i.e. it is within the rhs).

    '''
    _, invoke_info = get_invoke("implicit_do2.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.rhs
    trans = NemoArrayRange2LoopTrans()
    my_range = array_ref.children[0]
    with pytest.raises(TransformationError) as info:
        trans.validate(my_range)
    assert("Error in NemoArrayRange2LoopTrans transformation. The "
           "supplied node argument should be within an ArrayReference "
           "node that is within the left-hand-side of an Assignment "
           "node, but it is on the right-hand-side." in str(info.value))


def test_array_valued_operator():
    '''Check that the vaidate() method raises the expected exception if an
    array valued operation is found on the rhs of the assignment node.

    '''
    _, invoke_info = get_invoke("array_valued_operation.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.children[0])
    assert (
        "Error in NemoArrayRange2LoopTrans transformation. This "
        "transformation does not support array valued operations on the rhs "
        "of the associated Assignment node, but found 'MATMUL'."
        in str(info.value))


def test_not_outermost_range():
    '''Check that the validate() method raises the expected exception if
    the supplied node is not the outermost Range within an array
    reference.

    '''
    _, invoke_info = get_invoke("implicit_do2.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    my_range = array_ref.children[0]
    with pytest.raises(TransformationError) as info:
        trans.validate(my_range)
    assert("Error in NemoArrayRange2LoopTrans transformation. This "
           "transformation can only be applied to the outermost "
           "Range." in str(info.value))


def test_outer_index_idx():
    '''Check that when given an array reference the internal
    get_outer_index() function returns the outermost index of the
    array that is a range.

    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    assert get_outer_index(array_ref) == 2


def test_outer_index_error():
    '''Check that when given an array reference the internal
    get_outer_index() function returns an IndexError exception if
    there are no ranges in the array indices.

    '''
    _, invoke_info = get_invoke("explicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignments = schedule.walk(Assignment)
    assert len(assignments) == 1
    assignment = assignments[0]
    array_ref = assignment.lhs
    with pytest.raises(IndexError):
        _ = get_outer_index(array_ref)


@pytest.mark.parametrize("datatype",
                         [REAL_TYPE, ArrayType(INTEGER_TYPE, [10]),
                          DeferredType()])
def test_loop_variable_name_error(datatype):
    '''Check that the expected exception is raised when the config file
    specifies a loop iteration name but it is already declared in the
    code as something that is not a scalar.

    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    symbol_table = schedule.symbol_table
    symbol_table.add(DataSymbol("jk", datatype))
    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.children[2])
    assert ("The config file specifies 'jk' as the name of the iteration "
            "variable but this is already declared in the code as something "
            "that is not a scalar integer, or is a deferred type."
            in str(info.value))
