# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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

from psyclone.psyir.nodes import Assignment, CodeBlock, BinaryOperation, Call
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE, \
    ArrayType, DeferredType, UnknownType, RoutineSymbol
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


def test_apply_non_existing_bound_names(tmpdir):
    '''Check that the apply method creates valid expressions when the bound
    symbols are not defined in this scope.

    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()

    # This example will use non standard loop bounds, so the transformation
    # will have to use LBOUND and UBOUND expressions when appropriate
    symbol_table = schedule.symbol_table
    symbol_table.rename_symbol(symbol_table.lookup("jpk"), "somethingelse")
    symbol_table.rename_symbol(symbol_table.lookup("jpj"), "somethingelse1")
    symbol_table.rename_symbol(symbol_table.lookup("jpi"), "somethingelse2")

    # Create a new config instance and load a test config file with
    # the bounds information set the way we want.
    config = Config.get(do_not_load_file=True)
    config.load(config_file=TEST_CONFIG)

    # Apply the transformation
    trans.apply(array_ref.children[2])
    trans.apply(array_ref.children[1])
    trans.apply(array_ref.children[0])

    # Remove this config file so the next time the default one will be
    # loaded (in case we affect other tests)
    Config._instance = None

    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  do jk = LBOUND(umask, 3), 1, 1\n"
        "    do jj = 1, UBOUND(umask, 2), 1\n"
        "      do ji = LBOUND(umask, 1), 1, 1\n"
        "        umask(ji,jj,jk) = 0.0d0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_structure_of_arrays():
    '''Check that the apply method works when the assignment expression
    contains structures of arrays.

    '''
    _, invoke_info = get_invoke("implicit_do_structures.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment1 = schedule[0]
    assignment3 = schedule[2]
    trans = NemoArrayRange2LoopTrans()

    # Case 1: SoA in the RHS
    array_ref = assignment1.lhs
    trans.apply(array_ref.children[2])
    trans.apply(array_ref.children[1])
    trans.apply(array_ref.children[0])

    writer = FortranWriter()
    result = writer(schedule)
    assert (
        "  do jk = 1, jpk, 1\n"
        "    do jj = 1, jpj, 1\n"
        "      do ji = 1, jpi, 1\n"
        "        umask(ji,jj,jk) = mystruct%field(ji,jj,jk) "
        "+ mystruct%field2%field(ji,jj,jk)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in result)

    # Case 2: SoA in the LHS is not yet supported
    assert "mystruct%field2%field(:,:,:) = 0.0d0" in result

    # Case 3: Nested SoA currently causes an InternalError
    array_ref = assignment3.lhs
    with pytest.raises(InternalError) as info:
        trans.apply(array_ref.children[2])
    assert ("The number of ranges in the arrays within this assignment are "
            "not equal. Any such case should have been dealt with by the "
            "validation method or represents invalid PSyIR."
            in str(info.value))


def test_apply_existing_names_as_ancestor_loop_variables():
    '''Check that the transformation is not applied if the variable name
    already exists as the variable name of an ancestor loop.
    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    trans = NemoArrayRange2LoopTrans()
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs

    # We create the same-variable loop by converting an implicit loop into an
    # explicit one once and then copying the full implicit expression again
    # inside
    implicit_loop = assignment.copy()
    trans.apply(array_ref.children[2])
    schedule[0].loop_body.addchild(implicit_loop)

    # Now the variable 'jk' in this case will already exist in an ancestor
    with pytest.raises(TransformationError) as info:
        trans.apply(implicit_loop.lhs.children[2])
    assert ("The config file specifies 'jk' as the name of the iteration "
            "variable but this is already used by an ancestor loop variable "
            "in:\ndo jk = 1, jpk, 1\n" in str(info.value))


def test_apply_with_codeblock():
    '''Check that the transformation is not applied if there is a Codeblock as
    part of the assignment.
    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    trans = NemoArrayRange2LoopTrans()
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    previous_rhs = assignment.rhs.detach()
    assignment.addchild(
            BinaryOperation.create(
                BinaryOperation.Operator.ADD,
                previous_rhs,
                CodeBlock([], CodeBlock.Structure.EXPRESSION)))

    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.children[2])
    assert ("This transformation does not support array assignments that "
            "contain a CodeBlock anywhere in the expression, but found:\n"
            "umask(:,:,:) = 0.0d0 + \n" in str(info.value))


def test_apply_with_a_function_call():
    '''Check that the transformation is not applied if there is a Call as
    part of the assignment.
    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    trans = NemoArrayRange2LoopTrans()
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    previous_rhs = assignment.rhs.detach()
    assignment.addchild(
            BinaryOperation.create(
                BinaryOperation.Operator.ADD,
                previous_rhs,
                Call.create(RoutineSymbol("func"), [])))

    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.children[2])
    assert ("This transformation does not support array assignments that "
            "contain a Call anywhere in the expression, but found:\n"
            "umask(:,:,:) = 0.0d0 + func()\n" in str(info.value))


def test_apply_with_array_with_hidden_accessor():
    '''Check that the transformation is not applied if there is a RHS array
    (or UnknownType) with the accessor expression missing.

    '''
    _, invoke_info = get_invoke("implicit_do_hidden_accessor.f90",
                                api=API, idx=0)
    trans = NemoArrayRange2LoopTrans()
    schedule = invoke_info.schedule
    assignment1 = schedule[0]
    assignment2 = schedule[1]
    schedule.symbol_table.view()
    # This test expects arg1 is parsed as ArrayType and arg2 as UnknownType
    assert isinstance(schedule.symbol_table.lookup("arg1").datatype,
                      ArrayType)
    assert isinstance(schedule.symbol_table.lookup("arg2").datatype,
                      UnknownType)

    # The first one fails because we know the type of the RHS reference is
    # an array but we don't have explicit dimensions.
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment1.lhs.children[2])
    assert ("Error in NemoArrayRange2LoopTrans transformation. Variable "
            "'arg1' must be a DataSymbol of ScalarType, but it's a 'arg1: "
            "<Array<Scalar<REAL" in str(info.value))

    # The second fails because it's an UnknownType and we don't know whether
    # it's an scalar or an array.
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment2.lhs.children[2])
    print(str(info.value))
    assert ("Error in NemoArrayRange2LoopTrans transformation. Variable "
            "'arg2' must be a DataSymbol of ScalarType, but it's a 'arg2: "
            "<UnknownFortranType" in str(info.value))


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
            "assignment are not equal. Any such case should have "
            "been dealt with by the validation method or represents "
            "invalid PSyIR." in str(info.value))


def test_apply_imported_function():
    ''' Check that the apply method refuses to transform the assignment when
    range nodes are inside a function, as it does not know if the function is
    declared as 'elemental' which changes the semantics of the array notation.
    '''
    _, invoke_info = get_invoke("array_valued_function.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[1]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.children[2])
    # TODO fparser/#201: currently fparsre parses imported symbols that can be
    # functions or arrays always as arrays accessors, for this reason the error
    # message talks about arrays instead of functions. If this is resolved this
    # test would be equivalent to test_apply_with_a_function_call.
    assert("Error in NemoArrayRange2LoopTrans transformation. This "
           "transformation does not support assignments with rhs arrays that "
           "don't have a range, but found 'ptr_sjk' in:\n"
           "z3d(1,:,:) = ptr_sjk(pvtr(:,:,:),btmsk(:,:,jn) * btm30(:,:))\n"
           in str(info.value))


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
        assert(f"Error in NemoArrayRange2LoopTrans transformation. The "
               f"supplied node argument should be within an "
               f"ArrayReference node, but found '{result}'."
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
        assert(f"Error in NemoArrayRange2LoopTrans transformation. The "
               f"supplied node argument should be within an ArrayReference "
               f"node that is within an Assignment node, but found '{result}'."
               in str(info.value))


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


def test_array_non_elemental_operator():
    '''Check that the vaidate() method raises the expected exception if a
    a non-elemental operation is found on the rhs of the assignment node.

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
        "transformation does not support non-elemental operations on the rhs "
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
            "that is not a scalar integer or a deferred type."
            in str(info.value))
