# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

'''Module containing tests for the NemoArrayRange2LoopTrans
transformation.'''

import os
import pytest

from psyclone.domain.nemo.transformations import NemoArrayRange2LoopTrans
from psyclone.errors import InternalError
from psyclone.nemo import NemoLoop
from psyclone.psyGen import Transformation
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Assignment, CodeBlock, BinaryOperation, \
    Call, Range, Literal
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, ArrayType, \
    UnsupportedType, RoutineSymbol
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.utilities import get_invoke, Compile

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
    '''Check that the apply method uses the range bounds if these are specified
    or the LBOUND, UBOUND expressions if they are not.

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
        "  do idx = LBOUND(umask, dim=3), UBOUND(umask, dim=3), 1\n"
        "    do idx_1 = 2, 4, 1\n"
        "      do idx_2 = LBOUND(umask, dim=1), UBOUND(umask, dim=1), 1\n"
        "        umask(idx_2,idx_1,idx) = 0.0d0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in result)
    assigns = schedule[0].walk(Assignment)
    assert len(assigns) == 1
    assert isinstance(assigns[0].parent.parent, NemoLoop)
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
        "  do idx = LBOUND(umask, dim=5), UBOUND(umask, dim=5), 1\n"
        "    do idx_1 = LBOUND(umask, dim=3), UBOUND(umask, dim=3), 1\n"
        "      do idx_2 = LBOUND(umask, dim=1), UBOUND(umask, dim=1), 1\n"
        "        umask(idx_2,jpj,idx_1,ndim,idx) = vmask(jpi,idx_2,idx_1,idx,"
        "ndim) + 1.0\n"
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
        "  do idx_1 = LBOUND(umask, dim=5), UBOUND(umask, dim=5), 1\n"
        "    umask(:,:,:,:,idx_1) = vmask(:,:,:,:,idx_1) + 1.0\n"
        "  enddo" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_structure_of_arrays(fortran_reader, fortran_writer):
    '''Check that the apply method works when the assignment expression
    contains structures of arrays.

    '''
    trans = NemoArrayRange2LoopTrans()

    # Case 1: SoA in the RHS
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        umask(:,:,:) = mystruct%field(:,:,:) + mystruct%field2%field(:,:,:)
    end subroutine test
    ''')
    array_ref = psyir.walk(Assignment)[0].lhs
    trans.apply(array_ref.children[2])
    trans.apply(array_ref.children[1])
    trans.apply(array_ref.children[0])
    result = fortran_writer(psyir)
    assert (
        "  do idx = LBOUND(umask, dim=3), UBOUND(umask, dim=3), 1\n"
        "    do idx_1 = LBOUND(umask, dim=2), UBOUND(umask, dim=2), 1\n"
        "      do idx_2 = LBOUND(umask, dim=1), UBOUND(umask, dim=1), 1\n"
        "        umask(idx_2,idx_1,idx) = mystruct%field(idx_2,idx_1,idx) "
        "+ mystruct%field2%field(idx_2,idx_1,idx)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in result)

    # Case 2: SoA in the LHS
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        mystruct%field2%field(:,:,:) = 0.0d0
    end subroutine test
    ''')
    array_ref = psyir.walk(Assignment)[0].lhs
    trans.apply(array_ref.member.member.children[2])
    trans.apply(array_ref.member.member.children[1])
    trans.apply(array_ref.member.member.children[0])
    result = fortran_writer(psyir)
    assert (
        "  do idx = LBOUND(mystruct%field2%field, dim=3), "
        "UBOUND(mystruct%field2%field, dim=3), 1\n"
        "    do idx_1 = LBOUND(mystruct%field2%field, dim=2), "
        "UBOUND(mystruct%field2%field, dim=2), 1\n"
        "      do idx_2 = LBOUND(mystruct%field2%field, dim=1), "
        "UBOUND(mystruct%field2%field, dim=1), 1\n"
        "        mystruct%field2%field(idx_2,idx_1,idx) = 0.0d0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in result)

    # Case 3: SoAoS in the LHS
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        mystruct%field3(:,:,:)%field4 = 0.0d0
    end subroutine test
    ''')
    array_ref = psyir.walk(Assignment)[0].lhs
    trans.apply(array_ref.member.children[3])
    trans.apply(array_ref.member.children[2])
    trans.apply(array_ref.member.children[1])
    result = fortran_writer(psyir)
    assert (
        "  do idx = LBOUND(mystruct%field3, dim=3), "
        "UBOUND(mystruct%field3, dim=3), 1\n"
        "    do idx_1 = LBOUND(mystruct%field3, dim=2), "
        "UBOUND(mystruct%field3, dim=2), 1\n"
        "      do idx_2 = LBOUND(mystruct%field3, dim=1), "
        "UBOUND(mystruct%field3, dim=1), 1\n"
        "        mystruct%field3(idx_2,idx_1,idx)%field4 = 0.0d0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in result)

    # Case 4: SoAoS in the LHS and SoA in the RHS
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        mystruct%field3(:,:,:)%field4 = mystruct%field2%field(:,:,:)
    end subroutine test
    ''')
    array_ref = psyir.walk(Assignment)[0].lhs
    trans.apply(array_ref.member.children[3])
    trans.apply(array_ref.member.children[2])
    trans.apply(array_ref.member.children[1])
    result = fortran_writer(psyir)
    assert (
        "  do idx = LBOUND(mystruct%field3, dim=3), "
        "UBOUND(mystruct%field3, dim=3), 1\n"
        "    do idx_1 = LBOUND(mystruct%field3, dim=2), "
        "UBOUND(mystruct%field3, dim=2), 1\n"
        "      do idx_2 = LBOUND(mystruct%field3, dim=1), "
        "UBOUND(mystruct%field3, dim=1), 1\n"
        "        mystruct%field3(idx_2,idx_1,idx)%field4 = "
        "mystruct%field2%field(idx_2,idx_1,idx)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in result)


def test_apply_structure_of_arrays_multiple_arrays(fortran_reader,
                                                   fortran_writer):
    '''Check that the apply method works when the assignment expression
    contains structures of arrays with multiple array accessors.

    '''
    trans = NemoArrayRange2LoopTrans()

    # Case 1: 2 array accessors in LHS but only one has ranges
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        mystruct%field2(4, 3)%field(:,:,:) = mystruct%field2(5, 8)%field(:,:,:)
    end subroutine test
    ''')
    array_ref = psyir.walk(Assignment)[0].lhs
    trans.apply(array_ref.member.member.children[2])
    trans.apply(array_ref.member.member.children[1])
    trans.apply(array_ref.member.member.children[0])
    result = fortran_writer(psyir)
    assert (
        "  do idx = LBOUND(mystruct%field2(4,3)%field, dim=3), "
        "UBOUND(mystruct%field2(4,3)%field, dim=3), 1\n"
        "    do idx_1 = LBOUND(mystruct%field2(4,3)%field, dim=2), "
        "UBOUND(mystruct%field2(4,3)%field, dim=2), 1\n"
        "      do idx_2 = LBOUND(mystruct%field2(4,3)%field, dim=1), "
        "UBOUND(mystruct%field2(4,3)%field, dim=1), 1\n"
        "        mystruct%field2(4,3)%field(idx_2,idx_1,idx) = "
        "mystruct%field2(5,8)%field(idx_2,idx_1,idx)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in result)


def test_transform_apply_fixed():
    '''Check that the PSyIR is transformed as expected for a lat,lon loop
    when the lhs of the loop has known fixed bounds with the same
    values for each index. There used to be a bug where the first
    index was picked up in error instead of the second in the
    arrayrange2loop validate method but this should now be fixed.

    '''
    _, invoke_info = get_invoke("fixed_lhs.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    range_node = assignment.lhs.children[1]
    trans = NemoArrayRange2LoopTrans()
    trans.apply(range_node)
    writer = FortranWriter()
    result = writer(schedule)
    print(result)
    expected = (
        "  do idx = 6, 8, 1\n"
        "    sshn(2:4,idx) = sshn(2:4,idx) + ssh_ref * tmask(2:4,idx)\n"
        "  enddo\n")
    assert expected in result


def test_validate_unsupported_structure_of_arrays(fortran_reader):
    '''Check that nested structure_of_arrays are not supported. '''
    trans = NemoArrayRange2LoopTrans()

    # Case 1: 2 array accessors in LHS and both have ranges
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        mystruct%field2(4)%field(:,:,:) = 0
    end subroutine test
    ''')
    array_ref = psyir.walk(Assignment)[0].lhs
    array_ref.member.indices[0].replace_with(Range.create(
        Literal("1", INTEGER_TYPE), Literal("10", INTEGER_TYPE)))
    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.member.member.children[2])
    assert ("Error in NemoArrayRange2LoopTrans transformation. This "
            "transformation does not support array assignments that contain "
            "nested Range structures, but found:\n" in str(info.value))

    # Case 2: Nested array in another array
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        mystruct%field5(indices(:)) = 0.0d0
    end subroutine test
    ''')
    array_ref = psyir.walk(Assignment)[0].lhs
    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.member.indices[0].indices[0])
    assert ("Error in NemoArrayRange2LoopTrans transformation. This "
            "transformation does not support array assignments that contain "
            "nested Range structures, but found:\n" in str(info.value))

    # Case 3: Nested array in another array which also have Ranges
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        umask(:,mystruct%field2%field3(:),:) = &
            mystruct%field(mystruct%field2%field3(:),:,:)
    end subroutine test
    ''')
    array_ref = psyir.walk(Assignment)[0].lhs
    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.children[2])
    assert ("Error in NemoArrayRange2LoopTrans transformation. This "
            "transformation does not support array assignments that contain "
            "nested Range structures, but found:\n" in str(info.value))


def test_validate_with_codeblock():
    '''Check that the validation method of the transformation raises an
    exception if there is a Codeblock as part of the assignment.
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


def test_validate_with_a_function_call():
    '''Check that the validation method of the transformation raises an
    exception if there is a Call as part of the assignment.
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
    assert ("This transformation does not support non-elemental Calls on the "
            "rhs of the associated Assignment node, but found 'func' in:\n"
            "umask(:,:,:) = 0.0d0 + func()\n" in str(info.value))


def test_validate_with_array_with_hidden_accessor():
    '''Check that the validation method of the transformation raises an
    exception if there is a RHS array (or UnsupportedType) with the accessor
    expression missing.
    '''
    _, invoke_info = get_invoke("implicit_do_hidden_accessor.f90",
                                api=API, idx=0)
    trans = NemoArrayRange2LoopTrans()
    schedule = invoke_info.schedule
    assignment1 = schedule[0]
    assignment2 = schedule[1]
    # This test expects arg1 is parsed as ArrayType and arg2 as UnsupportedType
    assert isinstance(schedule.symbol_table.lookup("arg1").datatype,
                      ArrayType)
    assert isinstance(schedule.symbol_table.lookup("arg2").datatype,
                      UnsupportedType)

    # The first one fails because we know the type of the RHS reference is
    # an array but we don't have explicit dimensions.
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment1.lhs.children[2])
    assert ("Error in NemoArrayRange2LoopTrans transformation. Variable "
            "'arg1' must be a DataSymbol of ScalarType, but it's a 'arg1: "
            "DataSymbol<Array<Scalar<REAL" in str(info.value))

    # The second fails because it's an UnsupportedType and we don't know
    # whether it's an scalar or an array.
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment2.lhs.children[2])
    assert ("Error in NemoArrayRange2LoopTrans transformation. Variable "
            "'arg2' must be a DataSymbol of ScalarType, but it's a 'arg2: "
            "DataSymbol<UnsupportedFortranType" in str(info.value))


def test_apply_different_num_dims():
    '''Check that the validate method raises an exception when the number of
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


def test_validate_imported_function():
    '''Check that the validation method of the transformation raises an
    exception when range nodes are inside a function, as it does not know if
    the function is declared as 'elemental' which changes the semantics of the
    array notation.
    '''
    _, invoke_info = get_invoke("array_valued_function.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[1]
    array_ref = assignment.lhs
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(array_ref.children[2])
    # TODO fparser/#201: currently fparser parses imported symbols that can be
    # functions or arrays always as arrays accessors, for this reason the error
    # message talks about arrays instead of functions. If this is resolved this
    # test would be equivalent to test_apply_with_a_function_call.
    assert ("Error in NemoArrayRange2LoopTrans transformation. This "
            "transformation does not support array assignments that contain "
            "nested Range structures, but found:\n" in str(info.value))


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = NemoArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in NemoArrayRange2LoopTrans transformation. The supplied "
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
    assert ("Error in NemoArrayRange2LoopTrans transformation. The supplied "
            "node argument should be a PSyIR Range, but found 'NoneType'."
            in str(info.value))


def test_validate_within_array_reference():
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
        assert (f"Error in NemoArrayRange2LoopTrans transformation. The "
                f"supplied node argument should be within an array "
                f"access node, but found '{result}'."
                in str(info.value))


def test_validate_within_assignment():
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
    for parent in (schedule, None):
        array_ref._parent = parent
        with pytest.raises(TransformationError) as info:
            trans.validate(my_range)
        assert ("Error in NemoArrayRange2LoopTrans transformation. The "
                "supplied node argument should be within an Assignment node, "
                "but found a 'Range[]' that is not in an assignment."
                in str(info.value))


def test_validate_within_lhs_assignment():
    '''Check that the validate() method raises the expected exception if
    the supplied node is not within an array reference that is within
    the lhs of an assignment (i.e. it is within the rhs).

    '''
    _, invoke_info = get_invoke("implicit_do2.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.rhs
    trans = NemoArrayRange2LoopTrans()
    my_range = array_ref.children[-1]
    with pytest.raises(TransformationError) as info:
        trans.validate(my_range)
    assert ("Error in NemoArrayRange2LoopTrans transformation. The "
            "supplied node argument should be within an array access "
            "node that is within the left-hand-side of an Assignment "
            "node, but it is on the right-hand-side." in str(info.value))


def test_validate_array_non_elemental_operator():
    '''Check that the validate() method raises the expected exception if a
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
        "transformation does not support non-elemental IntrinsicCalls on the "
        "rhs of the associated Assignment node, but found 'MATMUL' in:"
        in str(info.value))


def test_validate_not_outermost_range():
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
    assert ("Error in NemoArrayRange2LoopTrans transformation. This "
            "transformation can only be applied to the outermost "
            "Range." in str(info.value))
