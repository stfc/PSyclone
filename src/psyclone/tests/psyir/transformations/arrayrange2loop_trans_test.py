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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab
# Modified by A. B. G. Chalk, STFC Daresbury Lab

'''Module containing tests for the ArrayRange2LoopTrans
transformation.'''


import pytest

from psyclone.psyir.nodes import Literal, BinaryOperation, Reference, \
    Range, ArrayReference, Assignment, Node, DataNode, KernelSchedule, \
    IntrinsicCall
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import (ArgumentInterface, ArrayType, DataSymbol,
                                    INTEGER_TYPE, REAL_TYPE, SymbolTable)
from psyclone.psyir.transformations import ArrayRange2LoopTrans, \
    TransformationError
from psyclone.errors import InternalError
from psyclone.tests.utilities import Compile



def test_str():
    '''Test that the str of an instance of the ArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert (str(ArrayRange2LoopTrans()) == "Convert a PSyIR assignment to an "
            "array Range into a PSyIR Loop.")


def test_name():
    '''Check that the name property of the ArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert ArrayRange2LoopTrans().name == "ArrayRange2LoopTrans"


@pytest.mark.parametrize(
        "code, expected",

        # Scalar RHS
        [("integer, dimension(:) :: x\n"
          "x(:) = 0.0",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = 0.0\n"),

         # Array LHS and RHS
         ("integer, dimension(:) :: x, y\n"
          "x(:) = y(:)\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = y(idx)\n"),

         # Multi-dimensional array LHS and RHS
         # ("integer, dimension(:,:,:) :: x, y\n"
         #  "x(:,:,:) = y(:,:,:)\n",
         #  "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
         #  "    do idx_1 = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
         #  "      do idx_2 = LBOUND(x, dim=3), UBOUND(x, dim=3), 1\n"
         #  "        x(idx, idx_1, idx_2) = y(idx, idx_1, idx_2)\n"),

         # Multiple array RHS and LHS
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(:) = y(:) + z(:) * t(:)\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = y(idx) + z(idx) * t(idx)\n"),

         # Elemental intrinsic

         # Non-Elemental intrinsic

         # Mix different array ranks with fixed indices
         ("integer, dimension(:) :: x\n"
          "integer, dimension(:,:) :: y\n"
          "x(:) = y(n,:)\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = y(n,idx)\n"),

         ("integer, dimension(:,:) :: x\n"
          "integer, dimension(:) :: y\n"
          "x(n,:) = y(:)\n",
          "  do idx = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
          "    x(n,idx) = y(idx)\n"),

         ("integer, dimension(:,:) :: x, y\n"
          "x(:,:)=y(:,n,:)\n",
          "  do idx = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
          "    x(:,idx) = y(:,n,idx)\n"),

         # Same rank but different range locations
         # ("integer, parameter :: jpi=2, jpj=4, jpk=6, jpt=9, ndim=10\n"
         #  "real, dimension(jpi,jpj,jpk,jpt,ndim) :: umask, vmask\n"
         #  "umask(:,jpj,:,ndim,:) = vmask(jpi,:,:,:,ndim) + 1.0\n",
         #  "  do idx = LBOUND(umask, dim=5), UBOUND(umask, dim=5), 1\n"
         #  "    do idx_1 = LBOUND(umask, dim=3), UBOUND(umask, dim=3), 1\n"
         #  "      do idx_2 = LBOUND(umask, dim=1), UBOUND(umask, dim=1), 1\n"
         #  "        umask(idx_2,jpj,idx_1,ndim,idx) = vmask(jpi,idx_2,idx_1,"
         #  "idx,ndim) + 1.0\n"
         #  "      enddo\n"
         #  "    enddo\n"
         #  "  enddo"),

         # Explicit slice values
         ("integer, dimension(:) :: x\n"
          "x(2:4) = 0",
          "  do idx = 2, 4, 1\n"
          "    x(idx) = 0\n"),
         ("integer, dimension(:) :: x\n"
          "x(1:1) = 0",
          "  do idx = 1, 1, 1\n"
          "    x(idx) = 0\n"),
         ("integer, dimension(:) :: x\n"
          "x(2:8:4) = 0",
          "  do idx = 2, 8, 4\n"
          "    x(idx) = 0\n"),

         # Explicitly declared dimension values
         ("integer, dimension(2:4) :: x\n"
          "x(:) = 0",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = 0\n"),

         # Symbolic equalities

         # Combine multiple features
         ("integer, dimension(:,:) :: x,z\n"
          "integer, dimension(:) :: y,t\n"
          "x(n,2:n:2)=y(2:n:2)*z(1,2:n:2)+t(1)",
          "  do idx = 2, n, 2\n"
          "    x(n,idx) = y(idx) * z(1,idx) + t(1)")])
def test_apply(code, expected, tmpdir, fortran_reader, fortran_writer):
    '''Check that the PSyIR is transformed as expected for various types
    of ranges in an array. The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    psyir = fortran_reader.psyir_from_source(f'''
        subroutine test()
          integer :: n
          {code}
        end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans = ArrayRange2LoopTrans()
    trans.apply(assignment)
    result = fortran_writer(psyir)
    assert expected in result, f"\nExpected:\n{expected}\nBut got:\n{result}"
    assert Compile(tmpdir).string_compiles(result)


def test_apply_calls_validate():
    ''' Check that the apply() method calls the validate method.'''
    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in ArrayRange2LoopTrans transformation. The supplied node "
            "should be a PSyIR Assignment, but found 'NoneType'."
            in str(info.value))


def test_validate_no_assignment_with_array_range_on_lhs():
    '''Test that the validate method in the ArrayRange2LoopTrans class
    raises the expected exceptions when the provided node is not an
    array Assignment with a Range in the LHS expression.

    '''
    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(Node())
    assert (
        "Error in ArrayRange2LoopTrans transformation. The supplied node "
        "should be a PSyIR Assignment, but found 'Node'."
        in str(info.value))

    with pytest.raises(TransformationError) as info:
        trans.validate(Assignment.create(DataNode(), DataNode()))
    assert (
        "Error in ArrayRange2LoopTrans transformation. The LHS of the "
        "supplied Assignment node should be a Reference that contains an "
        "array accessor somewhere in the expression, but found "
         in str(info.value))

    # Array Reference but with acessor that resolve to a single scalar
    array_symbol = DataSymbol("x", ArrayType(INTEGER_TYPE, [10, 10]))
    one = Literal("1", INTEGER_TYPE)
    array_assignment = ArrayReference.create(array_symbol, [one, one.copy()])
    with pytest.raises(TransformationError) as info:
        trans.validate(Assignment.create(array_assignment, DataNode()))
    assert (
        "Error in ArrayRange2LoopTrans transformation. The lhs of the "
        "supplied Assignment node should be a PSyIR ArrayReference with at "
        "least one of its dimensions being a Range, but none were found in "
        "'x(1,1) =" in str(info.value))


@pytest.mark.parametrize(
        "code, expected", [
         ("integer, dimension(:,:) :: x, y, z\n"
          "x(:) = matmul(y, z)",
          "The rhs of the supplied Assignment contains a non-elemental call "
          "to 'MATMUL(y, z)'.")])
def test_validate_with_log(code, expected, fortran_reader):
    '''Check that the validate method returns an exception if the rhs of
    the assignment contains an operator that only returns an array
    i.e. can't be performed elementwise.

    This errors also logged as a comment.

    '''
    psyir = fortran_reader.psyir_from_source(f'''
        subroutine test()
          integer :: n
          {code}
        end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(assignment)
    assert ("Error in ArrayRange2LoopTrans transformation. " + expected
            in str(err.value))


def test_character_validation(fortran_reader):
    '''Check that the validate method returns an exception if the
    lhs of the assignment contains a character array and the allow_string
    option isn't defined, and that it doesn't return an exception if the
    allow_string option is True.'''

    code = '''subroutine test()
        character :: a(100)
        character :: b(100)
        a(1:94) = b(1:94)
    end subroutine test'''

    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]

    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assign)
    assert (
        "The ArrayRange2LoopTrans transformation doesn't allow "
        "character arrays by default. This can be enabled by "
        "passing the allow_string option to the transformation."
        in str(info.value))

    trans.validate(assign, options={"allow_string": True})

    # Check it also works for rhs
    code = '''subroutine test()
    use some_mod
    character :: b(100)

    a(1:94) = b(1)
    end subroutine test'''
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]

    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assign)
    assert (
        "The ArrayRange2LoopTrans transformation doesn't allow "
        "character arrays by default. This can be enabled by "
        "passing the allow_string option to the transformation."
        in str(info.value))

    # Check it also works for RHS literals
    code = '''subroutine test()
        use some_mod
        a(1:94) = 'x'
    end subroutine test'''
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]

    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assign)
    assert (
        "The ArrayRange2LoopTrans transformation doesn't allow "
        "character arrays by default. This can be enabled by "
        "passing the allow_string option to the transformation."
        in str(info.value))

    # Check we accept when we find character(LEN=x) syntax as this is an
    # UnsupportedFortranType
    # TODO #2441
    code = '''subroutine test()
        character(LEN=100) :: a
        character(LEN=100) :: b
        a(1:94) = b(1:94)
    end subroutine test'''
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]

    trans = ArrayRange2LoopTrans()
    trans.validate(assign)


def test_apply_different_dims(tmpdir):
    '''Check that the apply method adds loop iterators appropriately when
    the range dimensions differ in different arrays.'''
    _, invoke_info = get_invoke("implicit_different_dims.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0]
    array_ref = assignment.lhs
    trans = ArrayRange2LoopTrans()
    for index in [4, 2, 0]:
        range_node = array_ref.children[index]
        trans.apply(range_node)
    result = fortran_writer(schedule)
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


def test_apply_structure_of_arrays(fortran_reader, fortran_writer):
    '''Check that the apply method works when the assignment expression
    contains structures of arrays.

    '''
    trans = ArrayRange2LoopTrans()

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
    trans = ArrayRange2LoopTrans()

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
    trans = ArrayRange2LoopTrans()
    trans.apply(range_node)
    result = fortran_writer(schedule)
    print(result)
    expected = (
        "  do idx = 6, 8, 1\n"
        "    sshn(2:4,idx) = sshn(2:4,idx) + ssh_ref * tmask(2:4,idx)\n"
        "  enddo\n")
    assert expected in result


def test_validate_unsupported_structure_of_arrays(fortran_reader):
    '''Check that nested structure_of_arrays are not supported. '''
    trans = ArrayRange2LoopTrans()

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
    assert ("Error in ArrayRange2LoopTrans transformation. This "
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
    assert ("Error in ArrayRange2LoopTrans transformation. This "
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
    assert ("Error in ArrayRange2LoopTrans transformation. This "
            "transformation does not support array assignments that contain "
            "nested Range structures, but found:\n" in str(info.value))


def test_validate_with_codeblock():
    '''Check that the validation method of the transformation raises an
    exception if there is a Codeblock as part of the assignment.
    '''
    _, invoke_info = get_invoke("implicit_do.f90", api=API, idx=0)
    trans = ArrayRange2LoopTrans()
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
    trans = ArrayRange2LoopTrans()
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
    trans = ArrayRange2LoopTrans()
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
    assert ("Error in ArrayRange2LoopTrans transformation. Variable "
            "'arg1' must be a DataSymbol of ScalarType, but it's a 'arg1: "
            "DataSymbol<Array<Scalar<REAL" in str(info.value))

    # The second fails because it's an UnsupportedType and we don't know
    # whether it's an scalar or an array.
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment2.lhs.children[2])
    assert ("Error in ArrayRange2LoopTrans transformation. Variable "
            "'arg2' must be a DataSymbol of ScalarType, but it's a 'arg2: "
            "DataSymbol<UnsupportedFortranType" in str(info.value))


def test_apply_different_num_dims(tmpdir, fortran_reader, fortran_writer):
    '''Check that the validate method raises an exception when the number of
    range dimensions differ in different arrays. This should never
    happen as it is invalid PSyIR.

    '''
    psyir = fortran_reader.psyir_from_source('''
        program implicit_mismatch_error
          implicit none
          integer, parameter :: jpi=64, jpj=32
          real, dimension(jpi,jpj) :: umask, vmask

          ! This is invalid Fortran
          umask(:,:) = vmask(:,jpj) + 1.0

        end program implicit_mismatch_error
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans = ArrayRange2LoopTrans()
    with pytest.raises(InternalError) as info:
        trans.apply(assignment)
    assert ("The number of ranges in the arrays within this "
            "assignment are not equal. Any such case should have "
            "been dealt with by the validation method or represents "
            "invalid PSyIR." in str(info.value))


def test_validate_imported_function(tmpdir, fortran_reader, fortran_writer):
    '''Check that the validation method of the transformation raises an
    exception when range nodes are inside a function, as it does not know if
    the function is declared as 'elemental' which changes the semantics of the
    array notation.
    '''
    psyir = fortran_reader.psyir_from_source('''
    program implicit_do
      use some_mod, only: ptr_sjk
      implicit none
      integer, parameter :: jpi=10, jpj=10, jpk=10, jpn=2
      integer :: ji, jk, jn
      real(kind=kind(1.0d0)), dimension(jpi,jpj,jpk) :: umask
      real(kind=kind(1.0d0)), dimension(jpi,jpj,jpk) :: z3d
      integer, dimension(jpi,jpj,jpk) :: pvtr
      integer, dimension(jpi,jpj) :: btm30
      integer, dimension(jpi,jpj,jpn) :: btmsk

      ! Test code with array notation used in call to array-valued function
      ! (`ptr_sjk`).
      jn = 2

      z3d(1,:,:) =  ptr_sjk(pvtr(:,:,:), btmsk(:,:,jn)*btm30(:,:))

    end program implicit_do
    ''')
    assignment = psyir.walk(Assignment)[1]
    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment)
    # TODO fparser/#201: currently fparser parses imported symbols that can be
    # functions or arrays always as arrays accessors, for this reason the error
    # message talks about arrays instead of functions. If this is resolved this
    # test would be equivalent to test_apply_with_a_function_call.
    assert ("Error in ArrayRange2LoopTrans transformation. This "
            "transformation does not support array assignments that contain "
            "nested Range structures, but found:\n" in str(info.value))

    # We can obtains the information that the symbol is a routine, but we still
    # don't know what to do if we don't know if it is elemental or not.
