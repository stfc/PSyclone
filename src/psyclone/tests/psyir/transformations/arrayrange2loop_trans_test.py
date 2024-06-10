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

from psyclone.psyir.nodes import (
    Literal, BinaryOperation, Range, ArrayReference, Assignment,
    Node, DataNode, CodeBlock, Schedule, Loop)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, INTEGER_TYPE, UnresolvedType)
from psyclone.psyir.transformations import ArrayRange2LoopTrans, \
    TransformationError
# from psyclone.tests.utilities import Compile


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
         ("integer, dimension(:,:,:) :: x, y\n"
          "x(:,:,:) = y(:,:,:)\n",
          "  do idx = LBOUND(x, dim=3), UBOUND(x, dim=3), 1\n"
          "    do idx_1 = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
          "      do idx_2 = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "        x(idx_2,idx_1,idx) = y(idx_2,idx_1,idx)\n"),

         # Multiple arrays on RHS
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(:) = y(:) + z(:) * t(:)\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = y(idx) + z(idx) * t(idx)\n"),

         # Argument of elemental functions are expanded
         ("integer, dimension(:) :: x, y, z\n"
          "x(:) = max(y(:), z(:))\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = MAX(y(idx), z(idx))\n"),

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

         ("integer, dimension(:,:) :: x\n"
          "integer, dimension(:,:,:) :: y\n"
          "x(:,:)=y(:,n,:)\n",
          "  do idx = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
          "    do idx_1 = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "      x(idx_1,idx) = y(idx_1,n,idx)\n"),

         # Same rank but different range locations
         ("integer, parameter :: jpi=2, jpj=4, jpk=6, jpt=9, ndim=10\n"
          "real, dimension(jpi,jpj,jpk,jpt,ndim) :: umask, vmask\n"
          "umask(:,jpj,:,ndim,:) = vmask(jpi,:,:,:,ndim) + 1.0\n",
          "  do idx = LBOUND(umask, dim=5), UBOUND(umask, dim=5), 1\n"
          "    do idx_1 = LBOUND(umask, dim=3), UBOUND(umask, dim=3), 1\n"
          "      do idx_2 = LBOUND(umask, dim=1), UBOUND(umask, dim=1), 1\n"
          "        umask(idx_2,jpj,idx_1,ndim,idx) = vmask(jpi,idx_2,idx_1,"
          "idx,ndim) + 1.0\n"
          "      enddo\n"
          "    enddo\n"
          "  enddo"),

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

         # Explicitly declared dimension values (L/UBOUND are correct)
         ("integer, dimension(2:4) :: x\n"
          "x(:) = 0",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = 0\n"),

         # Combine multiple previous features
         ("integer, dimension(:,:) :: x,z\n"
          "integer, dimension(:) :: y,t\n"
          "x(n,2:n:2)=y(2:n:2)*z(1,2:n:2)+t(1)",
          "  do idx = 2, n, 2\n"
          "    x(n,idx) = y(idx) * z(1,idx) + t(1)"),

         # SoA in LHS
         ("mystruct%field2%field(:,:,:) = 0.0d0",
          "  do idx = LBOUND(mystruct%field2%field, dim=3), "
          "UBOUND(mystruct%field2%field, dim=3), 1\n"
          "    do idx_1 = LBOUND(mystruct%field2%field, dim=2), "
          "UBOUND(mystruct%field2%field, dim=2), 1\n"
          "      do idx_2 = LBOUND(mystruct%field2%field, dim=1), "
          "UBOUND(mystruct%field2%field, dim=1), 1\n"
          "        mystruct%field2%field(idx_2,idx_1,idx) = 0.0d0\n"),

         # Array in LHS and SoA in RHS
         ("umask(:,:,:) = 3 + struct%field2%field(:,:,:)",
          "  do idx = LBOUND(umask, dim=3), UBOUND(umask, dim=3), 1\n"
          "    do idx_1 = LBOUND(umask, dim=2), UBOUND(umask, dim=2), 1\n"
          "      do idx_2 = LBOUND(umask, dim=1), UBOUND(umask, dim=1), 1\n"
          # Ignore offset for this test, it is tested below
          "        umask(idx_2,idx_1,idx) = 3 + struct%field2%field(idx_2 + "),

         # SoAoS on LHS
         ("mystruct%field3(:,:,:)%field4 = 0.0d0",
          "  do idx = LBOUND(mystruct%field3, dim=3), "
          "UBOUND(mystruct%field3, dim=3), 1\n"
          "    do idx_1 = LBOUND(mystruct%field3, dim=2), "
          "UBOUND(mystruct%field3, dim=2), 1\n"
          "      do idx_2 = LBOUND(mystruct%field3, dim=1), "
          "UBOUND(mystruct%field3, dim=1), 1\n"
          "        mystruct%field3(idx_2,idx_1,idx)%field4 = 0.0d0\n"),

         # SoAoS in the LHS and SoA in the RHS
         ("mystruct%field3(:,4,:)%field4 = mystruct%field2%field(3,:,:)",
          "  do idx = LBOUND(mystruct%field3, dim=3), "
          "UBOUND(mystruct%field3, dim=3), 1\n"
          "    do idx_1 = LBOUND(mystruct%field3, dim=1), "
          "UBOUND(mystruct%field3, dim=1), 1\n"
          # Ignore offset for this test, it is tested below
          "      mystruct%field3(idx_1,4,idx)%field4 = "
          "mystruct%field2%field(3,idx_1 + "),

         # 2 array accessors in each expression but only one has ranges
         ("mystruct%field2(4, 3)%field(:,:,:) = "
          "mystruct%field2(5, 8)%field(:,:,:)",
          "  do idx = LBOUND(mystruct%field2(4,3)%field, dim=3), "
          "UBOUND(mystruct%field2(4,3)%field, dim=3), 1\n"
          "    do idx_1 = LBOUND(mystruct%field2(4,3)%field, dim=2), "
          "UBOUND(mystruct%field2(4,3)%field, dim=2), 1\n"
          "      do idx_2 = LBOUND(mystruct%field2(4,3)%field, dim=1), "
          "UBOUND(mystruct%field2(4,3)%field, dim=1), 1\n"
          # Ignore offset for this test, it is tested below
          "        mystruct%field2(4,3)%field(idx_2,idx_1,idx) = "
          "mystruct%field2(5,8)%field(idx_2 +")])
def test_apply(code, expected, tmpdir, fortran_reader, fortran_writer):
    '''Check that the PSyIR is transformed as expected for various types
    of ranges in an array. The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    psyir = fortran_reader.psyir_from_source(f'''
        subroutine test()
          use other_mod
          integer :: n
          {code}
        end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans = ArrayRange2LoopTrans()
    trans.apply(assignment)
    result = fortran_writer(psyir)
    assert expected in result, f"\nExpected:\n{expected}\nBut got:\n{result}"
#     assert Compile(tmpdir).string_compiles(result)


def test_apply_to_arrays_with_different_bounds(fortran_reader, fortran_writer):
    '''Check that the apply method inserts the correct index offsets when
    the lhs and rhs range bounds can not be guaranteed to be the same. '''
    psyir = fortran_reader.psyir_from_source('''
        program test
          use other
          integer, dimension(10,10) :: x2
          integer, dimension(10:20,20:30) :: y2

          ! Test 1: we don't know the bounds
          x1(:,:) = y1(:,:)

          ! Test 2: We know the bounds and they are not equal
          x2(:,:) = y2(:,:)

          ! Test 3: We know the bounds and we have implicit bounds accesses
          x2(2:4,4:6) = y2(15:17,28:30) + 1.0

          ! Test 4: SoA and SoAoS
          x(:) = struct%values(:) + struct%array(:)%value
        end program test
    ''')
    trans = ArrayRange2LoopTrans()
    for assignment in psyir.walk(Assignment):
        trans.apply(assignment)

    # The bounds are not known, so L/UBOUND expressions are used
    output_test1 = fortran_writer(psyir.walk(Assignment)[0])
    assert ("x1(idx_1,idx) = y1(idx_1 + (LBOUND(y1, dim=1) - LBOUND(x1, "
            "dim=1)),idx + (LBOUND(y1, dim=2) - LBOUND(x1, dim=2)))"
            in output_test1)

    # When we know the bounds we can see they are different, we also
    # need the offsets (and can also use L/UBOUND)
    output_test2 = fortran_writer(psyir.walk(Assignment)[1])
    assert ("x2(idx_3,idx_2) = y2(idx_3 + (LBOUND(y2, dim=1) - LBOUND(x2, "
            "dim=1)),idx_2 + (LBOUND(y2, dim=2) - LBOUND(x2, dim=2)))"
            in output_test2)

    # If the bounds are implicit, the offset should also use the implicit
    # values
    output_test3 = fortran_writer(psyir)
    assert ("""
  do idx_4 = 4, 6, 1
    do idx_5 = 2, 4, 1
      x2(idx_5,idx_4) = y2(idx_5 + (15 - 2),idx_4 + (28 - 4)) + 1.0"""
            in output_test3)

    # SoA and SoAoS bounds are also constructed using L/UBOUNDS expressions
    output_test4 = fortran_writer(psyir.walk(Assignment)[3])
    assert (" struct%values(idx_6 + (LBOUND(struct%values, dim=1) - "
            "LBOUND(x, dim=1))) + struct%array(idx_6 + ("
            "LBOUND(struct%array, dim=1) - LBOUND(x, dim=1)))%value"
            in output_test4)


def test_apply_outside_routine(fortran_reader, fortran_writer):
    ''' Check that the transformation still succeeds and generates valid
    code when found in a scope detached from a Routine. '''

    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
          use other_mod
          do i = 1, 10
            A(:) = A(:) + B(:)
          end do
        end subroutine test
    ''')
    loop = psyir.walk(Loop)[0].detach()
    assignment = loop.walk(Assignment)[0]
    trans = ArrayRange2LoopTrans()
    trans.apply(assignment)
    result = fortran_writer(loop)
    assert "a(idx) = a(idx) + b(idx +" in result


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

    assignment = Assignment.create(DataNode(), DataNode())
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert (
        " Error in ArrayRange2LoopTrans transformation. The assignment should"
        " be in a scope to create the necessary new symbols, but"
        in str(info.value))

    scope = Schedule(children=[assignment])
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert (
        "Error in ArrayRange2LoopTrans transformation. The LHS of the "
        "supplied Assignment node should be a Reference that contains an "
        "array accessor somewhere in the expression, but found "
        in str(info.value))

    # Array Reference but with acessor that resolve to a single scalar
    array_symbol = DataSymbol("x", ArrayType(INTEGER_TYPE, [10, 10]))
    one = Literal("1", INTEGER_TYPE)
    array_assignment = ArrayReference.create(array_symbol, [one, one.copy()])
    assignment = Assignment.create(array_assignment, DataNode())
    scope.addchild(assignment)
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert (
        "Error in ArrayRange2LoopTrans transformation. The LHS of the supplied"
        " Assignment node should contain an array accessor with at least one "
        "of its dimensions being a Range, but none were found in 'x(1,1) ="
        in str(info.value))


def test_validate_different_num_of_ranges(fortran_reader):
    '''Check that the validate method raises an exception when the number of
    range dimensions differ in different arrays. This should never
    happen as it is invalid Fortran, but fparser-psyir accepts it.

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
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment)
    assert ("ArrayRange2LoopTrans does not support array with array accessor "
            "with a different number of ranges in the expression, but found:"
            in str(info.value))


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
        "ArrayRange2LoopTrans does not expand ranges on character arrays "
        "by default (use the'allow_string' option to expand them), but found"
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
        trans.validate(assign, options={"verbose": True})
    errmsg = ("ArrayRange2LoopTrans does not expand ranges on character "
              "arrays by default (use the'allow_string' option to expand "
              "them)")
    assert errmsg in str(info.value)
    assert errmsg in assign.preceding_comment

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
        "ArrayRange2LoopTrans does not expand ranges on character arrays "
        "by default (use the'allow_string' option to expand them), but found"
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


def test_validate_nested_or_invalid_expressions(fortran_reader):
    ''' Check that we refuse to apply the transformation when there is more
    than one array with ranges in a single term, due to psyir representing
    invalid Fortran, or indirect mappings.'''
    trans = ArrayRange2LoopTrans()

    # Case 1: 2 array accessors in LHS and both have ranges
    # This is invalid Fortran but there are no restrictions in the PSyIR.
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        mystruct%field2(4)%field(:) = 0
    end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    assignment.lhs.member.indices[0].replace_with(Range.create(
        Literal("1", INTEGER_TYPE), Literal("10", INTEGER_TYPE)))
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment)
    assert ("ArrayRange2LoopTrans does not support array assignments that "
            "contain nested Range expressions, but found"
            in str(info.value))

    # Case 2: Nested array in another array
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        mystruct%field5(indices(:)) = 0.0d0
    end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment, options={"verbose": True})
    errmsg = ("ArrayRange2LoopTrans does not support array assignments that "
              "contain nested Range expressions")
    assert errmsg in str(info.value)
    assert errmsg in assignment.preceding_comment

    # Case 3: Nested array in another array which also have Ranges
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        umask(:,mystruct%field2%field3(:)) = &
            mystruct%field(mystruct%field2%field3(:),:)
    end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment)
    assert ("ArrayRange2LoopTrans does not support array assignments that "
            "contain nested Range expressions, but found"
            in str(info.value))


def test_validate_with_codeblock(fortran_reader):
    '''Check that the validation method of the transformation raises an
    exception if there is a Codeblock as part of the assignment.
    '''
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        x(:,:,:) = 0
    end subroutine test
    ''')
    trans = ArrayRange2LoopTrans()
    assignment = psyir.walk(Assignment)[0]
    previous_rhs = assignment.rhs.detach()
    assignment.addchild(
            BinaryOperation.create(
                BinaryOperation.Operator.ADD,
                previous_rhs,
                CodeBlock([], CodeBlock.Structure.EXPRESSION)))

    with pytest.raises(TransformationError) as info:
        trans.apply(assignment, options={"verbose": True})
    errmsg = ("ArrayRange2LoopTrans does not support array assignments that "
              "contain a CodeBlock anywhere in the expression")
    assert errmsg in str(info.value)
    assert errmsg in assignment.preceding_comment


def test_validate_rhs_plain_references(fortran_reader, fortran_writer):
    ''' If the RHS has a plain symbol reference, we need to know its type
    because if it's scalar, the value can be re-used for each iteration of
    the loop, but if it's an array, it has to be accessed properly. Therefore
    we can not transform loops with Unresolved or Unsupported types.
    '''
    psyir = fortran_reader.psyir_from_source('''
    subroutine test(unsupported)
        use my_variables, only: unresolved
        integer :: scalar = 1
        integer, dimension(:) :: array = 1, x
        integer, dimension(:), optional :: unsupported

        x(:) = scalar
        x(:) = array
        x(:) = unresolved
        x(:) = unsupported
    end subroutine test
    ''')

    opts = {"verbose": True}
    trans = ArrayRange2LoopTrans()

    # The first one succeeds
    trans.apply(psyir.walk(Assignment)[0])

    # This succeeds by internally applying the Reference2ArrayRange first
    trans.apply(psyir.walk(Assignment)[1])

    # The unresolved and unsupported fail
    assign_with_unresolved = psyir.walk(Assignment)[2]
    with pytest.raises(TransformationError) as info:
        trans.apply(assign_with_unresolved, opts)
    assert ("ArrayRange2LoopTrans cannot expand expression because it "
            "contains the variable 'unresolved' which is not a DataSymbol "
            "and therefore cannot be guaranteed to be ScalarType. Resolving "
            "the import that brings this variable into scope may help."
            in str(info.value))
    # Knowing that it is a DataSymbol is still not enough
    assign_with_unresolved.scope.symbol_table.lookup("unresolved").specialise(
        DataSymbol, datatype=UnresolvedType())
    with pytest.raises(TransformationError) as info:
        trans.apply(assign_with_unresolved)
    assert ("ArrayRange2LoopTrans cannot expand expression because it "
            "contains the variable 'unresolved' which is an UnresolvedType "
            "and therefore cannot be guaranteed to be ScalarType. Resolving "
            "the import that brings this variable into scope may help."
            in str(info.value))
    with pytest.raises(TransformationError) as info:
        trans.apply(psyir.walk(Assignment)[3], opts)
    assert ("ArrayRange2LoopTrans cannot expand expression because it "
            "contains the variable 'unsupported' which is an Unsupported"
            "FortranType('INTEGER, DIMENSION(:), OPTIONAL :: unsupported') "
            "and therefore cannot be guaranteed to be ScalarType."
            in str(info.value))

    # The end result should look like:
    assert (
        "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
        "    x(idx) = scalar\n"
        "  enddo\n"
        "  do idx_1 = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
        "    x(idx_1) = array(idx_1)\n"
        "  enddo\n"
        "  ! ArrayRange2LoopTrans cannot expand expression because it contains"
        " the variable 'unresolved' which is not a DataSymbol and therefore "
        "cannot be guaranteed to be ScalarType. Resolving the import that "
        "brings this variable into scope may help.\n"
        "  x(:) = unresolved\n"
        "  ! ArrayRange2LoopTrans cannot expand expression because it contains"
        " the variable 'unsupported' which is an UnsupportedFortranType("
        "'INTEGER, DIMENSION(:), OPTIONAL :: unsupported') and therefore "
        "cannot be guaranteed to be ScalarType.\n"
        "  x(:) = unsupported\n"
    ) in fortran_writer(psyir)


def test_validate_non_elemental_functions(fortran_reader):
    '''Check that the validate method returns an exception if the rhs of
    the assignment contains a call that is not elemental.'''
    psyir = fortran_reader.psyir_from_source('''
    module mymod
        implicit none

        contains

        real function mylocalfunc()
            mylocalfunc = 3.4
        end function

        subroutine test()
          use other, only: myfunc
          integer, dimension(:,:) :: x, y, z
          x(:) = matmul(y, z)
          x(:) = mylocalfunc(y)
          x(:) = myfunc(y)
          x(:) = var%myfunc(y)
        end subroutine test
    end module mymod
    ''')
    trans = ArrayRange2LoopTrans()
    assignment1 = psyir.walk(Assignment)[1]
    assignment2 = psyir.walk(Assignment)[2]
    assignment3 = psyir.walk(Assignment)[3]
    assignment4 = psyir.walk(Assignment)[4]

    with pytest.raises(TransformationError) as err:
        trans.validate(assignment1, options={"verbose": True})
    errormsg = ("ArrayRange2LoopTrans does not accept calls which are not "
                "guaranteed to be elemental, but found: MATMUL")
    assert errormsg in assignment1.preceding_comment
    assert errormsg in str(err.value)

    with pytest.raises(TransformationError) as err:
        trans.validate(assignment2, options={"verbose": True})
    errormsg = ("ArrayRange2LoopTrans does not accept calls which are not "
                "guaranteed to be elemental, but found: mylocalfunc")
    assert errormsg in assignment2.preceding_comment
    assert errormsg in str(err.value)

    # Sometimes, like in the two cases below, PSyclone miscategorises function
    # calls as ArrayReferences, but we still fail with a resonable error msg.
    with pytest.raises(TransformationError) as err:
        trans.validate(assignment3, options={"verbose": True})
    errormsg = ("ArrayRange2LoopTrans cannot expand expression because it "
                "contains the variable 'myfunc' which is not a DataSymbol "
                "and therefore cannot be guaranteed to be ScalarType. "
                "Resolving the import that brings this variable into scope "
                "may help.")
    assert errormsg in assignment3.preceding_comment
    assert errormsg in str(err.value)

    with pytest.raises(TransformationError) as err:
        trans.validate(assignment4, options={"verbose": True})
    errormsg = ("ArrayRange2LoopTrans cannot expand expression because it "
                "contains the variable 'var' which is an UnresolvedType "
                "and therefore cannot be guaranteed to be ScalarType. "
                "Resolving the import that brings this variable into scope "
                "may help.")
    assert errormsg in assignment4.preceding_comment
    assert errormsg in str(err.value)
