# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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

'''Module containing tests for the ArrayAssignment2LoopsTrans
transformation.'''


import pytest

from psyclone.psyir.nodes import (
    BinaryOperation, ArrayReference, Assignment, Literal, Loop,
    Node, DataNode, CodeBlock, Range, Reference, Schedule)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, INTEGER_TYPE, UnresolvedType)
from psyclone.psyir.transformations import (
    ArrayAssignment2LoopsTrans, Reference2ArrayRangeTrans, TransformationError)
from psyclone.tests.utilities import Compile


def test_str():
    '''Test that the str of an instance of the ArrayAssignment2LoopsTrans class
    returns the expected value.

    '''
    assert (str(ArrayAssignment2LoopsTrans()) == "Convert a PSyIR assignment "
            "with array Ranges into explicit PSyIR Loops.")


@pytest.mark.parametrize(
        "code, expected",

        # Scalar RHS
        [("integer, dimension(:) :: x, y, z, t\n"
          "x(:) = 0.0",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = 0.0\n"),

         # Array LHS and RHS
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(:) = y(:)\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = y(idx)\n"),

         # Multi-dimensional array LHS and RHS
         ("integer, dimension(:,:,:) :: x, y, z, t\n"
          "x(:,:,:) = y(:,:,:)\n",
          "  do idx = LBOUND(x, dim=3), UBOUND(x, dim=3), 1\n"
          "    do idx_1 = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
          "      do idx_2 = LBOUND(x, dim=1), "
          "UBOUND(x, dim=1), 1\n"
          "        x(idx_2,idx_1,idx) = y(idx_2,idx_1,idx)\n"),

         # Multiple arrays on RHS
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(:) = y(:) + z(:) * t(:)\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = y(idx) + z(idx) * t(idx)\n"),

         # Argument of elemental functions are expanded
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(:) = max(y(:), z(:))\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = MAX(y(idx), z(idx))\n"),

         # Argument of inquiry functions are NOT expanded
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(:) = y + size(y)\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = y(idx) + SIZE(y)\n"),

         # Mix different array ranks with fixed indices
         ("integer, dimension(:) :: x, z, t\n"
          "integer, dimension(:,:) :: y\n"
          "x(:) = y(n,:)\n",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = y(n,idx)\n"),

         ("integer, dimension(:,:) :: x, z, t\n"
          "integer, dimension(:) :: y\n"
          "x(n,:) = y(:)\n",
          "  do idx = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
          "    x(n,idx) = y(idx)\n"),

         ("integer, dimension(:,:) :: x, z, t\n"
          "integer, dimension(:,:,:) :: y\n"
          "x(:,:)=y(:,n,:)\n",
          "  do idx = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
          "    do idx_1 = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "      x(idx_1,idx) = y(idx_1,n,idx)\n"),

         # Same rank but different range locations
         ("integer, parameter :: jpi=2, jpj=4, jpk=6, jpt=9, ndim=10\n"
          "real, dimension(jpi,jpj,jpk,jpt,ndim) :: x, y, z, t\n"
          "x(:,jpj,:,ndim,:) = y(jpi,:,:,:,ndim) + 1.0\n",
          "  do idx = LBOUND(x, dim=5), UBOUND(x, dim=5), 1\n"
          "    do idx_1 = LBOUND(x, dim=3), UBOUND(x, dim=3), 1\n"
          "      do idx_2 = LBOUND(x, dim=1), "
          "UBOUND(x, dim=1), 1\n"
          "        x(idx_2,jpj,idx_1,ndim,idx) = y(jpi,idx_2,idx_1,"
          "idx,ndim) + 1.0\n"
          "      enddo\n"
          "    enddo\n"
          "  enddo"),

         # Explicit slice values
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(2:4) = 0",
          "  do idx = 2, 4, 1\n"
          "    x(idx) = 0\n"),
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(1:1) = 0",
          "  do idx = 1, 1, 1\n"
          "    x(idx) = 0\n"),
         ("integer, dimension(:) :: x, y, z, t\n"
          "x(2:8:4) = 0",
          "  do idx = 2, 8, 4\n"
          "    x(idx) = 0\n"),

         # Explicitly declared dimension values (but generated code still
         # uses L/UBOUND which si correct).
         ("integer, dimension(2:4) :: x, y, z, t\n"
          "x(:) = 0",
          "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
          "    x(idx) = 0\n"),

         # Explicit lower bound value (assumed-shape array) - still just
         # uses LBOUND.
         ("integer, dimension(2:) :: x, y, z, t\n"
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
         ("integer :: x, y, z, t\n"
          "mystruct%soa%array(:,:,:) = 0.0d0",
          "  do idx = LBOUND(mystruct%soa%array, dim=3), "
          "UBOUND(mystruct%soa%array, dim=3), 1\n"
          "    do idx_1 = LBOUND(mystruct%soa%array, dim=2), "
          "UBOUND(mystruct%soa%array, dim=2), 1\n"
          "      do idx_2 = LBOUND(mystruct%soa%array, dim=1), "
          "UBOUND(mystruct%soa%array, dim=1), 1\n"
          "        mystruct%soa%array(idx_2,idx_1,idx) = 0.0d0\n"),

         # Array in LHS and SoA in RHS
         ("integer, dimension(:,:,:) :: x, y, z, t\n"
          "x(:,:,:) = 3 + mystruct%soa%array(:,:,:)",
          "  do idx = LBOUND(x, dim=3), UBOUND(x, dim=3), 1\n"
          "    do idx_1 = LBOUND(x, dim=2), UBOUND(x, dim=2), 1\n"
          "      do idx_2 = LBOUND(x, dim=1), "
          "UBOUND(x, dim=1), 1\n"
          # Ignore offset for this test, it is tested below
          "        x(idx_2,idx_1,idx) = 3 + mystruct%soa%array(idx_2 + "),

         # SoAoS on LHS
         ("integer :: x, y, z, t\n"
          "mystruct%aos(:,:,:)%value = 0.0d0",
          "  do idx = LBOUND(mystruct%aos, dim=3), "
          "UBOUND(mystruct%aos, dim=3), 1\n"
          "    do idx_1 = LBOUND(mystruct%aos, dim=2), "
          "UBOUND(mystruct%aos, dim=2), 1\n"
          "      do idx_2 = LBOUND(mystruct%aos, dim=1), "
          "UBOUND(mystruct%aos, dim=1), 1\n"
          "        mystruct%aos(idx_2,idx_1,idx)%value = 0.0d0\n"),

         # SoAoS in the LHS and SoA in the RHS
         ("integer :: x, y, z, t\n"
          "mystruct%aos(:,4,:)%value = mystruct%soa%array(3,:,:)",
          "  do idx = LBOUND(mystruct%aos, dim=3), "
          "UBOUND(mystruct%aos, dim=3), 1\n"
          "    do idx_1 = LBOUND(mystruct%aos, dim=1), "
          "UBOUND(mystruct%aos, dim=1), 1\n"
          # Ignore offset for this test, it is tested below
          "      mystruct%aos(idx_1,4,idx)%value = "
          "mystruct%soa%array(3,idx_1 + "),

         # 2 array accessors in each expression but only one has ranges
         ("integer :: x, y, z, t\n"
          "mystruct%aoa(4, 3)%array(:,:,:) = "
          "mystruct%aoa(5, 8)%array(:,:,:)",
          "  do idx = LBOUND(mystruct%aoa(4,3)%array, dim=3), "
          "UBOUND(mystruct%aoa(4,3)%array, dim=3), 1\n"
          "    do idx_1 = LBOUND(mystruct%aoa(4,3)%array, dim=2), "
          "UBOUND(mystruct%aoa(4,3)%array, dim=2), 1\n"
          "      do idx_2 = LBOUND(mystruct%aoa(4,3)%array, dim=1), "
          "UBOUND(mystruct%aoa(4,3)%array, dim=1), 1\n"
          # Ignore offset for this test, it is tested below
          "        mystruct%aoa(4,3)%array(idx_2,idx_1,idx) = "
          "mystruct%aoa(5,8)%array(idx_2 +")])
def test_apply(code, expected, tmpdir, fortran_reader, fortran_writer):
    '''Check that the PSyIR is transformed as expected for various types
    of ranges in an array. The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    psyir = fortran_reader.psyir_from_source(f'''
        subroutine test(x, y, z, t)
          type aos_type
             integer :: value
          end type
          type soa_type
             integer, dimension(10,10,10) :: array
          end type
          type mytype
              type(aos_type), dimension(10,10,10) :: aos
              type(soa_type) :: soa
              type(soa_type), dimension(10,10) :: aoa
          end type
          type(mytype) :: mystruct
          integer :: n
          {code}
        end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans = ArrayAssignment2LoopsTrans()
    trans.apply(assignment)
    result = fortran_writer(psyir)
    assert expected in result, f"\nExpected:\n{expected}\nBut got:\n{result}"
    assert Compile(tmpdir).string_compiles(result)


def test_apply_to_arrays_with_different_bounds(fortran_reader, fortran_writer):
    '''Check that the apply method inserts the correct index offsets when
    the lhs and rhs range bounds can not be guaranteed to be the same. '''
    psyir = fortran_reader.psyir_from_source('''
        program test
          use other

          type :: inner
              integer :: value
          end type
          type :: my_type
            integer, dimension(10) :: values
            type(inner), dimension(10) :: array
          end type

          type(my_type) :: struct
          integer, dimension(10,10) :: x2
          integer, dimension(10:20,20:30) :: y2

          ! Test 1: we don't know the bounds
          x1(:,:) = y1(:,:)

          ! Test 2: We know the bounds and they are not equal
          x2(:,:) = y2(:,:)

          ! Test 3: We know the bounds and we have explicit bounds accesses
          x2(2:4,4:6) = y2(15:17,28:30) + 1.0

          ! Test 4: SoA and SoAoS
          x(:) = struct%values(:) + struct%array(:)%value
        end program test
    ''')
    trans = ArrayAssignment2LoopsTrans()
    for assignment in psyir.walk(Assignment):
        trans.apply(assignment)

    # The bounds are not known, so L/UBOUND expressions are used
    output_test1 = fortran_writer(psyir.walk(Assignment)[0])
    assert ("x1(idx_1,idx) = y1(idx_1 + (LBOUND(y1, dim=1) "
            "- LBOUND(x1, dim=1)),idx + "
            "(LBOUND(y1, dim=2) - LBOUND(x1, dim=2)))"
            in output_test1)

    # When we know the bounds we can see they are different, we also
    # need the offsets (and can also use L/UBOUND)
    output_test2 = fortran_writer(psyir.walk(Assignment)[1])
    assert ("x2(idx_3,idx_2) = y2(idx_3 + (LBOUND(y2, dim=1) "
            "- LBOUND(x2, dim=1)),idx_2 + "
            "(LBOUND(y2, dim=2) - LBOUND(x2, dim=2)))"
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
            "LBOUND(struct%array, dim=1) - "
            "LBOUND(x, dim=1)))%value"
            in output_test4)


def test_apply_indirect_indexing(fortran_reader, fortran_writer):
    '''
    Check the application of the transformation when the array is indexed
    indirectly.

    '''
    psyir = fortran_reader.psyir_from_source('''
    program test
      use dom_oce
      INTEGER, DIMENSION(4)  ::   iwewe
      INTEGER, DIMENSION(8,kfld)  :: ishtSi
      integer :: jf
      do jf = 1, kfld
        ishtSi(5:8,jf) = ishtSi(iwewe, jf)
        ishtSi(1:4,jf) = grid(3)%var(iwewe, jf)
      end do
    end program test
    ''')
    assignments = psyir.walk(Assignment)
    trans = ArrayAssignment2LoopsTrans()
    trans.apply(assignments[0])
    trans.apply(assignments[1])
    result = fortran_writer(psyir)
    assert ('''
    do idx = 5, 8, 1
      ishtsi(idx,jf) = ishtsi(iwewe(idx + (1 - 5)),jf)
    enddo''' in result)
    assert ('''
    do idx_1 = 1, 4, 1
      ishtsi(idx_1,jf) = grid(3)%var(iwewe(idx_1),jf)
    enddo
  enddo''' in result)


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
    trans = ArrayAssignment2LoopsTrans()
    trans.apply(assignment)
    result = fortran_writer(loop)
    assert "a(idx) = a(idx) + b(idx +" in result


def test_apply_assumed_shape(fortran_reader, fortran_writer, tmpdir):
    '''Test when the underlying arrays are of assumed shape and have
    different lower bounds.'''
    code = ('''\
    subroutine sub(var, var2, istart, istart2)
      integer, intent(in) :: istart, istart2
      integer, dimension(istart:) :: var
      integer, dimension(istart2:) :: var2
      var = 2*var2
    end subroutine sub
    ''')
    psyir = fortran_reader.psyir_from_source(code)
    r2array = Reference2ArrayRangeTrans()
    assign = psyir.walk(Assignment)[0]
    for ref in assign.walk(Reference):
        r2array.apply(ref)
    trans = ArrayAssignment2LoopsTrans()
    trans.apply(assign)
    result = fortran_writer(psyir)
    assert '''do idx = istart, UBOUND(var, dim=1), 1
    var(idx) = 2 * var2(idx + (istart2 - istart))
  enddo''' in result
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.xfail(reason="TODO #2951: Dependencies are ignored")
def test_validate_with_dependency(fortran_reader):
    ''' Check that the validate method checks for dependencies (or
    resolve them using a temporary) '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
          use other_mod
          do i = 1, 10
            A(2:10) = A(1:9) + B(2:10)
          end do
        end subroutine test
    ''')
    loop = psyir.walk(Loop)[0]
    assignment = loop.walk(Assignment)[0]
    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError):
        trans.apply(assignment)


def test_apply_calls_validate():
    ''' Check that the apply() method calls the validate method.'''
    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in ArrayAssignment2LoopsTrans transformation. The supplied "
            "node should be a PSyIR Assignment, but found 'NoneType'."
            in str(info.value))


def test_validate_no_assignment_with_array_range_on_lhs():
    '''Test that the validate method in the ArrayAssignment2LoopsTrans class
    raises the expected exceptions when the provided node is not an
    array Assignment with a Range in the LHS expression.

    '''
    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(Node())
    assert (
        "Error in ArrayAssignment2LoopsTrans transformation. The supplied node"
        " should be a PSyIR Assignment, but found 'Node'."
        in str(info.value))

    assignment = Assignment.create(DataNode(), DataNode())
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert (
        "Error in ArrayAssignment2LoopsTrans transformation. The assignment "
        "should be in a scope to create the necessary new symbols, but"
        in str(info.value))

    scope = Schedule(children=[assignment])
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert (
        "Error in ArrayAssignment2LoopsTrans transformation. The LHS of the "
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
        "Error in ArrayAssignment2LoopsTrans transformation. The LHS of the "
        "supplied Assignment node should contain an array accessor with at "
        "least one of its dimensions being a Range, but none were found in "
        "'x(1,1) =" in str(info.value))


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
    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(assignment)
    assert ("ArrayAssignment2LoopsTrans does not support statements containing"
            " array accesses that have varying numbers of ranges in their "
            "accessors, but found:" in str(info.value))


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

    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assign)
    assert (
        "ArrayAssignment2LoopsTrans does not expand ranges on character arrays"
        " by default (use the'allow_string' option to expand them), but found"
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

    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assign, options={"verbose": True})
    errmsg = ("ArrayAssignment2LoopsTrans does not expand ranges on character "
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

    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assign)
    assert (
        "ArrayAssignment2LoopsTrans does not expand ranges on character arrays"
        " by default (use the'allow_string' option to expand them), but found"
        in str(info.value))


def test_unsupported_type_character(fortran_reader):
    ''' Test that the check for character references inside the assignment
    being transformed also works with 'unsupported characters arrays' (see
    issue #2612).
    '''
    code = '''subroutine test()
        character(LEN=100) :: a
        character(LEN=:) :: b
        integer, parameter :: max_len = 100
        character(LEN=max_len) :: c
        a(1:94) = b(1:94)
        a(1:94) = c(1:94)
    end subroutine test'''
    psyir = fortran_reader.psyir_from_source(code)

    for assign in psyir.walk(Assignment):
        trans = ArrayAssignment2LoopsTrans()
        with pytest.raises(TransformationError) as info:
            trans.validate(assign)
        assert (
            "ArrayAssignment2LoopsTrans does not expand ranges on character "
            "arrays by default (use the'allow_string' option to expand them), "
            "but found" in str(info.value))


def test_validate_nested_or_invalid_expressions(fortran_reader):
    ''' Check that we refuse to apply the transformation when there is more
    than one array with ranges in a single term, due to psyir representing
    invalid Fortran, or indirect mappings.'''
    trans = ArrayAssignment2LoopsTrans()

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
    assert ("ArrayAssignment2LoopsTrans does not support array assignments "
            "that contain nested Range expressions, but found"
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
    errmsg = ("ArrayAssignment2LoopsTrans does not support array assignments "
              "that contain nested Range expressions")
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
    assert ("ArrayAssignment2LoopsTrans does not support array assignments "
            "that contain nested Range expressions, but found"
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
    trans = ArrayAssignment2LoopsTrans()
    assignment = psyir.walk(Assignment)[0]
    previous_rhs = assignment.rhs.detach()
    assignment.addchild(
            BinaryOperation.create(
                BinaryOperation.Operator.ADD,
                previous_rhs,
                CodeBlock([], CodeBlock.Structure.EXPRESSION)))

    with pytest.raises(TransformationError) as info:
        trans.apply(assignment, options={"verbose": True})
    errmsg = ("ArrayAssignment2LoopsTrans does not support array assignments "
              "that contain a CodeBlock anywhere in the expression")
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
        use my_variables, only: unresolved, map
        integer :: scalar = 1
        integer, dimension(:) :: array = 1, x
        integer, dimension(:), optional :: unsupported
        integer, dimension(4) :: ishtsi
        x(:) = scalar
        x(:) = array
        x(:) = unresolved
        x(:) = unsupported
        ! An indirectly-addressed RHS but we can't tell whether or not map is
        ! an array reference
        x(:) = ishtsi(map,scalar)
    end subroutine test
    ''')

    opts = {"verbose": True}
    trans = ArrayAssignment2LoopsTrans()

    # The first one succeeds
    trans.apply(psyir.walk(Assignment)[0])

    # This succeeds by internally applying the Reference2ArrayRange first
    trans.apply(psyir.walk(Assignment)[1])

    # The unresolved and unsupported fail
    assign_with_unresolved = psyir.walk(Assignment)[2]
    with pytest.raises(TransformationError) as info:
        trans.apply(assign_with_unresolved, opts)
    assert ("ArrayAssignment2LoopsTrans cannot expand expression because it "
            "contains the access 'unresolved' which is not a DataSymbol "
            "and therefore cannot be guaranteed to be ScalarType. Resolving "
            "the import that brings this variable into scope may help."
            in str(info.value))
    # Knowing that it is a DataSymbol is still not enough
    assign_with_unresolved.scope.symbol_table.lookup("unresolved").specialise(
        DataSymbol, datatype=UnresolvedType())
    with pytest.raises(TransformationError) as info:
        trans.apply(assign_with_unresolved)
    assert ("ArrayAssignment2LoopsTrans cannot expand expression because it "
            "contains the access 'unresolved' which is an UnresolvedType "
            "and therefore cannot be guaranteed to be ScalarType. Resolving "
            "the import that brings this variable into scope may help."
            in str(info.value))
    with pytest.raises(TransformationError) as info:
        trans.apply(psyir.walk(Assignment)[3], opts)
    assert ("ArrayAssignment2LoopsTrans cannot expand expression because it "
            "contains the access 'unsupported' which is an Unsupported"
            "FortranType('INTEGER, DIMENSION(:), OPTIONAL :: unsupported') "
            "and therefore cannot be guaranteed to be ScalarType."
            in str(info.value))

    with pytest.raises(TransformationError) as info:
        trans.apply(psyir.walk(Assignment)[4], opts)
    assert ("ArrayAssignment2LoopsTrans cannot expand expression because it "
            "contains the access 'ishtsi(map,scalar)' which is an "
            "UnresolvedType and therefore cannot be guaranteed to be "
            "ScalarType" in str(info.value))

    # The end result should look like:
    assert (
        "  do idx = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
        "    x(idx) = scalar\n"
        "  enddo\n"
        "  do idx_1 = LBOUND(x, dim=1), UBOUND(x, dim=1), 1\n"
        "    x(idx_1) = array(idx_1)\n"
        "  enddo\n\n"
        "  ! ArrayAssignment2LoopsTrans cannot expand expression because it "
        "contains the access 'unresolved' which is not a DataSymbol and "
        "therefore cannot be guaranteed to be ScalarType. Resolving the import"
        " that brings this variable into scope may help.\n"
        "  x(:) = unresolved\n\n"
        "  ! ArrayAssignment2LoopsTrans cannot expand expression because it "
        "contains the access 'unsupported' which is an UnsupportedFortran"
        "Type('INTEGER, DIMENSION(:), OPTIONAL :: unsupported') and therefore "
        "cannot be guaranteed to be ScalarType.\n"
        "  x(:) = unsupported\n\n"
        "  ! ArrayAssignment2LoopsTrans cannot expand expression because it "
        "contains the access 'ishtsi(map,scalar)' which is an UnresolvedType "
        "and therefore cannot be guaranteed to be ScalarType.\n"
        "  x(:) = ishtsi(map,scalar)\n"
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
    trans = ArrayAssignment2LoopsTrans()
    assignment1 = psyir.walk(Assignment)[1]
    assignment2 = psyir.walk(Assignment)[2]
    assignment3 = psyir.walk(Assignment)[3]
    assignment4 = psyir.walk(Assignment)[4]

    # When we know for sure that they are not elemental
    with pytest.raises(TransformationError) as err:
        trans.validate(assignment1, options={"verbose": True})
    errormsg = ("ArrayAssignment2LoopsTrans does not accept calls which are "
                "not guaranteed to be elemental, but found: MATMUL")
    assert errormsg in assignment1.preceding_comment
    assert errormsg in str(err.value)

    with pytest.raises(TransformationError) as err:
        trans.validate(assignment2, options={"verbose": True})
    errormsg = ("ArrayAssignment2LoopsTrans does not accept calls which are "
                "not guaranteed to be elemental, but found: mylocalfunc")
    assert errormsg in assignment2.preceding_comment
    assert errormsg in str(err.value)

    # Also, when calls are to unresolved symbols and we don't know if they
    # are elemental or not
    with pytest.raises(TransformationError) as err:
        trans.validate(assignment3, options={"verbose": True})
    errormsg = ("ArrayAssignment2LoopsTrans does not accept calls which are "
                "not guaranteed to be elemental, but found: myfunc")
    assert errormsg in assignment3.preceding_comment
    assert errormsg in str(err.value)

    with pytest.raises(TransformationError) as err:
        trans.validate(assignment4, options={"verbose": True})
    errormsg = ("ArrayAssignment2LoopsTrans cannot expand expression because "
                "it contains the access 'var%myfunc(y)' which is an "
                "UnresolvedType and therefore cannot be guaranteed to be "
                "ScalarType. Resolving the import that brings this variable "
                "into scope may help.")
    assert errormsg in assignment4.preceding_comment
    assert errormsg in str(err.value)


def test_validate_indirect_indexing(fortran_reader):
    '''
    Check the validation of the transformation when the array is indexed
    indirectly in unsupported ways.

    '''
    psyir = fortran_reader.psyir_from_source('''
    program test
      use dom_oce
      INTEGER, DIMENSION(4)  ::   iwewe
      INTEGER, DIMENSION(8,kfld)  :: ishtSi
      ! Assignment with CodeBlock on RHS.
      iwewe(:) = (/ jpwe,jpea,jpwe,jpea /)
      ! Assignment with CodeBlock in array-index expression.
      iwewe(:) = ishtSi((/ jpwe,jpea,jpwe,jpea /), 1)
      ! Index expression does evaluate to an array but we don't currently
      ! handle getting a type in this case (TODO #1799).
      ishtSi(5:8,jf) = ishtSi(iwewe+1, jf)
      ! Index expression contains a call to an unknown function.
      ishtSi(5:8,jf) = ishtSi(my_func(1), jf)
    end program test
    ''')
    assignments = psyir.walk(Assignment)
    assert isinstance(assignments[0].rhs, CodeBlock)
    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(assignments[0])
    assert ("ArrayAssignment2LoopsTrans does not support array assignments "
            "that contain a CodeBlock anywhere in the expression"
            in str(err.value))
    with pytest.raises(TransformationError) as err:
        trans.validate(assignments[1])
    assert ("ArrayAssignment2LoopsTrans does not support array assignments "
            "that contain a CodeBlock anywhere in the expression"
            in str(err.value))
    # TODO #1799 - this requires that we extended ArrayMixin.datatype to
    # support array accesses with index expressions that are Operations.
    with pytest.raises(TransformationError) as err:
        trans.validate(assignments[2])
    assert ("cannot expand expression because it contains the access "
            "'ishtsi(iwewe + 1,jf)' which is an UnresolvedType and therefore "
            "cannot be guaranteed" in str(err.value))
    with pytest.raises(TransformationError) as err:
        trans.validate(assignments[3])
    assert ("ArrayAssignment2LoopsTrans does not accept calls which are not "
            "guaranteed to be elemental, but found: my_func"
            in str(err.value))


def test_validate_structure(fortran_reader):
    '''
    Check the validation of the transformation when it is a structure access
    on the RHS.

    '''
    psyir = fortran_reader.psyir_from_source('''
    program test
      use other
      integer, parameter :: ngrids = 4, kfld=5
      integer, dimension(8,kfld)  :: ishtSi
      type :: sub_grid_type
        integer, dimension(kfld,kfld) :: map
      end type sub_grid_type
      type :: grid_type
        real, dimension(:,:), allocatable :: data
        type(sub_grid_type), dimension(ngrids) :: subgrid
      end type grid_type
      type(grid_type) :: grid
      type(unresolved_type) :: grid1
      integer :: jf
      ! This is an array
      ishtSi(5:8,jf) = grid%data(1, jf)
      ! This is an array
      ishtSi(5:8,jf) = grid%subgrid%map(1,1)
      ! This is an array
      ishtSi(5:8,jf) = grid1%data(1, jf)
      ! This is an array
      ishtSi(5:8,jf) = grid1%subgrid%map(1,1)
    end program test
    ''')
    assignments = psyir.walk(Assignment)
    trans = ArrayAssignment2LoopsTrans()
    # We know the type of these, so they can be validated
    trans.validate(assignments[0])
    trans.validate(assignments[1])

    # We don't know the type of these
    with pytest.raises(TransformationError) as err:
        trans.validate(assignments[2])
    assert ("ArrayAssignment2LoopsTrans cannot expand expression because it "
            "contains the access 'grid1%data(1,jf)' which is an UnresolvedType"
            " and therefore cannot be guaranteed to be ScalarType"
            in str(err.value))
    with pytest.raises(TransformationError) as err:
        trans.validate(assignments[3])
    assert ("ArrayAssignment2LoopsTrans cannot expand expression because it "
            "contains the access 'grid1%subgrid%map(1,1)' which is an "
            "UnresolvedType and therefore cannot be guaranteed to be "
            "ScalarType" in str(err.value))


def test_shape_intrinsic(fortran_reader):
    '''
    Check the validation of the transformation when it has a call to an inquiry
    intrinsic which does not return a scalar.
    '''
    psyir = fortran_reader.psyir_from_source('''
    SUBROUTINE fld_map_core( pdta_read)
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pdta_read
      INTEGER,  DIMENSION(3) ::   idim_read
      idim_read(:) = SHAPE( pdta_read )
    end subroutine
    ''')
    assignments = psyir.walk(Assignment)
    trans = ArrayAssignment2LoopsTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(assignments[0])
    assert ("ArrayAssignment2LoopsTrans does not accept calls which "
            "are not guaranteed to be elemental" in str(err.value))
