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
# ----------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab

'''This module tests the hoist local arrays transformation.
'''

import pytest

from psyclone.psyir.nodes import Routine, Container, FileContainer
from psyclone.psyir.symbols import ArrayType, Symbol
from psyclone.psyir.transformations import (HoistLocalArraysTrans,
                                            TransformationError)
from psyclone.tests.utilities import Compile
from psyclone.transformations import ACCRoutineTrans

# init


def test_init():
    '''Test a hoist transformation can be successfully created.'''
    hoist_trans = HoistLocalArraysTrans()
    assert isinstance(hoist_trans, HoistLocalArraysTrans)


# apply


def test_apply_program(fortran_reader, fortran_writer, tmpdir):
    ''' Check that the apply() method does nothing if the supplied routine
    is a program. '''
    code = (
        "program test\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "  end do\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("  real, dimension(10) :: a\n\n"
            "  do i = 1, 10, 1\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_1d_known(fortran_reader, fortran_writer, tmpdir):
    ''' Test the apply method correctly handles an automatic array of rank 1
    with known extent. '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "  end do\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    orig_sym = routine.symbol_table.lookup("a")
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    # The array is no longer in the symbol table of the routine
    assert "a" not in routine.symbol_table._symbols
    # The array is now in the symbol table of the container.
    sym = routine.ancestor(Container).symbol_table.lookup("a")
    # It must be the same symbol so that any existing references aren't
    # left dangling.
    assert sym is orig_sym
    assert isinstance(sym.datatype, ArrayType)
    assert len(sym.shape) == 1
    assert sym.shape[0] == ArrayType.Extent.DEFERRED
    code = fortran_writer(psyir).lower()
    assert "real, allocatable, dimension(:), private :: a\n" in code
    assert ("    if (.not.allocated(a)) then\n"
            "      allocate(a(1:10))\n"
            "    end if\n"
            "    do i = 1, 10, 1\n" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_apply_multi_dim_imported_limits(fortran_reader, fortran_writer):
    ''' Test that the transformation correctly handles an array with rank > 1
    and extents specified by imported variables. '''
    code = (
        "module my_mod\n"
        "  use some_mod\n"
        "contains\n"
        "subroutine test\n"
        "  real :: a(jpi,jpj)\n"
        "  a(:,:) = 1.0\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    code = fortran_writer(psyir).lower()
    # We cannot test the compilation of the generated code because of
    # the 'use some_mod'.
    assert "real, allocatable, dimension(:,:), private :: a\n" in code
    assert ("    if (.not.allocated(a) .or. ubound(a, dim=1) /= jpi .or. "
            "ubound(a, dim=2) /= jpj) then\n"
            "      if (allocated(a)) then\n"
            "        deallocate(a)\n"
            "      end if\n"
            "      allocate(a(1:jpi,1:jpj))\n"
            "    end if\n"
            "    a(:,:) = 1.0\n" in code)


def test_apply_arg_limits(fortran_reader, fortran_writer, tmpdir):
    ''' Test that the transformation correctly handles an array with extents
    specified via subroutine arguments. Also checks when the lower bound
    is not unity. '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test(nx,ny)\n"
        "  integer, intent(in) :: nx, ny\n"
        "  real :: a(2:nx,3:ny)\n"
        "  a(:,:) = 1.0\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    code = fortran_writer(psyir).lower()
    assert "real, allocatable, dimension(:,:), private :: a\n" in code
    assert ("    if (.not.allocated(a) .or. ubound(a, dim=1) /= nx .or. "
            "ubound(a, dim=2) /= ny) then\n"
            "      if (allocated(a)) then\n"
            "        deallocate(a)\n"
            "      end if\n"
            "      allocate(a(2:nx,3:ny))\n"
            "    end if\n" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_apply_runtime_checks(fortran_reader, fortran_writer, tmpdir):
    ''' Test that the transformation correctly adds runtime checks for each
    boundary that is not a literal. '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test(nx,ny)\n"
        "  integer, intent(in) :: nx, ny\n"
        "  real :: a(nx,ny)\n"
        "  real :: b(nx:ny,nx:ny)\n"
        "  real :: c(3:4,5:6)\n"
        "  a(:,:) = 1.0\n"
        "  b(:,:) = 1.0\n"
        "  c(:,:) = 1.0\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    code = fortran_writer(psyir).lower()
    assert "real, allocatable, dimension(:,:), private :: a\n" in code
    assert ("    if (.not.allocated(a) .or. ubound(a, dim=1) /= nx .or. "
            "ubound(a, dim=2) /= ny) then\n"
            "      if (allocated(a)) then\n"
            "        deallocate(a)\n"
            "      end if\n"
            "      allocate(a(1:nx,1:ny))\n"
            "    end if\n" in code)
    assert ("    if (.not.allocated(b) .or. lbound(b, dim=1) /= nx .or. "
            "ubound(b, dim=1) /= ny .or. lbound(b, dim=2) /= nx .or. "
            "ubound(b, dim=2) /= ny) then\n"
            "      if (allocated(b)) then\n"
            "        deallocate(b)\n"
            "      end if\n"
            "      allocate(b(nx:ny,nx:ny))\n"
            "    end if\n" in code)
    # Unneeded inner condition is not inserted
    assert ("    if (.not.allocated(c)) then\n"
            "      allocate(c(3:4,5:6))\n"
            "    end if\n" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_apply_multi_arrays(fortran_reader, fortran_writer):
    ''' Test that the transformation handles the case where we have multiple
    automatic arrays. '''
    code = (
        "module my_mod\n"
        "use some_mod, only: jpi, jpj\n"
        "contains\n"
        "subroutine test(nx,ny)\n"
        "  integer, intent(in) :: nx, ny\n"
        "  real :: a(nx,ny)\n"
        "  integer :: mask(jpi,jpj)\n"
        "  a(:,:) = 1.0\n"
        "  mask(:,:) = 1\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    code = fortran_writer(psyir).lower()
    assert "real, allocatable, dimension(:,:), private :: a" in code
    assert "integer, allocatable, dimension(:,:), private :: mask" in code
    assert (
        "    if (.not.allocated(mask) .or. ubound(mask, dim=1) /= jpi .or. "
        "ubound(mask, dim=2) /= jpj) then\n"
        "      if (allocated(mask)) then\n"
        "        deallocate(mask)\n"
        "      end if\n"
        "      allocate(mask(1:jpi,1:jpj))\n"
        "    end if\n"
        "    if (.not.allocated(a) .or. ubound(a, dim=1) /= nx .or. "
        "ubound(a, dim=2) /= ny) then\n"
        "      if (allocated(a)) then\n"
        "        deallocate(a)\n"
        "      end if\n"
        "      allocate(a(1:nx,1:ny))\n"
        "    end if\n"
        "    a(:,:) = 1.0\n" in code)


def test_apply_name_clash(fortran_reader, fortran_writer, tmpdir):
    ''' Check that the transformation handles the case where the name of the
    symbol to be promoted already exists in the container symbol table and the
    first choice of new name clashes with a symbol in the subroutine. '''
    code = (
        "module my_mod\n"
        "  real, allocatable, dimension(:,:), private :: a\n"
        "contains\n"
        "subroutine test(nx,ny)\n"
        "  integer, intent(in) :: nx, ny\n"
        "  real :: a(nx,ny)\n"
        "  real :: a_1 = 1.0\n"
        "  a(:,:) = a_1\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    code = fortran_writer(psyir).lower()
    assert ("  real, allocatable, dimension(:,:), private :: a\n"
            "  real, allocatable, dimension(:,:), private :: a_2\n" in code)
    assert ("    if (.not.allocated(a_2) .or. ubound(a_2, dim=1) /= nx .or. "
            "ubound(a_2, dim=2) /= ny) then\n"
            "      if (allocated(a_2)) then\n"
            "        deallocate(a_2)\n"
            "      end if\n"
            "      allocate(a_2(1:nx,1:ny))\n"
            "    end if\n"
            "    a_2(:,:) = a_1\n" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_apply_validate():
    '''Test the apply method calls the validate method.'''
    hoist_trans = HoistLocalArraysTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.apply(None)
    assert ("The target of the HoistLocalArraysTrans transformation should be "
            "a Routine but found 'NoneType'." in str(info.value))


def test_apply_no_arrays(fortran_reader, fortran_writer, tmpdir):
    ''' Check that applying the transformation to a routine that does not
    contain any local arrays does nothing. '''
    code = (
        "module my_mod\n"
        "real :: a(10,10)\n"
        "contains\n"
        "subroutine test(nx,ny,b)\n"
        "  integer, intent(in) :: nx, ny\n"
        "  real, dimension(nx,ny), intent(in) :: b\n"
        "  a(:,:) = b(:,:)\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    output = fortran_writer(psyir).lower()
    assert ("    real, dimension(nx,ny), intent(in) :: b\n\n"
            "    a(:,:) = b(:,:)\n\n"
            "  end subroutine test\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_tagged_symbol(fortran_reader):
    ''' Check that any tag associated with the Symbol representing a local
    array is preserved during its promotion. '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "  end do\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    orig_sym = routine.symbol_table.lookup("a")
    # Add a tag for this symbol.
    routine.symbol_table._tags["important_tag"] = orig_sym
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    cont = psyir.children[0]
    assert isinstance(cont, Container)
    sym = cont.symbol_table.lookup_with_tag("important_tag")
    assert sym is orig_sym
    # Check that the tag has also been removed from the routine symbol table.
    assert "important_tag" not in routine.symbol_table.tags_dict


def test_apply_array_valued_function(fortran_reader, fortran_writer, tmpdir):
    ''' Check that the transformation does not attempt to hoist an array
    if it is the return value of a routine. '''
    code = (
        "module my_mod\n"
        "contains\n"
        "function test() result(a)\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "  end do\n"
        "end function test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    real, dimension(10) :: a\n\n"
            "    do i = 1, 10, 1\n" in output)
    assert Compile(tmpdir).string_compiles(output)


# _get_local_arrays

def test_get_local_arrays(fortran_reader):
    ''' Check that the _get_local_arrays() helper method works correctly for
    a routine containing arrays that are passed by argument, imported from a
    container or used as a return value. '''
    code = (
        "module my_mod\n"
        "contains\n"
        "function test(c) result(a)\n"
        "  use some_mod, only: b\n"
        "  real, dimension(10,10), intent(in) :: c\n"
        "  real, dimension(10) :: wrk\n"
        "  real, dimension(:), allocatable :: wrk2\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = b(i) + c(i,5)\n"
        "  end do\n"
        "end function test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    symbols = hoist_trans._get_local_arrays(routine)
    assert len(symbols) == 1
    assert symbols[0] is routine.symbol_table.lookup("wrk")


def test_get_local_arrays_codeblock(fortran_reader):
    ''' Check that the _get_local_arrays() method excludes any of the
    local arrays if they are accessed within a CodeBlock (since they
    may get renamed as part of the hoisting process). We check for the
    situation where we have more than one CodeBlock and the same symbol
    is referenced in both. '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test\n"
        "  integer :: i\n"
        "  real :: a(10), b(10)\n"
        "  a(:) = 1.0\n"
        "  write(*,*) a(10)\n"
        "  b(:) = 1.0\n"
        "  write(*,*) b(1),a(1)\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    assert hoist_trans._get_local_arrays(routine) == []
    # TODO #11. Once logging is implemented check that the exclusion of 'a'
    # and 'b' has been logged.


def test_get_local_arrays_not_parameters(fortran_reader):
    '''Check that the _get_local_arrays() helper method ignores any local
    arrays that are parameters.

    '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test\n"
        "  integer :: i\n"
        "  real, dimension(2), parameter :: a = (/1.0, 2.0/)\n"
        "  real :: b(2)\n"
        "  do i=1,2\n"
        "    b(i) = 2.0*a(i)\n"
        "  end do\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    symbols = hoist_trans._get_local_arrays(routine)
    assert len(symbols) == 1
    assert symbols[0].name == "b"


# validate

def test_validate_node():
    ''' Test the expected exception is raised if an invalid node is
    supplied to the transformation. '''
    hoist_trans = HoistLocalArraysTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(None)
    assert ("The target of the HoistLocalArraysTrans transformation should be "
            "a Routine but found 'NoneType'." in str(info.value))


def test_validate_program():
    ''' Test that the validate method accepts a Routine that is a Program
    (since there's nothing to do). '''
    routine = Routine("my_prog", is_program=True)
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.validate(routine)


def test_validate_ancestor_container():
    ''' Test the expected exception is raised if the supplied assignment is
    not within a container. '''
    hoist_trans = HoistLocalArraysTrans()
    routine = Routine("my_prog")
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(routine)
    assert ("The supplied routine 'my_prog' should be within a Container but "
            "none was found." in str(info.value))
    container = FileContainer("my_file")
    container.addchild(routine)
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(routine)
    assert ("The supplied routine 'my_prog' should be within a Container but "
            "the enclosing container is a FileContainer (named 'my_file')."
            in str(info.value))


def test_validate_acc_routine_directive(fortran_reader):
    '''
    Test that, by default, the transformation rejects a routine if it contains
    an ACCRoutineDirective and that this can be switched off.

    '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "  end do\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    acc_trans = ACCRoutineTrans()
    acc_trans.apply(routine)
    hoist_trans = HoistLocalArraysTrans()
    with pytest.raises(TransformationError) as err:
        hoist_trans.validate(routine)
    assert ("supplied routine 'test' contains an ACC Routine directive "
            "which implies" in str(err.value))
    # This check can be disabled.
    hoist_trans.validate(routine, options={"allow_accroutine": True})


def test_validate_tagged_symbol_clash(fortran_reader):
    ''' Check that we get the expected error message if the tag associated
    with the Symbol representing a local array clashes with a tag already
    present in the outer scope. '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "  end do\n"
        "end subroutine test\n"
        "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    cont = psyir.children[0]
    routine = psyir.walk(Routine)[0]
    orig_sym = routine.symbol_table.lookup("a")
    # Add a Symbol to the outer table with a clashing tag.
    cont.symbol_table.add(Symbol("b"), tag="important_tag")
    orig_sym = routine.symbol_table.lookup("a")
    # Add a tag for this symbol.
    routine.symbol_table._tags["important_tag"] = orig_sym
    hoist_trans = HoistLocalArraysTrans()
    with pytest.raises(TransformationError) as err:
        hoist_trans.validate(routine)
    assert ("The supplied routine 'test' contains a local array 'a' with tag "
            "'important_tag' but this tag is also present in the symbol table "
            "of the parent Container (associated with variable 'b')" in
            str(err.value))


# str


def test_str_method():
    ''' Test for the __str__ method of HoistLocalArraysTrans. '''
    hoist_trans = HoistLocalArraysTrans()
    assert (str(hoist_trans) == "Hoist all local, automatic arrays to "
            "container scope.")
