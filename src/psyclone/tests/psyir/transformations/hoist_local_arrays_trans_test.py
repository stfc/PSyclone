# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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

'''This module tests the hoist local arrays transformation.
'''

from __future__ import absolute_import, print_function
import pytest

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Literal, Loop, Assignment, Reference, \
    Routine, IfBlock, Container, FileContainer
from psyclone.psyir.symbols import (ArrayType, DataSymbol, REAL_TYPE,
                                    INTEGER_TYPE, BOOLEAN_TYPE)
from psyclone.psyir.transformations import (HoistLocalArraysTrans,
                                            TransformationError)
from psyclone.tests.utilities import Compile

# init

def test_init():
    '''Test a hoist transformation can be successfully created.'''
    hoist_trans = HoistLocalArraysTrans()
    assert isinstance(hoist_trans, HoistLocalArraysTrans)


# apply

def test_apply_1d_known(fortran_writer, tmpdir):
    '''
    Test the apply method correctly handles an automatic array of rank 1
    with known extent.

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
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
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
            "      allocate(a(1 : 10))\n"
            "    end if\n"
            "    do i = 1, 10, 1\n" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_apply_multi_dim_imported_limits(fortran_writer):
    '''
    Test that the transformation correctly handles an array with rank > 1
    and extents specified by imported variables.

    '''
    code = (
        "module my_mod\n"
        "  use some_mod\n"
        "contains\n"
        "subroutine test\n"
        "  real :: a(jpi,jpj)\n"
        "  a(:,:) = 1.0\n"
        "end subroutine test\n"
        "end module my_mod\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    code = fortran_writer(psyir).lower()
    # We cannot test the compilation of the generated code because of
    # the 'use some_mod'.
    assert "real, allocatable, dimension(:,:), private :: a\n" in code
    assert ("    if (.not.allocated(a)) then\n"
            "      allocate(a(1 : jpi, 1 : jpj))\n"
            "    end if\n"
            "    a(:,:) = 1.0\n" in code)


def test_apply_arg_limits(fortran_writer):
    '''
    Test that the transformation correctly handles an array with extents
    specified via subroutine arguments.

    '''
    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine test(nx,ny)\n"
        "  integer, intent(in) :: nx, ny\n"
        "  real :: a(nx,ny)\n"
        "  a(:,:) = 1.0\n"
        "end subroutine test\n"
        "end module my_mod\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    code = fortran_writer(psyir).lower()
    assert "real, allocatable, dimension(:,:), private :: a\n" in code
    assert ("    if (.not.allocated(a)) then\n"
            "      allocate(a(1 : nx, 1 : ny))\n"
            "    end if\n" in code)


def test_apply_multi_arrays(fortran_writer):
    '''
    Test that the transformation handles the case where we have multiple
    automatic arrays.

    '''
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
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    hoist_trans = HoistLocalArraysTrans()
    hoist_trans.apply(routine)
    code = fortran_writer(psyir).lower()
    assert "real, allocatable, dimension(:,:), private :: a" in code
    assert "integer, allocatable, dimension(:,:), private :: mask" in code
    assert (
        "    if (.not.allocated(a)) then\n"
        "      allocate(a(1 : nx, 1 : ny), mask(1 : jpi, 1 : jpj))\n"
        "    end if\n"
        "    a(:,:) = 1.0\n" in code)


def test_apply_validate():
    '''Test the apply method calls the validate method.'''
    hoist_trans = HoistLocalArraysTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.apply(None)
    assert ("The target of the HoistLocalArraysTrans transformation should be "
            "a Routine but found 'NoneType'." in str(info.value))


# validate

def test_validate_node():
    '''Test the expected exception is raised if an invalid node is
    supplied to the transformation.

    '''
    hoist_trans = HoistLocalArraysTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(None)
    assert ("The target of the HoistLocalArraysTrans transformation should be "
            "a Routine but found 'NoneType'." in str(info.value))


def test_validate_ancestor_container():
    '''Test the expected exception is raised if the supplied assignment is
    not within a container.

    '''
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
    
