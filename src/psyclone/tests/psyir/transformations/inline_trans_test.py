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

'''This module tests the inlining transformation.
'''

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import Call, Routine
from psyclone.psyir.transformations import (InlineTrans,
                                            TransformationError)
from psyclone.tests.utilities import Compile

# init


def test_init():
    '''Test an InlineTrans transformation can be successfully created.'''
    hoist_trans = InlineTrans()
    assert isinstance(hoist_trans, InlineTrans)


# apply


def test_apply_empty_routine(fortran_reader, fortran_writer, tmpdir):
    '''Check that a call to an empty routine is simply removed.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n\n"
            "  end subroutine run_it\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_single_return(fortran_reader, fortran_writer, tmpdir):
    '''Check that a call to a routine containing only a return statement
    is removed. '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    return\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n\n"
            "  end subroutine run_it\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_return_then_cb(fortran_reader, fortran_writer, tmpdir):
    '''Check that a call to a routine containing a return statement followed
    by a CodeBlock is removed.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    return\n"
        "    write(*,*) idx\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n\n"
            "  end subroutine run_it\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_array_arg(fortran_reader, fortran_writer, tmpdir):
    ''' Check that the apply() method works correctly for a very simple
    call to a routine with an array reference as argument. '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "    call sub(a(i))\n"
        "  end do\n"
        "  end subroutine run_it\n"
        "  subroutine sub(x)\n"
        "    real, intent(inout) :: x\n"
        "    x = 2.0*x\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    do i = 1, 10, 1\n"
            "      a(i) = 1.0\n"
            "      a(i) = 2.0 * a(i)\n"
            "    enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_name_clash(fortran_reader, fortran_writer, tmpdir):
    ''' Check that apply() correctly handles the case where a symbol
    in the routine to be in-lined clashes with an existing symbol. '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    real :: y\n"
        "    i = 10\n"
        "    y = 1.0\n"
        "    call sub(y)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(x)\n"
        "    real, intent(inout) :: x\n"
        "    real :: i\n"
        "    i = 3.0\n"
        "    x = 2.0*x + i\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n"
            "    y = 1.0\n"
            "    i_1 = 3.0\n"
            "    y = 2.0 * y + i_1\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_imported_symbols(fortran_reader, fortran_writer):
    '''Test that the apply method correctly handles imported symbols in the
    routine being inlined. '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use some_mod, only: var2\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = 3*var2\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use some_mod, only : var2\n"
            "    integer :: i\n\n"
            "    i = 10\n"
            "    i = 3 * var2\n" in output)
    # We can't check this with compilation because of the import of some_mod.


def test_apply_last_stmt_is_return(fortran_reader, fortran_writer, tmpdir):
    '''Test that the apply method correctly omits any final 'return'
    statement that may be present in the routine to be inlined.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = idx + 3\n"
        "    return\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n"
            "    i = i + 3\n\n"
            "  end subroutine run_it\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_call_args(fortran_reader, fortran_writer):
    '''Check that apply works correctly if any of the actual
    arguments are not simple references.'''
    code = (
        "module test_mod\n"
        " use kinds_mod, only: i_def\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i, 2*i, 5_i_def)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx, incr1, incr2)\n"
        "    integer, intent(inout) :: idx\n"
        "    integer, intent(in) :: incr1\n"
        "    integer(kind=i_def), intent(in) :: incr2\n"
        "    idx = idx + incr1 * incr2\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("    i = 10\n"
            "    i = i + 2 * i * 5_i_def\n\n"
            "  end subroutine run_it\n" in output)
    # Cannot test for compilation because of 'kinds_mod'.


def test_apply_duplicate_imports(fortran_reader, fortran_writer):
    '''Check that apply works correctly when the routine to be inlined
    imports symbols from a container that is also accessed in the
    calling routine.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: i_def\n"
        "  integer :: i\n"
        "  i = 10_i_def\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod, only : i_def\n"
            "    integer :: i\n\n" in output)
    assert ("    i = 10_i_def\n"
            "    i = i + 5_i_def\n\n"
            "  end subroutine run_it\n" in output)
    # Cannot test for compilation because of 'kinds_mod'.


def test_apply_wildcard_import(fortran_reader, fortran_writer):
    '''Check that apply works correctly when a wildcard import is present
    in the routine to be inlined.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: i_def\n"
        "  integer :: i\n"
        "  i = 10_i_def\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use kinds_mod\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod\n"
            "    integer :: i\n\n" in output)
    # Cannot test for compilation because of 'kinds_mod'.


def test_apply_import_union(fortran_reader, fortran_writer):
    '''Test that the apply method works correctly when the set of symbols
    imported from a given container is not the same as that imported into
    the scope of the call site.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: r_def\n"
        "  integer :: i\n"
        "  i = 10.0_r_def\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod, only : i_def, r_def\n"
            "    integer :: i\n\n" in output)
    assert ("    i = 10.0_r_def\n"
            "    i = i + 5_i_def\n" in output)
    # Cannot test for compilation because of 'kinds_mod'.


def test_apply_callsite_rename(fortran_reader, fortran_writer):
    '''Check that a symbol import in the routine causes a
    rename of a symbol that is local to the *calling* scope.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: r_def\n"
        "  integer :: i, a_clash\n"
        "  a_clash = 2\n"
        "  i = 10.0_r_def\n"
        "  call sub(i)\n"
        "  i = i * a_clash\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use a_mod, only: a_clash\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def + a_clash\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod, only : i_def, r_def\n"
            "    use a_mod, only : a_clash\n"
            "    integer :: i\n"
            "    integer :: a_clash_1\n\n"
            "    a_clash_1 = 2\n"
            "    i = 10.0_r_def\n"
            "    i = i + 5_i_def + a_clash\n"
            "    i = i * a_clash_1\n" in output)


def test_apply_callsite_rename_container(fortran_reader, fortran_writer):
    '''Check that an import from a container in the routine causes a
    rename of a symbol that is local to the *calling* scope.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: r_def\n"
        "  integer :: i, a_mod\n"
        "  a_mod = 2\n"
        "  i = 10.0_r_def\n"
        "  call sub(i)\n"
        "  i = i * a_mod\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use a_mod, only: a_clash\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def + a_clash\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod, only : i_def, r_def\n"
            "    use a_mod, only : a_clash\n"
            "    integer :: i\n"
            "    integer :: a_mod_1\n\n"
            "    a_mod_1 = 2\n"
            "    i = 10.0_r_def\n"
            "    i = i + 5_i_def + a_clash\n"
            "    i = i * a_mod_1\n" in output)


def test_inline_local_symbols_check(fortran_reader):
    '''Test the internal consistency check within _inline_local_symbols.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    use a_mod, only: a_clash\n"
        "    use kinds_mod, only: r_def\n"
        "    integer :: i, a_var\n"
        "    a_var = a_clash\n"
        "    i = 10.0_r_def\n"
        "    call sub(i)\n"
        "    i = i * a_var\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use a_mod, only: a_clash\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def + a_clash\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routines = psyir.walk(Routine)
    caller = routines[0]
    callee = routines[1]
    inline_trans = InlineTrans()
    with pytest.raises(InternalError) as err:
        inline_trans._inline_local_symbols(caller.symbol_table,
                                           callee.symbol_table, {})
    assert ("Symbol 'a_clash' imported from 'a_mod' has not been updated to "
            "refer to that container at the call site." in str(err.value))


def test_inline_non_local_import(fortran_reader, fortran_writer):
    '''Test that we correctly handle the case where the routine to be
    inlined accesses a symbol from an import in its parent container.'''
    code = (
        "module test_mod\n"
        "  use some_mod, only: trouble\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = idx + trouble\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    integer :: i\n\n"
            "    i = 10\n"
            "    i = i + trouble\n" in output)


def test_apply_validate():
    '''Test the apply method calls the validate method.'''
    hoist_trans = InlineTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.apply(None)
    assert ("The target of the InlineTrans transformation should be "
            "a Call but found 'NoneType'." in str(info.value))


# validate

def test_validate_node():
    ''' Test the expected exception is raised if an invalid node is
    supplied to the transformation. '''
    hoist_trans = InlineTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(None)
    assert ("The target of the InlineTrans transformation should be "
            "a Call but found 'NoneType'." in str(info.value))


def test_validate_calls_find_routine(fortran_reader):
    '''Test that validate() calls the _find_routine method.
raises the expected error if the source of the
    routine to be inlined cannot be found.'''
    code = (
        "module test_mod\n"
        "  use some_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' is imported and therefore cannot currently be "
            "inlined - TODO #924" in str(err.value))


def test_validate_return_stmt(fortran_reader):
    '''Test that validate() raises the expected error if the target routine
    contains one or more Returns which that aren't either the very first
    statement or very last statement.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = 3\n"
        "    return\n"
        "    idx = idx + 3\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' contains one or more Return statements and "
            "therefore cannot be inlined" in str(err.value))


def test_validate_codeblock(fortran_reader):
    '''Test that validate() raises the expected error for a routine that
    contains a CodeBlock.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    write(*,*) idx\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' contains one or more CodeBlocks and therefore "
            "cannot be inlined" in str(err.value))


def test_validate_import_clash(fortran_reader):
    '''Test that validate() raises the expected error when two symbols of the
    same name are imported from different containers at the call site and
    within the routine.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    use some_mod, only: trouble\n"
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use other_mod, only: trouble\n"
        "    integer :: idx\n"
        "    idx = idx + trouble\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' imports 'trouble' from Container 'other_mod' but "
            "the call site has an import of a symbol with the same name from "
            "Container 'some_mod'" in str(err.value))


def test_validate_non_local_symbol(fortran_reader):
    '''Test that validate() raises the expected error when the routine to be
    inlined accesses a symbol from its parent container.'''
    code = (
        "module test_mod\n"
        "  integer :: trouble\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = idx + trouble\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' cannot be inlined because it accesses variable "
            "'trouble' from its parent container" in str(err.value))


# _find_routine

def test_find_routine_missing(fortran_reader):
    '''Test that _find_routine() raises the expected error if the source of the
    called routine cannot be found.'''
    code = (
        "module test_mod\n"
        "  use some_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans._find_routine(call)
    assert ("Routine 'sub' is imported and therefore cannot currently be "
            "inlined - TODO #924" in str(err.value))


def test_find_routine_missing_implementation(fortran_reader):
    '''Test that _find_routine() raises the expected error if the RoutineSymbol
    is local but the implementation cannot be found.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    # Break the PSyIR by removing the Routine itself.
    routine = psyir.walk(Routine)[1]
    routine.detach()
    inline_trans = InlineTrans()
    with pytest.raises(InternalError) as err:
        inline_trans._find_routine(call)
    assert ("Failed to find the source for routine 'sub' and therefore "
            "cannot inline it." in str(err.value))
