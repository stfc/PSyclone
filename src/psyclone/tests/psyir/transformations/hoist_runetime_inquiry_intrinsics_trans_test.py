# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''Tests for extraction and safe movement of run-time array inquiries.'''
import pytest

from psyclone.psyir.nodes import Assignment, IntrinsicCall, Loop, Routine
from psyclone.psyir.transformations import (
    HoistRunetimeInquiryIntrinsicsTrans, TransformationError)


def transform(reader, writer, body, declarations='', local=False):
    '''Build and transform a routine containing the supplied statements.'''
    tree = reader.psyir_from_source(f'''
        subroutine test(a)
        real, allocatable :: a(:)
        integer :: i, j, x, d
        {declarations}
        {body}
        end subroutine
    ''')
    HoistRunetimeInquiryIntrinsicsTrans().apply(
        tree.walk(Routine)[0], assume_reallocations_are_local=local)
    return tree, writer(tree)


def test_reuse(fortran_reader, fortran_writer):
    '''Repeated inquiries share one temporary outside nested loops.'''
    tree, code = transform(fortran_reader, fortran_writer, '''
        do i=1,10
          do j=1,10
            a(j)=0
            x=ubound(a,1)+3+ubound(a,1)
          enddo
        enddo''')
    assert len(tree.walk(IntrinsicCall)) == 1
    assert 'x = ubound_a_1 + 3 + ubound_a_1' in code
    assert code.index('ubound_a_1 = UBOUND') < code.index('do i')


@pytest.mark.parametrize('change', [
    'deallocate(a)\nallocate(a(20))', 'a = [1., 2.]',
    'call move_alloc(b,a)'])
def test_reallocation(fortran_reader, fortran_writer, change):
    '''Explicit and automatic reallocations prevent reuse and loop exit.'''
    tree, code = transform(fortran_reader, fortran_writer, f'''
        do i=1,10
          x=size(a)
          {change}
          x=size(a)
        enddo''', 'real, allocatable :: b(:)')
    sizes = [c for c in tree.walk(IntrinsicCall)
             if c.intrinsic == IntrinsicCall.Intrinsic.SIZE]
    assert len(sizes) == 2
    assert all(c.ancestor(Loop) for c in sizes)
    assert code.index('size_a = SIZE') > code.lower().index(
        change.split('(')[0])


@pytest.mark.parametrize('local', [False, True])
def test_calls(fortran_reader, fortran_writer, local):
    '''The opt-in assumption permits movement past an indirect impure call.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        do i=1,10
          call unknown()
          x=size(a)
        enddo''', local=local)
    size = next(c for c in tree.walk(IntrinsicCall)
                if c.intrinsic == IntrinsicCall.Intrinsic.SIZE)
    assert bool(size.ancestor(Loop)) is not local


def test_local_actual_argument(fortran_reader, fortran_writer):
    '''Calls receiving the array remain barriers under the local assumption.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        do i=1,10
          call unknown(a)
          x=size(a)
        enddo''', local=True)
    assert tree.walk(IntrinsicCall)[0].ancestor(Loop)


def test_pointer(fortran_reader, fortran_writer):
    '''Pointer reassociation is a local descriptor change.'''
    tree, code = transform(fortran_reader, fortran_writer, '''
        do i=1,10
          p => b
          x=ubound(p,1)
        enddo''', 'real, pointer :: p(:)\nreal, target :: b(10)')
    assert tree.walk(IntrinsicCall)[0].ancestor(Loop)
    assert code.index('p => b') < code.index('ubound_p_1 = UBOUND')


def test_dim_dependency(fortran_reader, fortran_writer):
    '''DIM values cannot move past definitions or out of their loop.'''
    tree, code = transform(fortran_reader, fortran_writer, '''
        do i=1,10
          d=1
          x=ubound(a,d)
          d=2
        enddo''')
    assert tree.walk(IntrinsicCall)[0].ancestor(Loop)
    assert code.index('d = 1') < code.index('ubound_a_d = UBOUND')


def test_conditional(fortran_reader, fortran_writer):
    '''An inquiry must stay within a guard that ensures allocation.'''
    _, code = transform(fortran_reader, fortran_writer, '''
        if (allocated(a)) then
          x=size(a)
        endif''')
    assert code.index('if (allocated_a)') < code.index('size_a = SIZE')


def test_static_and_scalar(fortran_reader, fortran_writer):
    '''Known array bounds and scalar type inquiries are left intact.'''
    tree, _ = transform(fortran_reader, fortran_writer,
                        'x=size(b)+kind(x)', 'real :: b(10)')
    assert len(tree.walk(Assignment)) == 1


def test_region(fortran_reader, fortran_writer):
    '''Selected inquiries can move outside the supplied region.'''
    tree = fortran_reader.psyir_from_source('''
        subroutine t(a)
        real :: a(:)
        integer :: x
        x=size(a)
        x=lbound(a,1)
        end''')
    HoistRunetimeInquiryIntrinsicsTrans().apply(tree.walk(Assignment)[1])
    code = fortran_writer(tree)
    assert 'x = SIZE(a)' in code
    assert code.index('lbound_a_1 = LBOUND') < code.index('x = SIZE(a)')


def test_validation(fortran_reader):
    '''Invalid targets and options fail; empty regions are safe.'''
    trans = HoistRunetimeInquiryIntrinsicsTrans()
    assert str(trans) == 'Hoist run-time array inquiry intrinsics'
    trans.apply([])
    with pytest.raises(TransformationError):
        trans.apply('invalid')
    with pytest.raises(TypeError):
        trans.apply([], assume_reallocations_are_local='yes')
    tree = fortran_reader.psyir_from_source('program t\nend program')
    trans.apply(tree.walk(Routine)[0])


def test_multiple_inquiries(fortran_reader, fortran_writer):
    '''Other inquiries do not prevent reuse of an existing definition.'''
    tree, code = transform(fortran_reader, fortran_writer,
                           'x=size(a)+lbound(a,1)+size(a)')
    assert len(tree.walk(IntrinsicCall)) == 2
    assert 'x = size_a + lbound_a_1 + size_a' in code


@pytest.mark.parametrize('declaration', [
    'real :: a(:,:)', 'use arrays, only: a'])
def test_vector_result(fortran_reader, fortran_writer, declaration):
    '''Vector inquiries work for known and unresolved ranks.'''
    tree = fortran_reader.psyir_from_source(f'''
        subroutine t()
        {declaration}
        integer, allocatable :: x(:)
        x=ubound(a)
        end''')
    HoistRunetimeInquiryIntrinsicsTrans().apply(tree.walk(Routine)[0])
    code = fortran_writer(tree)
    assert 'ubound_a = UBOUND(a)' in code
    assert 'x = ubound_a' in code


def test_loop_bound(fortran_reader, fortran_writer):
    '''Inquiries in loop limits are evaluated before the loop body.'''
    tree, code = transform(fortran_reader, fortran_writer, '''
        do i=1,size(a)
          a(i)=0
        enddo''')
    assert not tree.walk(IntrinsicCall)[0].ancestor(Loop)
    assert 'do i = 1, size_a, 1' in code


def test_later_iteration_change(fortran_reader, fortran_writer):
    '''A later change in the loop prevents hoisting from earlier iterations.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        do i=1,10
          x=size(a)
          deallocate(a)
          allocate(a(i))
        enddo''')
    assert tree.walk(IntrinsicCall)[0].ancestor(Loop)


def test_codeblock(fortran_reader, fortran_writer):
    '''Opaque statements prevent movement, even with the local assumption.'''
    _, code = transform(fortran_reader, fortran_writer,
                        'print *, "hello"\nx=size(a)', local=True)
    assert code.index('PRINT') < code.index('size_a = SIZE')


def test_return_guard(fortran_reader, fortran_writer):
    '''Do not speculate an inquiry above an allocation guard and return.'''
    _, code = transform(fortran_reader, fortran_writer, '''
        if (.not. allocated(a)) return
        x=size(a)''')
    assert code.index('return') < code.index('size_a = SIZE')


def test_while_condition(fortran_reader, fortran_writer):
    '''A while condition must still be evaluated on every iteration.'''
    _, code = transform(fortran_reader, fortran_writer, '''
        do while (size(a)>1)
          a=a(:size(a)-1)
        enddo''')
    assert 'do while (SIZE(a) > 1)' in code


def test_nested_inquiry(fortran_reader, fortran_writer):
    '''Nested inquiries are extracted in dependency order.'''
    tree, code = transform(fortran_reader, fortran_writer,
                           'x=ubound(a,dim=size(b))', 'real :: b(:)')
    assert len(tree.walk(IntrinsicCall)) == 2
    assert code.index('size_b = SIZE') < code.index('= UBOUND')


def test_loop_body_reallocation(fortran_reader, fortran_writer):
    '''Body changes do not prevent extracting a loop-bound inquiry.'''
    _, code = transform(fortran_reader, fortran_writer, '''
        do i=1,size(a)
          deallocate(a)
          allocate(a(i))
        enddo''')
    assert code.index('size_a = SIZE') < code.index('do i')


def test_constant_bounds(fortran_reader, fortran_writer):
    '''Parameter bounds and constant arithmetic are compile-time bounds.'''
    tree, _ = transform(fortran_reader, fortran_writer, 'x=size(b)',
                        'integer, parameter :: n=10\nreal :: b(2*n)')
    assert len(tree.walk(Assignment)) == 1
