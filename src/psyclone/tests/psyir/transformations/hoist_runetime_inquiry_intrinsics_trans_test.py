# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''Tests for extraction and safe movement of run-time array inquiries.'''
import pytest

from psyclone.psyir.nodes import (
    Assignment, IfBlock, IntrinsicCall, Loop, Routine)
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


@pytest.mark.parametrize('body', [
    '''if (i>0) then
         if (j>0) then
           x=size(a)
         endif
       endif''',
    '''if (i>0) then
         x=0
       else
         x=size(a)
       endif''',
    '''do i=1,10
         if (j>0) then
           x=size(a)
         endif
       enddo''',
    '''if (j>0) then
         do i=1,10
           x=size(a)
         enddo
       endif'''])
def test_hoist_if_to_routine(fortran_reader, fortran_writer, body):
    '''Cross then/else bodies and mixed nesting all the way to the routine.'''
    tree, _ = transform(fortran_reader, fortran_writer, body)
    inquiry = tree.walk(IntrinsicCall)[0]
    routine = tree.walk(Routine)[0]
    assert inquiry.parent is routine.children[0]


def test_if_reuse(fortran_reader, fortran_writer):
    '''Both branches share a definition hoisted before their condition.'''
    tree, code = transform(fortran_reader, fortran_writer, '''
        if (i>0) then
          x=size(a)
        else
          x=size(a)+size(a)
        endif''')
    assert len(tree.walk(IntrinsicCall)) == 1
    routine = tree.walk(Routine)[0]
    assert tree.walk(IntrinsicCall)[0].parent is routine.children[0]
    assert 'x = size_a + size_a' in code


@pytest.mark.parametrize('change', ['allocate(a(10))', 'd=1'])
def test_if_preceding_definition(fortran_reader, fortran_writer, change):
    '''A required definition in a branch prevents hoisting outside it.'''
    tree, _ = transform(fortran_reader, fortran_writer, f'''
        if (i>0) then
          {change}
          x=ubound(a,d)
        endif''')
    assert tree.walk(IntrinsicCall)[-1].ancestor(IfBlock)


def test_if_later_change(fortran_reader, fortran_writer):
    '''Later changes in either branch do not prevent hoisting before the if.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        if (i>0) then
          x=size(a)
          deallocate(a)
        else
          deallocate(a)
        endif''')
    routine = tree.walk(Routine)[0]
    assert tree.walk(IntrinsicCall)[0].parent is routine.children[0]


@pytest.mark.parametrize('local', [False, True])
def test_if_condition_call(fortran_reader, fortran_writer, local):
    '''Condition side effects respect the assumption about reallocations.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        if (test_flag()) then
          x=size(a)
        endif''', 'logical, external :: test_flag', local=local)
    assert bool(tree.walk(IntrinsicCall)[0].ancestor(IfBlock)) is not local


@pytest.mark.parametrize('guard', ['allocated(a)', 'd>0'])
def test_if_protecting_condition(fortran_reader, fortran_writer, guard):
    '''Descriptor and DIM guards remain outside dependent inquiries.'''
    tree, _ = transform(fortran_reader, fortran_writer, f'''
        if ({guard}) then
          if (i>0) then
            x=ubound(a,d)
          endif
        endif''')
    inquiry = next(call for call in tree.walk(IntrinsicCall)
                   if call.intrinsic == IntrinsicCall.Intrinsic.UBOUND)
    outer_if = tree.walk(IfBlock)[0]
    assert inquiry.parent is outer_if.if_body.children[0]


def test_if_pointer_guard(fortran_reader, fortran_writer):
    '''Association guards are retained when crossing inner if bodies.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        if (associated(p)) then
          if (i>0) then
            x=size(p)
          endif
        endif''', 'real, pointer :: p(:)', local=True)
    inquiry = next(call for call in tree.walk(IntrinsicCall)
                   if call.intrinsic == IntrinsicCall.Intrinsic.SIZE)
    assert inquiry.parent is tree.walk(IfBlock)[0].if_body.children[0]


def test_if_hoist_stops_inside_loop(fortran_reader, fortran_writer):
    '''Cross an if but stay after an allocation in the enclosing loop.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        do i=1,10
          allocate(a(i))
          if (j>0) then
            x=size(a)
          endif
          deallocate(a)
        enddo''')
    inquiry = next(call for call in tree.walk(IntrinsicCall)
                   if call.intrinsic == IntrinsicCall.Intrinsic.SIZE)
    loop = tree.walk(Loop)[0]
    assert inquiry.parent is loop.loop_body.children[1]


def test_if_selected_assignment(fortran_reader, fortran_writer):
    '''A selected assignment can hoist inquiries beyond its enclosing if.'''
    tree = fortran_reader.psyir_from_source('''
        subroutine test(a, flag)
        real :: a(:)
        logical :: flag
        integer :: x
        if (flag) then
          x=size(a)
        endif
        end''')
    HoistRunetimeInquiryIntrinsicsTrans().apply(tree.walk(Assignment)[0])
    routine = tree.walk(Routine)[0]
    assert tree.walk(IntrinsicCall)[0].parent is routine.children[0]
    assert fortran_writer(tree).index('size_a = SIZE') < \
        fortran_writer(tree).index('if (flag)')


def test_if_optional_guard(fortran_reader):
    '''Optional arguments remain inside their presence guards.'''
    tree = fortran_reader.psyir_from_source('''
        subroutine test(a)
        real, optional :: a(:)
        integer :: x
        if (present(a)) then
          x=size(a)
        endif
        end''')
    HoistRunetimeInquiryIntrinsicsTrans().apply(tree.walk(Routine)[0])
    inquiry = next(call for call in tree.walk(IntrinsicCall)
                   if call.intrinsic == IntrinsicCall.Intrinsic.SIZE)
    assert inquiry.ancestor(IfBlock)


def test_if_pointer_assignment(fortran_reader, fortran_writer):
    '''A pointer assignment inside the branch is still a hoisting barrier.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        if (i>0) then
          p => b
          x=size(p)
        endif''', 'real, pointer :: p(:)\nreal, target :: b(10)')
    inquiry = tree.walk(IntrinsicCall)[0]
    assert inquiry.parent is tree.walk(IfBlock)[0].if_body.children[1]


@pytest.mark.parametrize('guarded', [False, True])
def test_final_schedule_after_extraction(fortran_reader, guarded):
    '''Later extraction can clear an earlier barrier in the final schedule.'''
    body = 'flag=associated(p)\nx=size(a)'
    if guarded:
        body = f'if (allocated(a)) then\n{body}\nendif'
    tree = fortran_reader.psyir_from_source(f'''
        subroutine test(a,p)
        real, allocatable :: a(:)
        real, pointer :: p(:)
        logical :: flag
        integer :: x
        {body}
        end''')
    HoistRunetimeInquiryIntrinsicsTrans().apply(tree.walk(Routine)[0])
    inquiry = next(call for call in tree.walk(IntrinsicCall)
                   if call.intrinsic == IntrinsicCall.Intrinsic.SIZE)
    assignment = inquiry.parent
    schedule = (tree.walk(IfBlock)[0].if_body if guarded
                else tree.walk(Routine)[0])
    assert assignment.parent is schedule
    # ASSOCIATED remains a conservative call barrier, but the assignment to
    # flag is now just a reference and must no longer delay the SIZE temporary.
    assert assignment.position == (0 if guarded else 1)
    assert schedule.children[assignment.position + 1].lhs.symbol.name == 'flag'


@pytest.mark.parametrize('case', [
    ('allocate(a(10))', '', 'a'),
    ('a=[1.,2.]', '', 'a'),
    ('p=>b', 'real, pointer :: p(:)\nreal, target :: b(10)', 'p')])
@pytest.mark.parametrize('nested', [False, True])
def test_earliest_after_barrier(fortran_reader, fortran_writer, case, nested):
    '''Place inquiries immediately after the last descriptor change.'''
    change, declarations, array = case
    body = f'{change}\nj=2\nx=ubound({array},1)'
    if nested:
        body = f'do i=1,10\n{body}\nenddo'
    tree, _ = transform(fortran_reader, fortran_writer, body, declarations)
    inquiry = next(call for call in tree.walk(IntrinsicCall)
                   if call.intrinsic == IntrinsicCall.Intrinsic.UBOUND)
    schedule = (tree.walk(Loop)[0].loop_body if nested
                else tree.walk(Routine)[0])
    assert inquiry.parent is schedule.children[1]
    assert schedule.children[2].lhs.symbol.name == 'j'


def test_earliest_root_position(fortran_reader, fortran_writer):
    '''Cross unrelated statements in every schedule, including the routine.'''
    tree, _ = transform(fortran_reader, fortran_writer, '''
        j=1
        if (j>0) then
          d=1
          do i=1,10
            x=0
            x=size(a)
          enddo
        endif''')
    assert tree.walk(IntrinsicCall)[0].parent is \
        tree.walk(Routine)[0].children[0]
