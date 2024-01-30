# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# Modified A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the PSyIR Directive node. '''

import os
import pytest
from collections import OrderedDict

from psyclone import f2pygen
from psyclone.core import Signature
from psyclone.errors import GenerationError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir import nodes
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.transformations import ACCDataTrans, DynamoOMPParallelLoopTrans

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")


def test_directive_backward_dependence():
    '''Test that the backward_dependence method works for Directives,
    returning the closest dependent Node before the current Node in
    the schedule or None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    for child in schedule.children:
        otrans.apply(child)
    # 1: omp directive no backwards dependence
    omp3 = schedule.children[2]
    assert not omp3.backward_dependence()
    # 2: omp to omp backward dependence
    # a) many steps
    last_omp_node = schedule.children[6]
    prev_dep_omp_node = schedule.children[3]
    assert last_omp_node.backward_dependence() == prev_dep_omp_node
    # b) previous
    assert prev_dep_omp_node.backward_dependence() == omp3
    # 3: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    otrans.apply(schedule.children[0])
    otrans.apply(schedule.children[1])
    otrans.apply(schedule.children[3])
    omp1 = schedule.children[0]
    omp2 = schedule.children[1]
    global_sum = schedule.children[2]
    omp3 = schedule.children[3]
    # a) omp3 depends on global sum
    assert omp3.backward_dependence() == global_sum
    # b) global sum depends on omp2
    assert global_sum.backward_dependence() == omp2
    # c) omp2 (sum) depends on omp1
    assert omp2.backward_dependence() == omp1


def test_create_data_movement_deep_copy_refs(fortran_reader):
    '''Tests for the create_data_movement_deep_copy_refs() method. This method
    is responsible for creating the required list of References for deep-
    copying any given type of Reference over to a remote address space (in e.g.
    OpenACC or OpenMP).

    '''
    psyir = fortran_reader.psyir_from_source(
        '''
program my_prog
  use some_mod
  implicit None
  integer :: ji = 1
  real :: a_scalar = 0.5
  real :: a(10)
  type(my_type) :: b, d(5)
  a(:) = 10.0
  b%grid(ji)%data(:) = a(:) + a_scalar
  d(ji)%grid(2)%data(:) = 3.0
  a_scalar = 1.0
  a(:) = a(:) + 1.0
  call some_sub(d)
end program my_prog
''')
    sched = psyir.walk(nodes.Routine)[0]
    # Use the ACCDataTrans transformation to insert the directive to test.
    data_trans = ACCDataTrans()
    # Flat array.
    data_trans.apply(sched[0])
    reads, writes, readwrites = sched[0].create_data_movement_deep_copy_refs()
    assert all(isinstance(obj, OrderedDict) for obj in
               [reads, writes, readwrites])
    sig = Signature('a')
    assert isinstance(writes[sig], nodes.Reference)
    assert writes[sig].symbol.name == "a"
    # Structure access.
    data_trans.apply(sched[1])
    reads, writes, readwrites = sched[1].create_data_movement_deep_copy_refs()
    assert isinstance(reads[Signature("a")], nodes.Reference)
    assert Signature("a_scalar") not in reads
    assert isinstance(writes[Signature("b")], nodes.Reference)
    assert isinstance(writes[Signature(("b", "grid"))],
                      nodes.StructureReference)
    assert isinstance(writes[Signature(("b", "grid", "data"))],
                      nodes.StructureReference)
    # Array of structures access.
    data_trans.apply(sched[2])
    reads, writes, readwrites = sched[2].create_data_movement_deep_copy_refs()
    assert isinstance(writes[Signature("d")], nodes.Reference)
    assert isinstance(writes[Signature(("d", "grid"))],
                      nodes.StructureReference)
    assert isinstance(writes[Signature(("d", "grid", "data"))],
                      nodes.StructureReference)
    # Scalars are excluded.
    data_trans.apply(sched[3])
    reads, writes, readwrites = sched[3].create_data_movement_deep_copy_refs()
    assert not reads and not writes and not readwrites
    data_trans.apply(sched[4])
    # Statement that reads and writes a variable.
    reads, writes, readwrites = sched[4].create_data_movement_deep_copy_refs()
    assert not reads and not writes
    assert isinstance(readwrites[Signature("a")], nodes.Reference)
    # Subroutine call - the arg. is conservatively assumed to be read-write.
    data_trans.apply(sched[5])
    reads, writes, readwrites = sched[5].create_data_movement_deep_copy_refs()
    assert not writes
    assert not reads
    assert Signature("d") in readwrites


def test_regiondirective_children_validation():
    '''Test that children added to RegionDirective are validated.
        RegionDirective accepts 1 Schedule as child.

    '''
    directive = nodes.RegionDirective()
    datanode = nodes.Literal("1", INTEGER_TYPE)
    schedule = nodes.Schedule()

    # First child
    with pytest.raises(GenerationError) as excinfo:
        directive.children[0] = datanode
    assert ("Item 'Literal' can't be child 0 of 'RegionDirective'. The valid "
            "format is: 'Schedule'." in str(excinfo.value))

    # Additional children
    with pytest.raises(GenerationError) as excinfo:
        directive.addchild(schedule)
    assert ("Item 'Schedule' can't be child 1 of 'RegionDirective'. The valid "
            "format is: 'Schedule'." in str(excinfo.value))


@pytest.mark.usefixtures("dist_mem")
def test_regiondirective_gen_post_region_code():
    '''Test that the RegionDirective.gen_post_region_code() method does
    nothing for language-level PSyIR.

    TODO #1648 - this can be removed when the gen_post_region_code() method is
    removed.'''
    temporary_module = f2pygen.ModuleGen("test")
    subroutine = nodes.Routine("testsub")
    directive = nodes.RegionDirective()
    sym = subroutine.symbol_table.new_symbol(
            "i", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    loop = nodes.Loop.create(sym,
                             nodes.Literal("1", INTEGER_TYPE),
                             nodes.Literal("10", INTEGER_TYPE),
                             nodes.Literal("1", INTEGER_TYPE), [])
    directive.dir_body.addchild(loop)
    subroutine.addchild(directive)
    directive.gen_post_region_code(temporary_module)
    # No nodes should have been added to the tree.
    assert len(temporary_module.children) == 1
    assert isinstance(temporary_module.children[0], f2pygen.ImplicitNoneGen)


def test_standalonedirective_children_validation():
    '''Test that children cannot be added to StandaloneDirective.'''
    cdir = nodes.StandaloneDirective()
    schedule = nodes.Schedule()

    # test adding child
    with pytest.raises(GenerationError) as excinfo:
        cdir.addchild(schedule)
    assert ("Item 'Schedule' can't be child 0 of 'StandaloneDirective'. The "
            "valid format is: 'None'." in str(excinfo.value))
