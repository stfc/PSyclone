# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
# Author A. B. G. Chalk, STFC Daresbury Lab
# Modified S. Siso, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs pytest tests on the ExtractNode PSyIR node. '''

import pytest

from psyclone.domain.gocean.transformations import GOceanExtractTrans
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.errors import InternalError
from psyclone.psyir.nodes import ExtractNode, Node, Schedule
from psyclone.psyir.symbols import SymbolTable
from psyclone.tests.utilities import get_invoke


def test_extract_node_constructor():
    '''Tests the constructor of the ExtractNode. '''
    # The constructor must have a read_write_info key
    en = ExtractNode()
    assert en.post_name == "_post"
    assert en._read_write_info is None

    en = ExtractNode(options={'read_write_info': 1})
    assert en._post_name == "_post"
    assert en._read_write_info == 1

    ex_node = ExtractNode(options={"post_var_postfix": "_my_own_post"})
    assert ex_node._post_name == "_my_own_post"

    # Add a schedule and test that we get the same object as extract body:
    schedule = Schedule()
    en.children.append(schedule)
    assert en.extract_body is schedule


def test_extract_node_gen_code():
    '''Test the ExtractNode's gen_code function if there is no ReadWriteInfo
    object specified in the options. Since the transformations will always
    do that, we need to manually insert the ExtractNode into a schedule:

    '''
    _, invoke = get_invoke("1.0.1_single_named_invoke.f90",
                           "lfric", idx=0, dist_mem=False)
    loop = invoke.schedule.children[0]
    loop.detach()
    en = ExtractNode()
    en._var_name = "psydata"
    en.addchild(Schedule(children=[loop]))
    invoke.schedule.addchild(en)

    code = str(invoke.gen())
    expected = [
        'CALL psydata%PreStart("single_invoke_psy", '
        '"invoke_important_invoke-testkern_code-r0", 17, 2)',
        'CALL psydata%PreDeclareVariable("a", a)',
        'CALL psydata%PreDeclareVariable("f1_data", f1_data)',
        'CALL psydata%PreDeclareVariable("f2_data", f2_data)',
        'CALL psydata%PreDeclareVariable("loop0_start", loop0_start)',
        'CALL psydata%PreDeclareVariable("loop0_stop", loop0_stop)',
        'CALL psydata%PreDeclareVariable("m1_data", m1_data)',
        'CALL psydata%PreDeclareVariable("m2_data", m2_data)',
        'CALL psydata%PreDeclareVariable("map_w1", map_w1)',
        'CALL psydata%PreDeclareVariable("map_w2", map_w2)',
        'CALL psydata%PreDeclareVariable("map_w3", map_w3)',
        'CALL psydata%PreDeclareVariable("ndf_w1", ndf_w1)',
        'CALL psydata%PreDeclareVariable("ndf_w2", ndf_w2)',
        'CALL psydata%PreDeclareVariable("ndf_w3", ndf_w3)',
        'CALL psydata%PreDeclareVariable("nlayers_f1", nlayers_f1)',
        'CALL psydata%PreDeclareVariable("undf_w1", undf_w1)',
        'CALL psydata%PreDeclareVariable("undf_w2", undf_w2)',
        'CALL psydata%PreDeclareVariable("undf_w3", undf_w3)',
        'CALL psydata%PreDeclareVariable("cell_post", cell)',
        'CALL psydata%PreDeclareVariable("f1_data_post", f1_data)']
    for line in expected:
        assert line in code


# ---------------------------------------------------------------------------
def test_extract_node_equality():
    ''' Test the __eq__ method of ExtractNode. '''
    # We need to manually set the same SymbolTable instance in both directives
    # for their equality to be True
    symboltable = SymbolTable()
    symboltable2 = SymbolTable()
    # Make sure they have the same ST instance, providing them as constructor
    # parameters would create a copy and not use the same instance.
    sched1 = Schedule()
    sched1._symbol_table = symboltable
    sched2 = Schedule()
    sched2._symbol_table = symboltable
    sched3 = Schedule(symbol_table=symboltable2)
    node1 = ExtractNode(children=[sched1])
    node2 = ExtractNode(children=[sched2])
    node3 = ExtractNode(children=[sched3])

    assert node1 == node2
    assert node1 != node3

    node1._post_name = "testa"
    node2._post_name = "testb"
    assert node1 != node2


# ---------------------------------------------------------------------------
def test_malformed_extract_node(monkeypatch):
    ''' Check that we raise the expected error if an ExtractNode does not have
    a single Schedule node as its child. '''
    enode = ExtractNode()
    monkeypatch.setattr(enode, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = enode.extract_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(enode, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = enode.extract_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# ---------------------------------------------------------------------------
def test_extract_node_lower_to_language_level():
    '''Tests the code created by lower_to_language_level.
    '''

    etrans = GOceanExtractTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    psy, invoke = get_invoke("single_invoke_three_kernels.f90",
                             "gocean", idx=0, dist_mem=False)
    etrans.apply(invoke.schedule.children[0])

    code = str(psy.gen)
    output = (
        """CALL extract_psy_data % """
        """PreStart("psy_single_invoke_three_kernels", "invoke_0-compute_cu_"""
        """code-r0", 9, 3)
      CALL extract_psy_data % PreDeclareVariable("cu_fld%internal%xstart", """
        """cu_fld % internal % xstart)
      CALL extract_psy_data % PreDeclareVariable("cu_fld%internal%xstop", """
        """cu_fld % internal % xstop)
      CALL extract_psy_data % PreDeclareVariable("cu_fld%internal%ystart", """
        """cu_fld % internal % ystart)
      CALL extract_psy_data % PreDeclareVariable("cu_fld%internal%ystop", """
        """cu_fld % internal % ystop)
      CALL extract_psy_data % PreDeclareVariable("p_fld", p_fld)
      CALL extract_psy_data % PreDeclareVariable("u_fld", u_fld)
      CALL extract_psy_data % PreDeclareVariable("cu_fld", cu_fld)
      CALL extract_psy_data % PreDeclareVariable("i", i)
      CALL extract_psy_data % PreDeclareVariable("j", j)
      CALL extract_psy_data % PreDeclareVariable("cu_fld_post", cu_fld)
      CALL extract_psy_data % PreDeclareVariable("i_post", i)
      CALL extract_psy_data % PreDeclareVariable("j_post", j)
      CALL extract_psy_data % PreEndDeclaration
      CALL extract_psy_data % ProvideVariable("cu_fld%internal%xstart", """
        """cu_fld % internal % xstart)
      CALL extract_psy_data % ProvideVariable("cu_fld%internal%xstop", """
        """cu_fld % internal % xstop)
      CALL extract_psy_data % ProvideVariable("cu_fld%internal%ystart", """
        """cu_fld % internal % ystart)
      CALL extract_psy_data % ProvideVariable("cu_fld%internal%ystop", """
        """cu_fld % internal % ystop)
      CALL extract_psy_data % ProvideVariable("p_fld", p_fld)
      CALL extract_psy_data % ProvideVariable("u_fld", u_fld)
      CALL extract_psy_data % ProvideVariable("cu_fld", cu_fld)
      CALL extract_psy_data % ProvideVariable("i", i)
      CALL extract_psy_data % ProvideVariable("j", j)
      CALL extract_psy_data % PreEnd
      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1
        DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1
          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO
      END DO
      CALL extract_psy_data % PostStart
      CALL extract_psy_data % ProvideVariable("cu_fld_post", cu_fld)
      CALL extract_psy_data % ProvideVariable("i_post", i)
      CALL extract_psy_data % ProvideVariable("j_post", j)
      CALL extract_psy_data % PostEnd
      """)
    assert output in code


# ---------------------------------------------------------------------------
def test_extract_node_gen():
    ''' Tests that 'gen' way of creating code works as expected.
    '''

    etrans = LFRicExtractTrans()

    psy, invoke = get_invoke("1_single_invoke.f90", "lfric",
                             idx=0, dist_mem=False)
    etrans.apply(invoke.schedule.children[0])
    code = str(psy.gen)
    output = '''      ! ExtractStart
      !
      CALL extract_psy_data%PreStart("single_invoke_psy", \
"invoke_0_testkern_type-testkern_code-r0", 18, 2)
      CALL extract_psy_data%PreDeclareVariable("a", a)
      CALL extract_psy_data%PreDeclareVariable("f1_data", f1_data)
      CALL extract_psy_data%PreDeclareVariable("f2_data", f2_data)
      CALL extract_psy_data%PreDeclareVariable("loop0_start", loop0_start)
      CALL extract_psy_data%PreDeclareVariable("loop0_stop", loop0_stop)
      CALL extract_psy_data%PreDeclareVariable("m1_data", m1_data)
      CALL extract_psy_data%PreDeclareVariable("m2_data", m2_data)
      CALL extract_psy_data%PreDeclareVariable("map_w1", map_w1)
      CALL extract_psy_data%PreDeclareVariable("map_w2", map_w2)
      CALL extract_psy_data%PreDeclareVariable("map_w3", map_w3)
      CALL extract_psy_data%PreDeclareVariable("ndf_w1", ndf_w1)
      CALL extract_psy_data%PreDeclareVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%PreDeclareVariable("ndf_w3", ndf_w3)
      CALL extract_psy_data%PreDeclareVariable("nlayers_f1", nlayers_f1)
      CALL extract_psy_data%PreDeclareVariable("undf_w1", undf_w1)
      CALL extract_psy_data%PreDeclareVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreDeclareVariable("undf_w3", undf_w3)
      CALL extract_psy_data%PreDeclareVariable("cell", cell)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("f1_data_post", f1_data)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("a", a)
      CALL extract_psy_data%ProvideVariable("f1_data", f1_data)
      CALL extract_psy_data%ProvideVariable("f2_data", f2_data)
      CALL extract_psy_data%ProvideVariable("loop0_start", loop0_start)
      CALL extract_psy_data%ProvideVariable("loop0_stop", loop0_stop)
      CALL extract_psy_data%ProvideVariable("m1_data", m1_data)
      CALL extract_psy_data%ProvideVariable("m2_data", m2_data)
      CALL extract_psy_data%ProvideVariable("map_w1", map_w1)
      CALL extract_psy_data%ProvideVariable("map_w2", map_w2)
      CALL extract_psy_data%ProvideVariable("map_w3", map_w3)
      CALL extract_psy_data%ProvideVariable("ndf_w1", ndf_w1)
      CALL extract_psy_data%ProvideVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%ProvideVariable("ndf_w3", ndf_w3)
      CALL extract_psy_data%ProvideVariable("nlayers_f1", nlayers_f1)
      CALL extract_psy_data%ProvideVariable("undf_w1", undf_w1)
      CALL extract_psy_data%ProvideVariable("undf_w2", undf_w2)
      CALL extract_psy_data%ProvideVariable("undf_w3", undf_w3)
      CALL extract_psy_data%ProvideVariable("cell", cell)
      CALL extract_psy_data%PreEnd
      DO cell = loop0_start, loop0_stop, 1
        CALL testkern_code(nlayers_f1, a, f1_data, f2_data, ''' + \
        "m1_data, m2_data, ndf_w1, undf_w1, " + \
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, " + \
        '''undf_w3, map_w3(:,cell))
      END DO
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("cell_post", cell)
      CALL extract_psy_data%ProvideVariable("f1_data_post", f1_data)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd'''
    assert output in code
