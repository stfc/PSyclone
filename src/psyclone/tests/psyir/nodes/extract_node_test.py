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

from psyclone.core import Signature
from psyclone.domain.gocean.transformations import GOceanExtractTrans
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.errors import InternalError
from psyclone.psyir.nodes import ExtractNode, Node, Schedule
from psyclone.psyir.symbols import SymbolTable
from psyclone.psyir.tools import ReadWriteInfo
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


def test_extract_node_lowering(fortran_writer):
    '''Test the ExtractNode's lowering function if there is no ReadWriteInfo
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

    code = fortran_writer(invoke.schedule)
    expected = [
        'CALL psydata % PreStart("single_invoke_psy", '
        '"invoke_important_invoke-testkern_code-r0", 17, 16)',
        'CALL psydata % PreDeclareVariable("a", a)',
        'CALL psydata % PreDeclareVariable("f1_data", f1_data)',
        'CALL psydata % PreDeclareVariable("f2_data", f2_data)',
        'CALL psydata % PreDeclareVariable("loop0_start", loop0_start)',
        'CALL psydata % PreDeclareVariable("loop0_stop", loop0_stop)',
        'CALL psydata % PreDeclareVariable("m1_data", m1_data)',
        'CALL psydata % PreDeclareVariable("m2_data", m2_data)',
        'CALL psydata % PreDeclareVariable("map_w1", map_w1)',
        'CALL psydata % PreDeclareVariable("map_w2", map_w2)',
        'CALL psydata % PreDeclareVariable("map_w3", map_w3)',
        'CALL psydata % PreDeclareVariable("ndf_w1", ndf_w1)',
        'CALL psydata % PreDeclareVariable("ndf_w2", ndf_w2)',
        'CALL psydata % PreDeclareVariable("ndf_w3", ndf_w3)',
        'CALL psydata % PreDeclareVariable("nlayers_f1", nlayers_f1)',
        'CALL psydata % PreDeclareVariable("undf_w1", undf_w1)',
        'CALL psydata % PreDeclareVariable("undf_w2", undf_w2)',
        'CALL psydata % PreDeclareVariable("undf_w3", undf_w3)',
        'CALL psydata % PreDeclareVariable("cell_post", cell)',
        'CALL psydata % PreDeclareVariable("f1_data_post", f1_data)']
    for line in expected:
        assert line in code, line + "\n---\n" + code


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
      """code-r0", 7, 5)
    CALL extract_psy_data % PreDeclareVariable("cu_fld_data", cu_fld_data)
    CALL extract_psy_data % PreDeclareVariable("cu_fld_internal_xstart", """
      """cu_fld_internal_xstart)
    CALL extract_psy_data % PreDeclareVariable("cu_fld_internal_xstop", """
      """cu_fld_internal_xstop)
    CALL extract_psy_data % PreDeclareVariable("cu_fld_internal_ystart", """
      """cu_fld_internal_ystart)
    CALL extract_psy_data % PreDeclareVariable("cu_fld_internal_ystop", """
      """cu_fld_internal_ystop)
    CALL extract_psy_data % PreDeclareVariable("p_fld_data", p_fld_data)
    CALL extract_psy_data % PreDeclareVariable("u_fld_data", u_fld_data)
    CALL extract_psy_data % PreDeclareVariable("cu_fld_data_post", cu_fld_data)
    CALL extract_psy_data % PreDeclareVariable("i_post", i)
    CALL extract_psy_data % PreDeclareVariable("j_post", j)
    CALL extract_psy_data % PreDeclareVariable("p_fld_data_post", p_fld_data)
    CALL extract_psy_data % PreDeclareVariable("u_fld_data_post", u_fld_data)
    CALL extract_psy_data % PreEndDeclaration
    CALL extract_psy_data % ProvideVariable("cu_fld_data", cu_fld_data)
    CALL extract_psy_data % ProvideVariable("cu_fld_internal_xstart", """
      """cu_fld_internal_xstart)
    CALL extract_psy_data % ProvideVariable("cu_fld_internal_xstop", """
      """cu_fld_internal_xstop)
    CALL extract_psy_data % ProvideVariable("cu_fld_internal_ystart", """
      """cu_fld_internal_ystart)
    CALL extract_psy_data % ProvideVariable("cu_fld_internal_ystop", """
      """cu_fld_internal_ystop)
    CALL extract_psy_data % ProvideVariable("p_fld_data", p_fld_data)
    CALL extract_psy_data % ProvideVariable("u_fld_data", u_fld_data)
    CALL extract_psy_data % PreEnd
    do j = cu_fld_internal_ystart, cu_fld_internal_ystop, 1
      do i = cu_fld_internal_xstart, cu_fld_internal_xstop, 1
        call compute_cu_code(i, j, cu_fld_data, p_fld_data, u_fld_data)
      enddo
    enddo
    CALL extract_psy_data % PostStart
    CALL extract_psy_data % ProvideVariable("cu_fld_data_post", cu_fld_data)
    CALL extract_psy_data % ProvideVariable("i_post", i)
    CALL extract_psy_data % ProvideVariable("j_post", j)
    CALL extract_psy_data % ProvideVariable("p_fld_data_post", p_fld_data)
    CALL extract_psy_data % ProvideVariable("u_fld_data_post", u_fld_data)
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
    output = '''CALL extract_psy_data % PreStart("single_invoke_psy", \
"invoke_0_testkern_type-testkern_code-r0", 17, 16)
    CALL extract_psy_data % PreDeclareVariable("a", a)
    CALL extract_psy_data % PreDeclareVariable("f1_data", f1_data)
    CALL extract_psy_data % PreDeclareVariable("f2_data", f2_data)
    CALL extract_psy_data % PreDeclareVariable("loop0_start", loop0_start)
    CALL extract_psy_data % PreDeclareVariable("loop0_stop", loop0_stop)
    CALL extract_psy_data % PreDeclareVariable("m1_data", m1_data)
    CALL extract_psy_data % PreDeclareVariable("m2_data", m2_data)
    CALL extract_psy_data % PreDeclareVariable("map_w1", map_w1)
    CALL extract_psy_data % PreDeclareVariable("map_w2", map_w2)
    CALL extract_psy_data % PreDeclareVariable("map_w3", map_w3)
    CALL extract_psy_data % PreDeclareVariable("ndf_w1", ndf_w1)
    CALL extract_psy_data % PreDeclareVariable("ndf_w2", ndf_w2)
    CALL extract_psy_data % PreDeclareVariable("ndf_w3", ndf_w3)
    CALL extract_psy_data % PreDeclareVariable("nlayers_f1", nlayers_f1)
    CALL extract_psy_data % PreDeclareVariable("undf_w1", undf_w1)
    CALL extract_psy_data % PreDeclareVariable("undf_w2", undf_w2)
    CALL extract_psy_data % PreDeclareVariable("undf_w3", undf_w3)
    CALL extract_psy_data % PreDeclareVariable("a_post", a)
    CALL extract_psy_data % PreDeclareVariable("cell_post", cell)
    CALL extract_psy_data % PreDeclareVariable("f1_data_post", f1_data)
    CALL extract_psy_data % PreDeclareVariable("f2_data_post", f2_data)
    CALL extract_psy_data % PreDeclareVariable("m1_data_post", m1_data)
    CALL extract_psy_data % PreDeclareVariable("m2_data_post", m2_data)
    CALL extract_psy_data % PreDeclareVariable("map_w1_post", map_w1)
    CALL extract_psy_data % PreDeclareVariable("map_w2_post", map_w2)
    CALL extract_psy_data % PreDeclareVariable("map_w3_post", map_w3)
    CALL extract_psy_data % PreDeclareVariable("ndf_w1_post", ndf_w1)
    CALL extract_psy_data % PreDeclareVariable("ndf_w2_post", ndf_w2)
    CALL extract_psy_data % PreDeclareVariable("ndf_w3_post", ndf_w3)
    CALL extract_psy_data % PreDeclareVariable("nlayers_f1_post", nlayers_f1)
    CALL extract_psy_data % PreDeclareVariable("undf_w1_post", undf_w1)
    CALL extract_psy_data % PreDeclareVariable("undf_w2_post", undf_w2)
    CALL extract_psy_data % PreDeclareVariable("undf_w3_post", undf_w3)
    CALL extract_psy_data % PreEndDeclaration
    CALL extract_psy_data % ProvideVariable("a", a)
    CALL extract_psy_data % ProvideVariable("f1_data", f1_data)
    CALL extract_psy_data % ProvideVariable("f2_data", f2_data)
    CALL extract_psy_data % ProvideVariable("loop0_start", loop0_start)
    CALL extract_psy_data % ProvideVariable("loop0_stop", loop0_stop)
    CALL extract_psy_data % ProvideVariable("m1_data", m1_data)
    CALL extract_psy_data % ProvideVariable("m2_data", m2_data)
    CALL extract_psy_data % ProvideVariable("map_w1", map_w1)
    CALL extract_psy_data % ProvideVariable("map_w2", map_w2)
    CALL extract_psy_data % ProvideVariable("map_w3", map_w3)
    CALL extract_psy_data % ProvideVariable("ndf_w1", ndf_w1)
    CALL extract_psy_data % ProvideVariable("ndf_w2", ndf_w2)
    CALL extract_psy_data % ProvideVariable("ndf_w3", ndf_w3)
    CALL extract_psy_data % ProvideVariable("nlayers_f1", nlayers_f1)
    CALL extract_psy_data % ProvideVariable("undf_w1", undf_w1)
    CALL extract_psy_data % ProvideVariable("undf_w2", undf_w2)
    CALL extract_psy_data % ProvideVariable("undf_w3", undf_w3)
    CALL extract_psy_data % PreEnd
    do cell = loop0_start, loop0_stop, 1
      call testkern_code(nlayers_f1, a, f1_data, f2_data, m1_data, m2_data, \
ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, \
undf_w3, map_w3(:,cell))
    enddo
    CALL extract_psy_data % PostStart
    CALL extract_psy_data % ProvideVariable("a_post", a)
    CALL extract_psy_data % ProvideVariable("cell_post", cell)
    CALL extract_psy_data % ProvideVariable("f1_data_post", f1_data)
    CALL extract_psy_data % ProvideVariable("f2_data_post", f2_data)
    CALL extract_psy_data % ProvideVariable("m1_data_post", m1_data)
    CALL extract_psy_data % ProvideVariable("m2_data_post", m2_data)
    CALL extract_psy_data % ProvideVariable("map_w1_post", map_w1)
    CALL extract_psy_data % ProvideVariable("map_w2_post", map_w2)
    CALL extract_psy_data % ProvideVariable("map_w3_post", map_w3)
    CALL extract_psy_data % ProvideVariable("ndf_w1_post", ndf_w1)
    CALL extract_psy_data % ProvideVariable("ndf_w2_post", ndf_w2)
    CALL extract_psy_data % ProvideVariable("ndf_w3_post", ndf_w3)
    CALL extract_psy_data % ProvideVariable("nlayers_f1_post", nlayers_f1)
    CALL extract_psy_data % ProvideVariable("undf_w1_post", undf_w1)
    CALL extract_psy_data % ProvideVariable("undf_w2_post", undf_w2)
    CALL extract_psy_data % ProvideVariable("undf_w3_post", undf_w3)
    CALL extract_psy_data % PostEnd'''
    assert output in code


def test_flatten_signature():
    '''Tests that a user-defined type access is correctly converted
    to a 'flattened' string.'''

    new_name = ExtractNode._flatten_signature(Signature("a%b%c"))
    assert new_name == "a_b_c"


def test_determine_postfix():
    '''Test that a unique postfix is determined.
    '''

    # Test if there is no clash that the specified postfix is returned as is:
    read_write_info = ReadWriteInfo()
    postfix = ExtractNode.determine_postfix(read_write_info)
    assert postfix == "_post"
    postfix = ExtractNode.determine_postfix(read_write_info,
                                            postfix="_new_postfix")
    assert postfix == "_new_postfix"

    # Clash between input variable and a created output variable:
    read_write_info = ReadWriteInfo()
    read_write_info.add_read(Signature("var_post"))
    read_write_info.add_write(Signature("var"))
    postfix = ExtractNode.determine_postfix(read_write_info)
    assert postfix == "_post0"

    # Two clashes between input variable and a created output variable:
    read_write_info.add_read(Signature("var_post0"))
    postfix = ExtractNode.determine_postfix(read_write_info)
    assert postfix == "_post1"

    # Two clashes between different input variables and created output
    # variables: 'var1' prevents the '_post' to be used, 'var2'
    # prevents "_post0" to be used, 'var3' prevents "_post1":
    read_write_info = ReadWriteInfo()
    read_write_info.add_read(Signature("var1_post"))
    read_write_info.add_read(Signature("var2_post0"))
    read_write_info.add_read(Signature("var3_post1"))
    read_write_info.add_write(Signature("var1"))
    read_write_info.add_write(Signature("var2"))
    read_write_info.add_write(Signature("var3"))
    postfix = ExtractNode.determine_postfix(read_write_info)
    assert postfix == "_post2"

    # Handle clash between output variables: the first variable will
    # create "var" and var_post", the second "var_post" and "var_post_post".
    read_write_info = ReadWriteInfo()
    read_write_info. add_write(Signature("var"))
    read_write_info. add_write(Signature("var_post"))
    postfix = ExtractNode.determine_postfix(read_write_info)
    assert postfix == "_post0"
    read_write_info.add_write(Signature("var_post0"))
    postfix = ExtractNode.determine_postfix(read_write_info)
    assert postfix == "_post1"


@pytest.mark.usefixtures("change_into_tmpdir")
def test_psylayer_flatten_same_symbols():
    '''Make sure that when we flatten a symbol, we bring its data back
    after using the flattened version, including repeatedly if there are
    multiple extraction regions.

    '''
    psy, invoke = get_invoke("driver_test.f90", "gocean",
                             idx=3, dist_mem=False)

    etrans = GOceanExtractTrans()
    etrans.apply(invoke.schedule[0])
    etrans.apply(invoke.schedule[1])
    etrans.apply(invoke.schedule[2])
    code = psy.gen

    # Flatten before region 1
    assert """
    out_fld_internal_ystart = out_fld%internal%ystart
    out_fld_internal_ystop = out_fld%internal%ystop
    out_fld_internal_xstart = out_fld%internal%xstart
    out_fld_internal_xstop = out_fld%internal%xstop
    out_fld_data = out_fld%data
    in_out_fld_data = in_out_fld%data
    in_fld_data = in_fld%data
    dx_data = dx%data
    in_fld_grid_dx = in_fld%grid%dx
    in_fld_grid_gphiu = in_fld%grid%gphiu
    CALL extract_psy_data % PreStart("psy_extract_example_with_various_\
variable_access_patterns", "invoke_3-compute_kernel_code-r0", 10, 8)
    """ in code

    # Brint the data back to the orginal structure at the end of the region1
    # and then flatten it again to prepare for the second region
    assert """
    CALL extract_psy_data % PostEnd
    in_fld%grid%gphiu = in_fld_grid_gphiu
    in_fld%grid%dx = in_fld_grid_dx
    dx%data = dx_data
    in_fld%data = in_fld_data
    in_out_fld%data = in_out_fld_data
    out_fld%data = out_fld_data
    out_fld%internal%xstop = out_fld_internal_xstop
    out_fld%internal%xstart = out_fld_internal_xstart
    out_fld%internal%ystop = out_fld_internal_ystop
    out_fld%internal%ystart = out_fld_internal_ystart
    out_fld_internal_ystart_1 = out_fld%internal%ystart
    out_fld_internal_ystop_1 = out_fld%internal%ystop
    out_fld_internal_xstart_1 = out_fld%internal%xstart
    out_fld_internal_xstop_1 = out_fld%internal%xstop
    out_fld_data_1 = out_fld%data
    in_out_fld_data_1 = in_out_fld%data
    in_fld_data_1 = in_fld%data
    dx_data_1 = dx%data
    in_fld_grid_dx_1 = in_fld%grid%dx
    in_fld_grid_gphiu_1 = in_fld%grid%gphiu
    CALL extract_psy_data_1 % PreStart("psy_extract_example_with_\
various_variable_access_patterns", "invoke_3-compute_kernel_code-r1", 10, 8)
    """ in code

    # Bring data back and flatten agian a thrid time
    assert """
    CALL extract_psy_data_1 % PostEnd
    in_fld%grid%gphiu = in_fld_grid_gphiu_1
    in_fld%grid%dx = in_fld_grid_dx_1
    dx%data = dx_data_1
    in_fld%data = in_fld_data_1
    in_out_fld%data = in_out_fld_data_1
    out_fld%data = out_fld_data_1
    out_fld%internal%xstop = out_fld_internal_xstop_1
    out_fld%internal%xstart = out_fld_internal_xstart_1
    out_fld%internal%ystop = out_fld_internal_ystop_1
    out_fld%internal%ystart = out_fld_internal_ystart_1
    out_fld_internal_ystart_2 = out_fld%internal%ystart
    out_fld_internal_ystop_2 = out_fld%internal%ystop
    out_fld_internal_xstart_2 = out_fld%internal%xstart
    out_fld_internal_xstop_2 = out_fld%internal%xstop
    out_fld_data_2 = out_fld%data
    in_out_fld_data_2 = in_out_fld%data
    in_fld_data_2 = in_fld%data
    dx_data_2 = dx%data
    in_fld_grid_dx_2 = in_fld%grid%dx
    in_fld_grid_gphiu_2 = in_fld%grid%gphiu
    CALL extract_psy_data_2 % PreStart("psy_extract_example_with_various_\
variable_access_patterns", "invoke_3-compute_kernel_code-r2", 10, 8)
    """ in code
