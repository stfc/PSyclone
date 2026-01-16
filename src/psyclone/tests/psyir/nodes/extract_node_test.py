# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026, Science and Technology Facilities Council.
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

import logging
import pytest

from psyclone.configuration import Config
from psyclone.core import Signature
from psyclone.domain.gocean.transformations import GOceanExtractTrans
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.errors import InternalError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import ExtractNode, Loop, Node, Schedule, Routine
from psyclone.psyir.symbols import SymbolTable, ArrayType
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


def test_get_unique_region_name():
    ''' Test the get_unique_region_name utility method. '''
    etrans = GOceanExtractTrans()

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean", idx=0, dist_mem=False)
    etrans.apply(invoke.schedule.children[0])

    # In PSyKAl it returns the psy module name and no given region_name
    extract_node = invoke.schedule.walk(ExtractNode)[0]
    mod_name, region_name = extract_node.get_unique_region_name(
                                                extract_node.children)
    assert mod_name == "psy_single_invoke_three_kernels"
    assert region_name is None

    # If a region name is given, it will be returned
    extract_node._region_name = "my_region_name"
    # and it is not in an Invoke, it will return its outer scope name
    Routine.create("myscope", children=[extract_node.detach()])
    mod_name, region_name = extract_node.get_unique_region_name(
                                                extract_node.children)
    assert mod_name == "myscope"
    assert region_name == "my_region_name"


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

    psy, invoke = get_invoke("single_invoke_three_kernels.f90",
                             "gocean", idx=0, dist_mem=False)
    etrans.apply(invoke.schedule.children[0])

    code = str(psy.gen)
    output = (
      """CALL extract_psy_data % """
      """PreStart("psy_single_invoke_three_kernels", "invoke_0-compute_cu_"""
      """code-r0", 7, 3)
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
    """)
    assert output in code

    # Currently the following fields are also compared, even if DSL info tells
    # they are only read. If we take advantage of this information, these
    # and the associated _post declarations would be gone
    expected = '''
    CALL extract_psy_data % ProvideVariable("p_fld_data_post", p_fld_data)
    CALL extract_psy_data % ProvideVariable("u_fld_data_post", u_fld_data)
    CALL extract_psy_data % PostEnd
    '''
    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in code

    # Make sure loop variables are excluded:
    loop_vars = [
        'PreDeclareVariable("i", i)',
        'PreDeclareVariable("j", j)',
        'PreDeclareVariable("i_post", i)',
        'PreDeclareVariable("j_post", j)',
        'ProvideVariable("i", i)',
        'ProvideVariable("j", j)',
        'ProvideVariable("i_post", i)',
        'ProvideVariable("j_post", j)',
        ]
    for line in loop_vars:
        assert line not in code


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
"invoke_0_testkern_type-testkern_code-r0", 17, 15)
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
    '''
    assert output in code

    output = '''
    CALL extract_psy_data % ProvideVariable("f1_data_post", f1_data)
    '''
    assert output in code

    # Currently the following fields are also compared, even if DSL info tells
    # they are only read. If we take advantage of this information, these
    # and the associated _post declarations would be gone
    assert '''CALL extract_psy_data % ProvideVariable("a_post", a)''' in code
    expected = '''
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
    CALL extract_psy_data % PostEnd
    '''
    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in code


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
variable_access_patterns", "invoke_3-compute_kernel_code-r0", 10, 6)
    """ in code

    # Bring the data back to the orginal structure at the end of the region1
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
various_variable_access_patterns", "invoke_3-compute_kernel_code-r1", 10, 6)
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
variable_access_patterns", "invoke_3-compute_kernel_code-r2", 10, 6)
    """ in code


def test_extraction_flatten_datatype(monkeypatch):
    ''' Test that the flatten datatype method resolve the GOcean datatypes
    using DSL information and it raises the appropriate error if a gocean
    properties does not exist.'''
    _, invoke = get_invoke("driver_test.f90",
                           "gocean", idx=0, dist_mem=False)
    schedule = invoke.schedule
    en = ExtractNode()

    # Get the reference to the 6th argument, which is the gocean
    # property.
    ref = schedule.children[0].loop_body.children[0] \
        .loop_body.children[0].arguments.args[5].psyir_expression()
    # Make sure we have the right reference:
    assert ref.children[0].children[0].name == "gphiu"
    dtype = en._flatten_datatype(ref)
    # Gphiu is a 2D array
    assert isinstance(dtype, ArrayType)
    assert len(dtype.shape) == 2

    # Monkey patch the grid property dictionary to remove the
    # go_grid_lat_u entry, triggering an earlier error:
    api_config = Config.get().api_conf("gocean")
    grid_properties = api_config.grid_properties
    monkeypatch.delitem(grid_properties, "go_grid_lat_u")

    with pytest.raises(InternalError) as err:
        dtype = en._flatten_datatype(ref)
    assert ("Could not find type for reference 'in_fld%grid%gphiu' in the"
            " config file" in str(err.value))


# ---------------------------------------------------------------------------
def test_extract_node_multi_node():
    '''Tests extracting more than one loop works, and that loop variables
    are indeed excluded even though they appear in more than one loop
    '''

    etrans = GOceanExtractTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90",
                             "gocean", idx=0, dist_mem=False)
    etrans.apply(invoke.schedule.children[0:3])

    code = str(psy.gen)
    # We don't test the full code here, but just make sure that we
    # have the right number of variables that indicates that indeed
    # all three loops are included
    expected = (
      'CALL extract_psy_data % '
      'PreStart("psy_single_invoke_three_kernels", "invoke_0-r0", 15, 7)')
    assert expected in code

    # Make sure that the loop variables are indeed removed
    loop_vars = [
        'PreDeclareVariable("i", i)',
        'PreDeclareVariable("j", j)',
        'PreDeclareVariable("i_post", i)',
        'PreDeclareVariable("j_post", j)',
        'ProvideVariable("i", i)',
        'ProvideVariable("j", j)',
        'ProvideVariable("i_post", i)',
        'ProvideVariable("j_post", j)',
        ]

    for line in loop_vars:
        assert line not in code


# ---------------------------------------------------------------------------
def test_extract_ignore_imported_symbol(fortran_reader: FortranReader,
                                        fortran_writer: FortranWriter) -> None:
    '''
    Test that if a loop variable is imported from a module, it is
    also ignored.
    '''
    prog = """
    module my_mod
       integer :: a
    end module my_mod

    program test
       use my_mod, only: a

       do a=1, 10
       end do
    end program test
    """
    psyir = fortran_reader.psyir_from_source(prog)
    extract_trans = GOceanExtractTrans()
    extract_trans.apply(psyir.walk(Loop)[0])
    code = fortran_writer(psyir)

    assert 'PreStart("test", "test-r0", 0, 0)' in code


# ---------------------------------------------------------------------------
def test_extract_ignore_wrong_vars(fortran_reader: FortranReader,
                                   fortran_writer: FortranWriter,
                                   monkeypatch,
                                   caplog) -> None:
    '''
    Test various edge cases when variables are supposed to be ignored
    that are not in the list.
    '''
    prog = """
    program test
       integer :: i, a, b

       do i=1, 10
          a = b
       end do
    end program test
    """
    psyir = fortran_reader.psyir_from_source(prog)
    extract_trans = GOceanExtractTrans()
    extract_trans.apply(psyir.walk(Loop)[0])

    # First remove b as read variable,
    # which should not log a warning:
    # -------------------------------
    monkeypatch.setattr(ExtractNode, "get_ignored_variables",
                        lambda self: [("", Signature("b"))])
    with caplog.at_level(logging.WARNING):
        code = fortran_writer(psyir)
    assert 'PreStart("test", "test-r0", 2, 2)' in code
    assert caplog.text == ""

    # Then remove a as written variable,
    # which should not log a warning:
    # ----------------------------------
    monkeypatch.setattr(ExtractNode, "get_ignored_variables",
                        lambda self: [("", Signature("a"))])
    with caplog.at_level(logging.WARNING):
        code = fortran_writer(psyir)
    assert 'PreStart("test", "test-r0", 2, 1)' in code
    assert caplog.text == ""

    # Now try to remove non-existing variable,
    # which must log a warning:
    # ----------------------------------------
    monkeypatch.setattr(ExtractNode, "get_ignored_variables",
                        lambda self: [("", "xx")])
    with caplog.at_level(logging.WARNING):
        code = fortran_writer(psyir)

    assert 'PreStart("test", "test-r0", 3, 2)' in code
    assert ("Variable 'xx' is to be removed from ReadWriteInfo, but it's "
            "neither in the list of read variables" in caplog.text)
