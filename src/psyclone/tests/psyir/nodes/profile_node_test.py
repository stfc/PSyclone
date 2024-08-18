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
# Author:   A. R. Porter, STFC Daresbury Laboratory
# Modified: S. Siso, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the ProfileNode. '''

import pytest
from fparser.two import Fortran2003
from psyclone.psyir.nodes import (ProfileNode, Literal, Assignment, CodeBlock,
                                  Reference, Return, KernelSchedule, Loop)
from psyclone.psyir.symbols import SymbolTable, DataSymbol, REAL_TYPE, \
    ContainerSymbol, DataTypeSymbol, UnsupportedFortranType, ImportInterface
from psyclone.profiler import Profiler
from psyclone.errors import InternalError
from psyclone.tests.utilities import get_invoke


def teardown_function():
    '''This function is called at the end of any test function. It disables
    any automatic profiling set. This is necessary in case of a test failure
    to make sure any further tests will not be run with profiling enabled.
    '''
    Profiler._options = []


def test_profile_node_constructor():
    ''' Basic checks for the ProfileNode constructor. '''
    pnode = ProfileNode()
    assert pnode._prefix == "profile_"
    assert pnode._psy_data_symbol_with_prefix == "profile_psy_data"


def test_profile_node_create():
    ''' Basic checks for the create() method of ProfileNode. '''
    sched = KernelSchedule.create("test", SymbolTable(), [])
    pnode = ProfileNode.create([], SymbolTable())
    sched.addchild(pnode)
    assert str(pnode) == ("ProfileStart[var=profile_psy_data]\n"
                          "Schedule:\n"
                          "End Schedule\n"
                          "ProfileEnd")
    pnode2 = ProfileNode.create([], symbol_table=sched.symbol_table,
                                options={"region_name": ("my_mod", "first")})
    assert pnode2._module_name == "my_mod"
    assert pnode2._region_name == "first"
    # Check that the symbol table contains the appropriate symbols:
    # A Container for the profile_psy_data_mod module
    table = sched.symbol_table
    csym = table.lookup("profile_psy_data_mod")
    assert isinstance(csym, ContainerSymbol)
    # A type symbol for the derived type used to capture profiling data
    type_sym = table.lookup("profile_PSyDataType")
    assert isinstance(type_sym, DataTypeSymbol)
    assert isinstance(type_sym.interface, ImportInterface)
    assert type_sym.interface.container_symbol is csym
    # A symbol of derived type to contain the profiling data. As it must
    # have the (unsupported) 'save' and 'target' attributes, it has to be of
    # UnsupportedFortranType.
    dsym = table.lookup("profile_psy_data")
    assert isinstance(dsym, DataSymbol)
    assert isinstance(dsym.datatype, UnsupportedFortranType)
    assert (dsym.datatype.declaration ==
            "type(profile_PSyDataType), save, target :: profile_psy_data")


@pytest.mark.parametrize("value", [["a", "b"], ("a"), ("a", "b", "c"),
                                   ("a", []), ([], "a")])
def test_profile_node_invalid_name(value):
    '''Test that the expected exception is raised when an invalid profile
    name is provided to a ProfileNode.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = ProfileNode.create([], SymbolTable(),
                               options={"region_name": value})
    assert ("Error in PSyDataNode. The name must be a tuple containing "
            "two non-empty strings." in str(excinfo.value))


@pytest.mark.usefixtures("parser")
def test_lower_to_lang_level_single_node():
    ''' Test the lower_to_language_level() method when a Schedule contains
    a single ProfileNode.

    '''
    Profiler.set_options([Profiler.INVOKES], api="nemo")
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        symbol_type=DataSymbol, datatype=REAL_TYPE)
    zero = Literal("0.0", REAL_TYPE)
    one = Literal("1.0", REAL_TYPE)
    assign1 = Assignment.create(Reference(arg1), zero)
    assign2 = Assignment.create(Reference(arg1), one)

    kschedule = KernelSchedule.create(
        "work1", symbol_table, [assign1, assign2, Return()])
    Profiler.add_profile_nodes(kschedule, Loop)
    assert isinstance(kschedule.children[0], ProfileNode)
    assert isinstance(kschedule.children[-1], Return)
    kschedule.lower_to_language_level()
    # The ProfileNode should have been replaced by two CodeBlocks with its
    # children inserted between them.
    assert isinstance(kschedule[0], CodeBlock)
    # The first CodeBlock should have the "psy-data-start" annotation.
    assert kschedule[0].annotations == ["psy-data-start"]
    ptree = kschedule[0].get_ast_nodes
    assert len(ptree) == 1
    assert isinstance(ptree[0], Fortran2003.Call_Stmt)
    assert kschedule[1] is assign1
    assert kschedule[2] is assign2
    assert isinstance(kschedule[-2], CodeBlock)
    assert kschedule[-2].annotations == []
    ptree = kschedule[-2].get_ast_nodes
    assert len(ptree) == 1
    assert isinstance(ptree[0], Fortran2003.Call_Stmt)
    assert isinstance(kschedule[-1], Return)


def test_lower_named_profile_node():
    ''' Test that the lower_to_language_level method behaves as expected when
    a ProfileNode has pre-set names for the module and region.

    '''
    Profiler.set_options([Profiler.INVOKES], api="nemo")
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        symbol_type=DataSymbol, datatype=REAL_TYPE)
    assign1 = Assignment.create(Reference(arg1), Literal("0.0", REAL_TYPE))
    kschedule = KernelSchedule.create(
        "work1", symbol_table, [assign1, Return()])
    Profiler.add_profile_nodes(kschedule, Loop)
    pnode = kschedule.walk(ProfileNode)[0]
    # Manually set the module and region names (to save using a transformation)
    pnode._module_name = 'my_mod'
    pnode._region_name = 'first'
    kschedule.lower_to_language_level()
    cblocks = kschedule.walk(CodeBlock)
    assert ("PreStart(\"my_mod\", \"first\", 0, 0)" in
            str(cblocks[0].get_ast_nodes[0]))


def test_lower_to_lang_level_multi_node():
    ''' Test the lower_to_language_level() method when a Schedule contains
    multiple ProfileNodes.

    '''
    # We use a GOcean example containing multiple kernel calls
    Profiler.set_options([Profiler.KERNELS], api="gocean1.0")
    _, invoke = get_invoke("single_invoke_two_kernels.f90", "gocean1.0",
                           idx=0)
    sched = invoke.schedule
    table = sched.symbol_table
    Profiler.add_profile_nodes(sched, Loop)
    sched.lower_to_language_level()
    sym0 = table.lookup("profile_psy_data")
    assert isinstance(sym0, DataSymbol)
    sym1 = table.lookup("profile_psy_data_1")
    assert isinstance(sym1, DataSymbol)
    cblocks = sched.walk(CodeBlock)
    ptree = cblocks[0].get_ast_nodes
    code = str(ptree[0]).lower()
    assert "call profile_psy_data % prestart(\"invoke_0\", \"r0\"" in code
    assert cblocks[0].annotations == ["psy-data-start"]
    assert cblocks[1].annotations == []
    ptree = cblocks[2].get_ast_nodes
    code = str(ptree[0]).lower()
    assert "call profile_psy_data_1 % prestart(\"invoke_0\", \"r1\"" in code
    assert cblocks[2].annotations == ["psy-data-start"]
    assert cblocks[3].annotations == []
