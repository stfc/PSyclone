# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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
# Author J. Henrichs, Bureau of Meteorology
# Modified by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

''' Module containing tests for generating PSyData hooks'''

import re
import pytest

from psyclone.errors import InternalError, GenerationError
from psyclone.f2pygen import ModuleGen
from psyclone.psyir.nodes import (
    PSyDataNode, Schedule, Return, Routine, CodeBlock)
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.transformations import PSyDataTrans, TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, ImportInterface, SymbolTable, DataTypeSymbol,
    UnresolvedType, DataSymbol, UnsupportedFortranType)
from psyclone.tests.utilities import get_invoke


# -----------------------------------------------------------------------------
def test_psy_data_node_constructor():
    ''' Check that we can construct a PSyDataNode and that any options are
    picked up correctly. '''
    psy_node = PSyDataNode()
    assert psy_node._prefix == ""
    assert psy_node._var_name == ""
    assert psy_node._module_name is None
    assert psy_node._region_name is None
    assert psy_node.options == {}
    psy_node = PSyDataNode(options={"prefix": "profile"})
    assert psy_node.options == {"prefix": "profile"}
    assert psy_node._prefix == "profile_"
    assert psy_node.fortran_module == "profile_psy_data_mod"
    assert psy_node.type_name == "profile_PSyDataType"
    assert psy_node._var_name == ""
    assert psy_node._module_name is None
    assert psy_node._region_name is None
    psy_node = PSyDataNode(options={"region_name": ("a_routine", "reg1")})
    assert psy_node._var_name == ""
    assert psy_node._module_name == "a_routine"
    assert psy_node._region_name == "reg1"
    assert psy_node.region_identifier == ("a_routine", "reg1")

    # Test incorrect rename type
    with pytest.raises(InternalError) as error:
        PSyDataNode(options={"region_name": 1})
    assert ("The name must be a tuple containing two non-empty strings." in
            str(error.value))

    # Invalid prefix
    with pytest.raises(InternalError) as err:
        PSyDataNode(options={"prefix": "not-a-valid-prefix"})
    assert ("Invalid 'prefix' parameter: found 'not-a-valid-prefix', "
            "expected" in str(err.value))


def test_psy_data_node_equality():
    ''' Check the __eq__ member of the PSyDataNode.'''
    options1 = {"prefix": "profile", "region_name": ("a_routine", "ref1")}
    options2 = {"prefix": "extract", "region_name": ("a_routine", "ref1")}
    options3 = {"prefix": "profile", "region_name": ("a_routine1", "ref1")}
    options4 = {"prefix": "profile", "region_name": ("a_routine", "ref2")}
    psy_node1 = PSyDataNode(options=options1)
    psy_node1_1 = PSyDataNode(options=options1)
    psy_node2 = PSyDataNode(options=options2)
    psy_node3 = PSyDataNode(options=options3)
    psy_node4 = PSyDataNode(options=options4)
    assert psy_node1 == psy_node1_1
    assert psy_node1 != psy_node2
    assert psy_node1 != psy_node3
    assert psy_node1 != psy_node4


# -----------------------------------------------------------------------------
def test_psy_data_node_basics():
    '''Tests some elementary functions.'''
    psy_node = PSyDataNode.create([], SymbolTable())
    assert "PSyDataStart[var=psy_data]\n"\
        "PSyDataEnd[var=psy_data]" in str(psy_node)

    psy_node.children = []
    with pytest.raises(InternalError) as error:
        _ = psy_node.psy_data_body
    assert "PSyData node malformed or incomplete" in str(error.value)


# -----------------------------------------------------------------------------
def test_psy_data_node_create_errors():
    ''' Test the various checks on the arguments to the create() method. '''
    sym_tab = SymbolTable()
    with pytest.raises(TypeError) as err:
        PSyDataNode.create("hello", sym_tab)
    assert ("create(). The 'children' argument must be a list (of PSyIR "
            "nodes) but got 'str'" in str(err.value))
    with pytest.raises(TypeError) as err:
        PSyDataNode.create(["hello"], sym_tab)
    assert ("create(). The 'children' argument must be a list of PSyIR "
            "nodes but it contains: ['str']" in str(err.value))
    with pytest.raises(TypeError) as err:
        PSyDataNode.create([], "hello")
    assert ("create(). The 'symbol_table' argument must be an instance of "
            "psyir.symbols.SymbolTable but got 'str'" in str(err.value))


# -----------------------------------------------------------------------------
def test_psy_data_node_tree_correct():
    '''Test that adding children and parents will result in the correct
    relationship with the inserted node.
    '''

    # 1. No parent and no children:
    # =============================
    psy_node = PSyDataNode.create([], SymbolTable())

    # We must have a single profile node with a schedule which has
    # no children:
    assert psy_node.parent is None
    assert len(psy_node.children) == 1   # This is the Schedule
    assert isinstance(psy_node.psy_data_body, Schedule)
    assert psy_node.psy_data_body.parent == psy_node
    assert not psy_node.psy_data_body.children

    # 2. Parent, but no children:
    # ===========================
    parent = Schedule()
    psy_node = PSyDataNode.create([], parent.symbol_table)
    parent.addchild(psy_node)

    # We must have a single node connected to the parent, and an
    # empty schedule for the ExtractNode:
    assert psy_node.parent == parent
    assert parent.children[0] == psy_node
    assert len(psy_node.children) == 1
    assert isinstance(psy_node.psy_data_body, Schedule)
    assert psy_node.psy_data_body.parent is psy_node
    assert not psy_node.psy_data_body.children

    # 3. No parent, but children:
    # ===========================
    children = [Statement(), Statement()]
    psy_node = PSyDataNode.create(children, SymbolTable())

    # The children must be connected to the schedule, which is
    # connected to the ExtractNode:
    assert psy_node.parent is None
    assert len(psy_node.children) == 1
    assert isinstance(psy_node.psy_data_body, Schedule)
    assert psy_node.psy_data_body.parent is psy_node
    assert len(psy_node.psy_data_body.children) == 2
    assert psy_node.psy_data_body.children[0] is children[0]
    assert psy_node.psy_data_body.children[1] is children[1]
    assert children[0].parent == psy_node.psy_data_body
    assert children[1].parent == psy_node.psy_data_body

    # 4. Parent and children:
    # =======================
    parent = Schedule()
    # The children must be added to the parent before creating the ExtractNode
    parent.addchild(Statement())
    parent.addchild(Statement())
    # Add another child that must stay with the parent node
    third_child = Statement()
    parent.addchild(third_child)
    assert parent.children[2] is third_child
    # Only move the first two children, leave the third where it is
    children = [parent.children[0], parent.children[1]]
    for child in children:
        child.detach()
    psy_node = PSyDataNode.create(children, SymbolTable())
    parent.addchild(psy_node, 0)

    # Check all connections
    assert psy_node.parent is parent
    assert parent.children[0] is psy_node
    assert len(psy_node.children) == 1
    assert isinstance(psy_node.psy_data_body, Schedule)
    schedule = psy_node.psy_data_body
    assert schedule.parent is psy_node
    assert len(schedule.children) == 2
    for i in range(2):
        assert schedule.children[i] is children[i]
        assert children[i].parent is schedule
    # The third child of the original parent is now the
    # second child, next to the inserted ExtractNode
    assert parent.children[1] is third_child
    assert third_child.parent is parent


# -----------------------------------------------------------------------------
def test_psy_data_generate_symbols():
    ''' Check that the generate_symbols method inserts the appropriate
    symbols in the provided symbol table if they don't exist already. '''

    # By inserting the psy_data no symbols are created (only the routine symbol
    # name exists)
    routine = Routine('my_routine')
    psy_data = PSyDataNode()
    psy_data2 = PSyDataNode()
    routine.addchild(psy_data)
    routine.addchild(psy_data2)
    assert len(routine.symbol_table.symbols) == 1

    # Executing generate_symbols adds 3 more symbols:
    psy_data.generate_symbols(routine.symbol_table)
    assert len(routine.symbol_table.symbols) == 4

    # - The module (with a tag equal to its name)
    assert "psy_data_mod" in routine.symbol_table
    assert isinstance(routine.symbol_table.lookup("psy_data_mod"),
                      ContainerSymbol)
    assert routine.symbol_table.lookup_with_tag("psy_data_mod").name == \
        "psy_data_mod"

    # - The type (with a tag equal to its name)
    assert "PSyDataType" in routine.symbol_table
    typesymbol = routine.symbol_table.lookup("PSyDataType")
    assert isinstance(typesymbol, DataTypeSymbol)
    assert isinstance(typesymbol.interface, ImportInterface)
    assert isinstance(typesymbol.datatype, UnresolvedType)
    assert routine.symbol_table.lookup_with_tag("PSyDataType") == typesymbol

    # - The instantiated object
    assert "psy_data" in routine.symbol_table
    objectsymbol = routine.symbol_table.lookup("psy_data")
    assert isinstance(objectsymbol, DataSymbol)
    assert isinstance(objectsymbol.datatype, UnsupportedFortranType)

    # Executing it again doesn't add anything new
    psy_data.generate_symbols(routine.symbol_table)
    assert len(routine.symbol_table.symbols) == 4

    # But executing it again from a different psy_data re-utilises the module
    # and type but creates a new object instance
    psy_data2.generate_symbols(routine.symbol_table)
    assert len(routine.symbol_table.symbols) == 5
    assert "psy_data_1" in routine.symbol_table
    objectsymbol = routine.symbol_table.lookup("psy_data_1")
    assert isinstance(objectsymbol, DataSymbol)
    assert isinstance(objectsymbol.datatype, UnsupportedFortranType)


# -----------------------------------------------------------------------------
def test_psy_data_node_incorrect_container():
    ''' Check that the PSyDataNode constructor raises the expected error if
    the symbol table already contains an entry for the PSyDataType that is
    not associated with the PSyData container. '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    csym = schedule.symbol_table.new_symbol("some_mod",
                                            symbol_type=ContainerSymbol)
    schedule.symbol_table.new_symbol("PSyDataType",
                                     interface=ImportInterface(csym))
    data_trans = PSyDataTrans()
    with pytest.raises(TransformationError) as err:
        data_trans.apply(schedule[0].loop_body)
    assert ("already a symbol named 'PSyDataType' which clashes with one of "
            "those used by the PSyclone PSyData API" in str(err.value))


# -----------------------------------------------------------------------------
def test_psy_data_node_invokes_gocean1p0():
    '''Check that an invoke is instrumented correctly
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    data_trans = PSyDataTrans()

    data_trans.apply(schedule[0])

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")
    # First a simple test that the nesting is correct - the
    # PSyData regions include both loops. Note that indeed
    # the function 'compute_cv_code' is in the module file
    # kernel_ne_offset_mod.
    # Since this is only PSyData, which by default does not supply
    # variable information, the parameters to PreStart are both 0.
    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), target, save :: psy_data.*"
                  r"call psy_data%PreStart\(\"psy_single_invoke_different"
                  r"_iterates_over\", \"invoke_0:compute_cv_code:r0\","
                  r" 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call psy_data%PostEnd")

    assert re.search(correct_re, code, re.I) is not None

    # Check that if gen() is called more than once the same PSyDataNode
    # variables and region names are created:
    code_again = str(invoke.gen()).replace("\n", "")
    assert code == code_again


# -----------------------------------------------------------------------------
def test_psy_data_node_options():
    '''Check that the options for PSyData work as expected.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    data_trans = PSyDataTrans()

    data_trans.apply(schedule[0].loop_body)
    data_node = schedule[0].loop_body[0]
    assert isinstance(data_node, PSyDataNode)

    # 1) Test that the listed variables will appear in the list
    # ---------------------------------------------------------
    mod = ModuleGen(None, "test")
    data_node.gen_code(mod, options={"pre_var_list": [("", "a")],
                                     "post_var_list": [("", "b")]})

    out = "\n".join([str(i.root) for i in mod.children])
    expected = ['CALL psy_data%PreDeclareVariable("a", a)',
                'CALL psy_data%PreDeclareVariable("b", b)',
                'CALL psy_data%ProvideVariable("a", a)',
                'CALL psy_data%PostStart',
                'CALL psy_data%ProvideVariable("b", b)']
    for line in expected:
        assert line in out

    # 2) Test that variables suffixes are added as expected
    # -----------------------------------------------------
    mod = ModuleGen(None, "test")
    data_node.gen_code(mod, options={"pre_var_list": [("", "a")],
                                     "post_var_list": [("", "b")],
                                     "pre_var_postfix": "_pre",
                                     "post_var_postfix": "_post"})

    out = "\n".join([str(i.root) for i in mod.children])
    expected = ['CALL psy_data%PreDeclareVariable("a_pre", a)',
                'CALL psy_data%PreDeclareVariable("b_post", b)',
                'CALL psy_data%ProvideVariable("a_pre", a)',
                'CALL psy_data%PostStart',
                'CALL psy_data%ProvideVariable("b_post", b)']
    for line in expected:
        assert line in out

    # 3) Check that we don't get any declaration if there are no variables:
    # ---------------------------------------------------------------------
    mod = ModuleGen(None, "test")
    data_node.gen_code(mod, options={})

    out = "\n".join([str(i.root) for i in mod.children])
    # Only PreStart and PostEnd should appear
    assert "PreStart" in out
    assert "PreDeclareVariable" not in out
    assert "ProvideVariable" not in out
    assert "PreEnd" not in out
    assert "PostStart" not in out
    assert "PostEnd" in out


def test_psy_data_node_children_validation():
    '''Test that children added to PSyDataNode are validated. PSyDataNode
    accepts just one Schedule as its child.

    '''
    psy_node = PSyDataNode.create([], SymbolTable())
    del psy_node.children[0]

    # Invalid children (e.g. Return Statement)
    ret = Return()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.addchild(ret)
    assert ("Item 'Return' can't be child 0 of 'PSyData'. The valid format"
            " is: 'Schedule'." in str(excinfo.value))

    # Valid children
    psy_node.addchild(Schedule())

    # Additional children
    with pytest.raises(GenerationError) as excinfo:
        psy_node.addchild(Schedule())
    assert ("Item 'Schedule' can't be child 1 of 'PSyData'. The valid format"
            " is: 'Schedule'." in str(excinfo.value))


def test_psy_data_node_lower_to_language_level():
    ''' Test that the generic PSyDataNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = PSyDataNode.create([], SymbolTable())
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("A PSyDataNode must be inside a Routine context when lowering but"
            " 'PSyDataStart[var=psy_data]\nPSyDataEnd[var=psy_data]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()
    # The PSyDataNode is substituted by 2 CodeBlocks, the first one with the
    # psy-data-start annotation
    assert not routine.walk(PSyDataNode)
    codeblocks = routine.walk(CodeBlock)
    assert len(codeblocks) == 2
    assert str(codeblocks[0].ast) == \
        'CALL psy_data % PreStart("my_routine", "r0", 0, 0)'
    assert "psy-data-start" in codeblocks[0].annotations
    assert str(codeblocks[1].ast) == \
        'CALL psy_data % PostEnd'

    # Now try with a PSyDataNode with specified module and region names
    routine = Routine("my_routine")
    psy_node = PSyDataNode.create([], SymbolTable())
    routine.addchild(psy_node)
    psy_node._module_name = "my_module"
    psy_node._region_name = "my_region"
    psy_node.lower_to_language_level()
    assert not routine.walk(PSyDataNode)
    codeblocks = routine.walk(CodeBlock)
    assert len(codeblocks) == 2
    assert str(codeblocks[0].ast) == \
        'CALL psy_data % PreStart("my_module", "my_region", 0, 0)'
    assert str(codeblocks[1].ast) == \
        'CALL psy_data % PostEnd'


def test_psy_data_node_lower_to_language_level_with_options():
    '''Check that the  generic PSyDataNode is lowered as expected when it
    is provided with an options dictionary. '''

    # 1) Test that the listed variables will appear in the list
    # ---------------------------------------------------------
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    data_trans = PSyDataTrans()

    data_trans.apply(schedule[0].loop_body)
    data_node = schedule[0].loop_body[0]

    data_node.lower_to_language_level(options={"pre_var_list": [("", "a")],
                                               "post_var_list": [("", "b")]})

    codeblocks = schedule.walk(CodeBlock)
    expected = ['CALL psy_data % PreStart("invoke_0", "r0", 1, 1)',
                'CALL psy_data % PreDeclareVariable("a", a)',
                'CALL psy_data % PreDeclareVariable("b", b)',
                'CALL psy_data % PreEndDeclaration',
                'CALL psy_data % ProvideVariable("a", a)',
                'CALL psy_data % PreEnd',
                'CALL psy_data % PostStart',
                'CALL psy_data % ProvideVariable("b", b)']

    for codeblock, string in zip(codeblocks, expected):
        assert string == str(codeblock.ast)

    # 2) Test that variables suffixes are added as expected
    # -----------------------------------------------------
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    data_trans = PSyDataTrans()

    data_trans.apply(schedule[0].loop_body)
    data_node = schedule[0].loop_body[0]

    data_node.lower_to_language_level(options={"pre_var_list": [("", "a")],
                                               "post_var_list": [("", "b")],
                                               "pre_var_postfix": "_pre",
                                               "post_var_postfix": "_post"})

    codeblocks = schedule.walk(CodeBlock)
    expected = ['CALL psy_data % PreStart("invoke_0", "r0", 1, 1)',
                'CALL psy_data % PreDeclareVariable("a_pre", a)',
                'CALL psy_data % PreDeclareVariable("b_post", b)',
                'CALL psy_data % PreEndDeclaration',
                'CALL psy_data % ProvideVariable("a_pre", a)',
                'CALL psy_data % PreEnd',
                'CALL psy_data % PostStart',
                'CALL psy_data % ProvideVariable("b_post", b)']

    for codeblock, string in zip(codeblocks, expected):
        assert string == str(codeblock.ast)
