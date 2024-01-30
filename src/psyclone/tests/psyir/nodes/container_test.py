# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Container PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Container, Return, KernelSchedule, \
    FileContainer
from psyclone.psyir.symbols import SymbolTable, DataSymbol, REAL_SINGLE_TYPE
from psyclone.errors import GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links
from psyclone.psyir.nodes.node import colored


def test_container_init():
    '''Test that a container is initialised as expected.'''
    container = Container("test")
    assert container._name == "test"
    assert container._parent is None
    assert isinstance(container._symbol_table, SymbolTable)


def test_container_equality():
    '''Test the __eq__ method of the container class.'''
    # Subclasses of ScopingNode need to have the same SymbolTable
    symboltable = SymbolTable()
    container1 = Container("test")
    container2 = Container("test")
    container3 = Container("not_test")
    container1._symbol_table = symboltable
    container2._symbol_table = symboltable
    container3._symbol_table = symboltable
    assert container1 == container2
    assert container1 != container3


def test_container_init_parent():
    '''Test that a container parent argument is stored as expected.'''
    container = Container("test", parent=FileContainer("hello"))
    assert container.parent.name == "hello"


def test_container_name():
    '''Test that the container name can be set and changed as
    expected.'''
    container = Container("test")
    assert container.name == "test"
    container.name = "new_test"
    assert container.name == "new_test"


def test_container_node_str():
    '''Check the node_str method of the Container class.'''
    cont_stmt = Container("bin")
    coloredtext = colored("Container", Container._colour)
    assert coloredtext+"[bin]" in cont_stmt.node_str()


def test_container_can_be_printed():
    '''Test that a Container instance can always be printed (i.e. is
    initialised fully)'''
    cont_stmt = Container("box")
    assert "Container[box]\n" in str(cont_stmt)


def test_container_create():
    '''Test that the create method in the Container class correctly
    creates a Container instance.

    '''
    symbol_table = SymbolTable()
    symbol_table.add(DataSymbol("tmp", REAL_SINGLE_TYPE))
    kernel1 = KernelSchedule.create("mod_1", SymbolTable(), [])
    kernel2 = KernelSchedule.create("mod_2", SymbolTable(), [])
    container = Container.create("container_name", symbol_table,
                                 [kernel1, kernel2])
    check_links(container, [kernel1, kernel2])
    assert container.symbol_table is symbol_table
    result = FortranWriter().container_node(container)
    assert result == (
        "module container_name\n"
        "  implicit none\n"
        "  real, public :: tmp\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine mod_1()\n\n\n"
        "  end subroutine mod_1\n"
        "  subroutine mod_2()\n\n\n"
        "  end subroutine mod_2\n\n"
        "end module container_name\n")


def test_container_create_invalid():
    '''Test that the create method in a Container class raises the
    expected exception if the provided input is invalid.

    '''
    symbol_table = SymbolTable()
    symbol_table.add(DataSymbol("x", REAL_SINGLE_TYPE))
    children = [KernelSchedule.create("mod_1", SymbolTable(), [])]

    # name is not a string.
    with pytest.raises(GenerationError) as excinfo:
        _ = Container.create(1, symbol_table, children)
    assert ("name argument in create method of Container class "
            "should be a string but found 'int'.") in str(excinfo.value)

    # symbol_table not a SymbolTable.
    with pytest.raises(GenerationError) as excinfo:
        _ = Container.create("container", "invalid", children)
    assert ("symbol_table argument in create method of Container class "
            "should be a SymbolTable but found 'str'.") in str(excinfo.value)

    # children not a list.
    with pytest.raises(GenerationError) as excinfo:
        _ = Container.create("mod_name", symbol_table, "invalid")
    assert ("children argument in create method of Container class should "
            "be a list but found 'str'." in str(excinfo.value))

    # contents of children list are not Container or KernelSchedule.
    with pytest.raises(GenerationError) as excinfo:
        _ = Container.create("mod_name", symbol_table, ["invalid"])
    assert ("Item 'str' can't be child 0 of 'Container'. The valid format is:"
            " '[Container | Routine | CodeBlock]*'."
            in str(excinfo.value))


def test_container_children_validation():
    '''Test that children added to Container are validated. Container
    accepts just Container and kernelSchedule as children.

    '''
    container = Container.create("container", SymbolTable(), [])

    # Valid children
    container2 = Container.create("container2", SymbolTable(), [])
    container.addchild(container2)

    # Invalid children (e.g. Return Statement)
    ret = Return()
    with pytest.raises(GenerationError) as excinfo:
        container.addchild(ret)
    assert ("Item 'Return' can't be child 1 of 'Container'. The valid format"
            " is: '[Container | Routine | CodeBlock]*'."
            "" in str(excinfo.value))
