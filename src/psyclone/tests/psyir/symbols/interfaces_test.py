# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author S. Siso, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab
#             A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psyclone.psyir.symbols.interfaces. '''

import pytest

from psyclone.psyir.symbols.interfaces import SymbolInterface, \
    AutomaticInterface, ArgumentInterface, ImportInterface, \
    UnresolvedInterface, StaticInterface, DefaultModuleInterface, \
    CommonBlockInterface, UnknownInterface
from psyclone.psyir.symbols import ContainerSymbol


def test_symbolinterface():
    '''Test we can create a SymbolInterface instance and make a copy of it.

    '''
    inter1 = SymbolInterface()
    inter2 = inter1.copy()
    assert isinstance(inter2, SymbolInterface)
    assert inter2 is not inter1


def test_automatic_interface():
    '''Test we can create a AutomaticInterface instance and check its __str__
    value

    '''
    interface = AutomaticInterface()
    assert str(interface) == "Automatic"


def test_defaultmoduleinterface():
    '''Test we can create an DefaultModuleInterface instance and check its
    __str__ value

    '''
    interface = DefaultModuleInterface()
    assert str(interface) == "DefaultModule"


def test_static_interface():
    '''Test we can create a StaticInterface instance and check its __str__
    value

    '''
    interface = StaticInterface()
    assert str(interface) == "Static"


def test_commonblockinterface():
    '''Test we can create an CommonBlockInterface instance and check its
    __str__ value

    '''
    interface = CommonBlockInterface()
    assert str(interface) == "CommonBlock"


def test_unresolvedinterface():
    '''Test we can create an UnresolvedInterface instance and check its
    __str__ value

    '''
    interface = UnresolvedInterface()
    assert str(interface) == "Unresolved"


def test_unknowninterface():
    '''Test we can create an UnknownInterface instance and check its
    __str__ value

    '''
    interface = UnknownInterface()
    assert str(interface) == "Unknown"


def test_importinterface():
    '''Test that we can create an Import Interface successfully, that it
    raises the expected exceptions if the container_symbol or
    orig_name attributes are of the wrong type, that the container
    symbol property and str method work as expected.

    '''
    container_symbol = ContainerSymbol("my_mod")
    import_interface = ImportInterface(container_symbol)
    assert import_interface.container_symbol is container_symbol
    assert str(import_interface) == "Import(container='my_mod')"

    import_interface = ImportInterface(container_symbol, orig_name="orig_name")
    assert import_interface.container_symbol is container_symbol
    assert str(import_interface) == ("Import(container='my_mod', "
                                     "orig_name='orig_name')")

    with pytest.raises(TypeError) as info:
        _ = ImportInterface("hello")
    assert ("ImportInterface container_symbol parameter must be of type "
            "ContainerSymbol, but found 'str'." in str(info.value))
    with pytest.raises(TypeError) as info:
        _ = ImportInterface(container_symbol, orig_name=[])
    assert ("ImportInterface orig_name parameter must be of type str or None, "
            "but found 'list'." in str(info.value))


def test_importinterface_container_symbol_getter_setter():
    '''Test that the container_symbol getter and setter properties
    retrieve and update the expected attribute and perform error checking.

    '''
    container_symbol = ContainerSymbol("my_mod")
    import_interface = ImportInterface(container_symbol)
    assert import_interface.container_symbol is container_symbol

    # Check invalid setter
    with pytest.raises(TypeError) as info:
        import_interface.container_symbol = 3
    assert ("ImportInterface container_symbol parameter must be of type "
            "ContainerSymbol, but found 'int'." in str(info.value))

    # Check valid setter and getter
    container_symbol2 = ContainerSymbol("another_mod")
    import_interface.container_symbol = container_symbol2
    assert import_interface.container_symbol is container_symbol2

    assert import_interface.orig_name is None
    import_interface = ImportInterface(
        container_symbol, orig_name="orig_name")
    assert import_interface.orig_name == "orig_name"


def test_importinterface_copy():
    ''' Test the copy() method of ImportInterface. '''
    csym = ContainerSymbol("my_mod")
    import_interface = ImportInterface(csym)
    new_interface = import_interface.copy()
    assert new_interface is not import_interface
    assert new_interface.container_symbol is csym
    assert new_interface.orig_name is None
    new_interface.container_symbol = ContainerSymbol("other_mod")
    assert import_interface.container_symbol is csym

    import_interface = ImportInterface(csym, orig_name="orig_name")
    new_interface = import_interface.copy()
    assert new_interface.orig_name == "orig_name"


def test_argumentinterface_init():
    '''Check that the ArgumentInterface can be created successfully and
    has the expected values. Also checks the access property and that
    an exception is raised if the supplied access value is the wrong
    type.

    '''
    argument_interface = ArgumentInterface()
    assert argument_interface._access == ArgumentInterface.Access.UNKNOWN
    assert argument_interface.access == argument_interface._access

    argument_interface = ArgumentInterface(ArgumentInterface.Access.READ)
    assert argument_interface._access == ArgumentInterface.Access.READ
    assert argument_interface.access == argument_interface._access

    with pytest.raises(TypeError) as info:
        _ = ArgumentInterface("hello")
    assert ("SymbolInterface.access must be an 'ArgumentInterface.Access' but "
            "got 'str'." in str(info.value))
    with pytest.raises(TypeError) as info:
        argument_interface.access = "hello"
    assert ("SymbolInterface.access must be an 'ArgumentInterface.Access' but "
            "got 'str'." in str(info.value))


@pytest.mark.parametrize("access", [ArgumentInterface.Access.READ,
                                    ArgumentInterface.Access.WRITE,
                                    ArgumentInterface.Access.READWRITE,
                                    ArgumentInterface.Access.UNKNOWN])
def test_argumentinterface_access_values(access):
    '''Check that all the ArgumentInterface access values are supported.

    '''
    argument_interface = ArgumentInterface()
    argument_interface.access = access
    assert argument_interface.access == access


def test_argumentinterface_str():
    '''Test that an ArgumentInterface instance can be stringified'''

    argument_interface = ArgumentInterface()
    assert str(argument_interface) == "Argument(Access.UNKNOWN)"

    argument_interface = ArgumentInterface(ArgumentInterface.Access.WRITE)
    assert str(argument_interface) == "Argument(Access.WRITE)"


def test_argumentinterface_copy():
    ''' Test the copy() method of ArgumentInterface. '''
    arg_interface = ArgumentInterface(access=ArgumentInterface.Access.WRITE)
    new_interface = arg_interface.copy()
    assert new_interface.access == ArgumentInterface.Access.WRITE
    # Check that we can modify the copy without affecting the original
    new_interface.access = ArgumentInterface.Access.READ
    assert arg_interface.access == ArgumentInterface.Access.WRITE
