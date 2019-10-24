# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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

''' Perform py.test tests on the psygen.psyir.symbols.datasymbols file '''

import pytest
from psyclone.psyir.symbols import DataSymbol, ContainerSymbol
from psyclone.psyGen import InternalError


# Test DataSymbol Class
def test_symbol_initialisation():
    '''Test that a DataSymbol instance can be created when valid arguments are
    given, otherwise raise relevant exceptions.'''

    # Test with valid arguments
    assert isinstance(DataSymbol('a', 'real'), DataSymbol)
    # real constants are not currently supported
    assert isinstance(DataSymbol('a', 'integer'), DataSymbol)
    assert isinstance(DataSymbol('a', 'integer', constant_value=0), DataSymbol)
    assert isinstance(DataSymbol('a', 'character'), DataSymbol)
    assert isinstance(DataSymbol('a', 'character', constant_value="hello"),
                      DataSymbol)
    assert isinstance(DataSymbol('a', 'boolean'), DataSymbol)
    assert isinstance(DataSymbol('a', 'boolean', constant_value=False),
                      DataSymbol)
    assert isinstance(DataSymbol('a', 'real', [None]), DataSymbol)
    assert isinstance(DataSymbol('a', 'real', [3]), DataSymbol)
    assert isinstance(DataSymbol('a', 'real', [3, None]), DataSymbol)
    assert isinstance(DataSymbol('a', 'real', []), DataSymbol)
    assert isinstance(DataSymbol('a', 'real', [],
                                 interface=DataSymbol.Argument()),
                      DataSymbol)
    assert isinstance(
        DataSymbol('a', 'real', [],
                   interface=DataSymbol.Argument(
                       access=DataSymbol.Access.READWRITE)),
        DataSymbol)
    assert isinstance(
        DataSymbol('a', 'real', [],
                   interface=DataSymbol.Argument(
                       access=DataSymbol.Access.READ)),
        DataSymbol)
    my_mod = ContainerSymbol("my_mod")
    assert isinstance(
        DataSymbol('a', 'deferred', interface=DataSymbol.Global(my_mod)),
        DataSymbol)
    dim = DataSymbol('dim', 'integer', [])
    assert isinstance(DataSymbol('a', 'real', [dim]), DataSymbol)
    assert isinstance(DataSymbol('a', 'real', [3, dim, None]), DataSymbol)

    # Test with invalid arguments
    with pytest.raises(NotImplementedError) as error:
        DataSymbol('a', 'invalidtype', [], 'local')
    assert (
        "DataSymbol can only be initialised with {0} datatypes but found "
        "'invalidtype'.".format(str(DataSymbol.valid_data_types))) in str(
            error.value)

    with pytest.raises(TypeError) as error:
        DataSymbol('a', 'real', shape=dim)
    assert "DataSymbol shape attribute must be a list." in str(error.value)

    with pytest.raises(TypeError) as error:
        DataSymbol('a', 'real', ['invalidshape'])
    assert ("DataSymbol shape list elements can only be 'DataSymbol', "
            "'integer' or 'None'.") in str(error.value)

    with pytest.raises(TypeError) as error:
        bad_dim = DataSymbol('dim', 'real', [])
        DataSymbol('a', 'real', [bad_dim])
    assert ("Symbols that are part of another symbol shape can "
            "only be scalar integers, but found") in str(error.value)

    with pytest.raises(TypeError) as error:
        bad_dim = DataSymbol('dim', 'integer', [3])
        DataSymbol('a', 'real', [bad_dim])
    assert ("Symbols that are part of another symbol shape can "
            "only be scalar integers, but found") in str(error.value)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', 'integer', interface=DataSymbol.Argument(),
                   constant_value=9)
    assert ("Symbol with a constant value is currently limited to having "
            "a Local interface but found '") in str(error)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', 'integer', shape=[None], constant_value=9)
    assert ("Symbol with a constant value must be a scalar but the shape "
            "attribute is not empty.") in str(error)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', 'integer', constant_value=9.81)
    assert ("This DataSymbol instance's datatype is 'integer' which means the "
            "constant value is expected to be") in str(error)
    assert "'int'>' but found " in str(error)
    assert "'float'>'." in str(error)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', 'character', constant_value=42)
    assert ("This DataSymbol instance's datatype is 'character' which means "
            "the constant value is expected to be") in str(error)
    assert "'str'>' but found " in str(error)
    assert "'int'>'." in str(error)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', 'boolean', constant_value="hello")
    assert ("This DataSymbol instance's datatype is 'boolean' which means the "
            "constant value is expected to be") in str(error)
    assert "'bool'>' but found " in str(error)
    assert "'str'>'." in str(error)


def test_symbol_map():
    '''Test the mapping variable in the DataSymbol class does not raise any
    exceptions when it is used with the valid_data_types variable in
    the DataSymbol class.

    '''
    # "deferred" is not supported in the mapping so we expect
    # it to have 1 fewer entries than there are valid data types
    assert len(DataSymbol.valid_data_types) == len(DataSymbol.mapping) + 1
    for data_type in DataSymbol.valid_data_types:
        if data_type not in ["deferred"]:
            assert data_type in DataSymbol.mapping


def test_symbol_can_be_printed():
    '''Test that a DataSymbol instance can always be printed. (i.e. is
    initialised fully.)'''
    symbol = DataSymbol("sname", "real")
    assert "sname: <real, Scalar, Local>" in str(symbol)

    sym1 = DataSymbol("s1", "integer")
    assert "s1: <integer, Scalar, Local>" in str(sym1)

    sym2 = DataSymbol("s2", "real", [None, 2, sym1])
    assert "s2: <real, Array['Unknown bound', 2, s1], Local>" in str(sym2)

    my_mod = ContainerSymbol("my_mod")
    sym3 = DataSymbol("s3", "real",
                      interface=DataSymbol.Global(my_mod))
    assert ("s3: <real, Scalar, Global(container='my_mod')"
            in str(sym3))

    sym2._shape.append('invalid')
    with pytest.raises(InternalError) as error:
        _ = str(sym2)
    assert ("DataSymbol shape list elements can only be 'DataSymbol', "
            "'integer' or 'None', but found") in str(error.value)

    sym3 = DataSymbol("s3", "integer", constant_value=12)
    assert "s3: <integer, Scalar, Local, constant_value=12>" in str(sym3)


def test_symbol_constant_value_setter():
    '''Test that a DataSymbol constant value can be set if given a new valid
    constant value. Also test that is_constant returns True

    '''

    # Test with valid constant value
    sym = DataSymbol('a', 'integer', constant_value=7)
    assert sym.constant_value == 7
    sym.constant_value = 9
    assert sym.constant_value == 9

    sym = DataSymbol('a', 'real', constant_value=3.1415)
    assert sym.constant_value == 3.1415
    sym.constant_value = 1.0
    assert sym.constant_value == 1.0

    sym = DataSymbol('a', 'deferred')
    with pytest.raises(ValueError) as error:
        sym.constant_value = 1.0
    assert ("A constant value is not supported for datatype "
            "'deferred'.") in str(error)


def test_symbol_is_constant():
    '''Test that the DataSymbol is_constant property returns True if a
    constant value is set and False if it is not.

    '''
    sym = DataSymbol('a', 'integer')
    assert not sym.is_constant
    sym.constant_value = 9
    assert sym.is_constant


def test_symbol_scalar_array():
    '''Test that the DataSymbol property is_scalar returns True if the
    DataSymbol is a scalar and False if not and that the DataSymbol property
    is_array returns True if the DataSymbol is an array and False if not.

    '''
    sym1 = DataSymbol("s1", "integer")
    sym2 = DataSymbol("s2", "real", [None, 2, sym1])
    assert sym1.is_scalar
    assert not sym1.is_array
    assert not sym2.is_scalar
    assert sym2.is_array


def test_symbol_invalid_interface():
    ''' Check that the DataSymbol.interface setter rejects the supplied value
    if it is not a SymbolInterface. '''
    sym = DataSymbol("some_var", "real")
    with pytest.raises(TypeError) as err:
        sym.interface = "invalid interface spec"
    assert ("interface to a DataSymbol must be a SymbolInterface or None but"
            in str(err))


def test_symbol_interface():
    ''' Check the interface getter on a DataSymbol. '''
    my_mod = ContainerSymbol("my_mod")
    symbol = DataSymbol("some_var", "real",
                        interface=DataSymbol.Global(my_mod))
    assert symbol.interface.container_symbol.name == "my_mod"


def test_symbol_interface_access():
    ''' Tests for the SymbolInterface.access setter. '''
    my_mod = ContainerSymbol("my_mod")
    symbol = DataSymbol("some_var", "real",
                        interface=DataSymbol.Argument())
    symbol.interface.access = DataSymbol.Access.READ
    assert symbol.interface.access == DataSymbol.Access.READ
    # Force the error by supplying a string instead of a SymbolAccess type.
    with pytest.raises(TypeError) as err:
        symbol.interface.access = "read"
    assert "must be a 'DataSymbol.Access' but got " in str(err)


def test_symbol_argument_str():
    ''' Check the __str__ method of the DataSymbol.Argument class. '''
    # A DataSymbol.Argument represents a routine argument by default.
    interface = DataSymbol.Argument()
    assert str(interface) == "Argument(pass-by-value=False)"


def test_fortranglobal_str():
    ''' Test the __str__ method of DataSymbol.Global. '''
    # If it's not an argument then we have nothing else to say about it (since
    # other options are language specific and are implemented in sub-classes).
    my_mod = ContainerSymbol("my_mod")
    interface = DataSymbol.Global(my_mod)
    assert str(interface) == "Global(container='my_mod')"


def test_global_modname():
    ''' Test the Global.module_name setter error conditions. '''
    with pytest.raises(TypeError) as err:
        _ = DataSymbol.Global(None)
    assert ("Global container_symbol parameter must be of type"
            " ContainerSymbol, but found ") in str(err)


def test_symbol_copy():
    '''Test that the DataSymbol copy method produces a faithful separate copy
    of the original symbol.

    '''
    symbol = DataSymbol("myname", "real", shape=[1, 2], constant_value=None,
                        interface=DataSymbol.Argument(
                            access=DataSymbol.Access.READWRITE))
    new_symbol = symbol.copy()

    # Check the new symbol has the same properties as the original
    assert symbol.name == new_symbol.name
    assert symbol.datatype == new_symbol.datatype
    assert symbol.shape == new_symbol.shape
    assert symbol.constant_value == new_symbol.constant_value
    assert symbol.interface == new_symbol.interface

    # Change the properties of the new symbol and check the original
    # is not affected. Can't check constant_value yet as we have a
    # shape value
    new_symbol._name = "new"
    new_symbol._datatype = "integer"
    new_symbol.shape[0] = 3
    new_symbol.shape[1] = 4
    new_symbol._interface = DataSymbol.Local()

    assert symbol.name == "myname"
    assert symbol.datatype == "real"
    assert symbol.shape == [1, 2]
    assert not symbol.constant_value

    # Now check constant_value
    new_symbol._shape = []
    new_symbol.constant_value = True

    assert symbol.shape == [1, 2]
    assert not symbol.constant_value


def test_symbol_copy_properties():
    '''Test that the DataSymbol copy_properties method works as expected.'''

    symbol = DataSymbol("myname", "real", shape=[1, 2], constant_value=None,
                        interface=DataSymbol.Argument(
                            access=DataSymbol.Access.READWRITE))

    # Check an exception is raised if an incorrect argument is passed in
    with pytest.raises(TypeError) as excinfo:
        symbol.copy_properties(None)
    assert ("Argument should be of type 'DataSymbol' but found 'NoneType'."
            "") in str(excinfo.value)

    new_symbol = DataSymbol("other_name", "integer", shape=[],
                            constant_value=7)

    symbol.copy_properties(new_symbol)

    assert symbol.name == "myname"
    assert symbol.datatype == "integer"
    assert symbol.shape == []
    assert isinstance(symbol.interface, DataSymbol.Local)
    assert symbol.constant_value == 7
