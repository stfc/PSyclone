# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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

from __future__ import absolute_import
import pytest

from psyclone.psyir.symbols import SymbolError, DataSymbol, ContainerSymbol, \
    LocalInterface, GlobalInterface, ArgumentInterface, UnresolvedInterface, \
    DataType
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Container, Literal, Reference, \
    BinaryOperation, Return


def test_datasymbol_initialisation():
    '''Test that a DataSymbol instance can be created when valid arguments are
    given, otherwise raise relevant exceptions.'''

    real_single_type = ScalarType(ScalarType.Name.REAL,
                                  ScalarType.Precision.SINGLE)
    real_double_type = ScalarType(ScalarType.Name.REAL,
                                  ScalarType.Precision.DOUBLE)
    real4_type = ScalarType(ScalarType.Name.REAL,
                                  ScalarType.Precision.DOUBLE)

    # Test with valid arguments
    assert isinstance(DataSymbol('a', real_single_type), DataSymbol)
    assert isinstance(DataSymbol('a', real_double_type), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.REAL, precision=4), DataSymbol)
    kind = DataSymbol('r_def', DataType.INTEGER)
    assert isinstance(DataSymbol('a', DataType.REAL, precision=kind),
                      DataSymbol)
    # real constants are not currently supported
    assert isinstance(DataSymbol('a', DataType.INTEGER), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.INTEGER, constant_value=0),
                      DataSymbol)
    assert isinstance(DataSymbol('a', DataType.INTEGER, precision=4),
                      DataSymbol)

    assert isinstance(DataSymbol('a', DataType.CHARACTER), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.CHARACTER,
                                 constant_value="hello"), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.BOOLEAN), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.BOOLEAN, constant_value=False),
                      DataSymbol)

    assert isinstance(DataSymbol('a', DataType.REAL,
                                 [DataSymbol.Extent.ATTRIBUTE]), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.REAL, [3]), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.REAL,
                                 [3, DataSymbol.Extent.ATTRIBUTE]), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.REAL, []), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.REAL, [], precision=8),
                      DataSymbol)
    assert isinstance(DataSymbol('a', DataType.REAL, [],
                                 interface=ArgumentInterface()), DataSymbol)
    assert isinstance(
        DataSymbol('a', DataType.REAL, [],
                   interface=ArgumentInterface(
                       ArgumentInterface.Access.READWRITE)), DataSymbol)
    assert isinstance(
        DataSymbol('a', DataType.REAL, [],
                   interface=ArgumentInterface(
                       ArgumentInterface.Access.READ)), DataSymbol)
    my_mod = ContainerSymbol("my_mod")
    assert isinstance(
        DataSymbol('a', DataType.DEFERRED, interface=GlobalInterface(my_mod)),
        DataSymbol)
    dim = DataSymbol('dim', DataType.INTEGER, [])
    assert isinstance(DataSymbol('a', DataType.REAL, [dim]), DataSymbol)
    assert isinstance(DataSymbol('a', DataType.REAL,
                                 [3, dim, DataSymbol.Extent.ATTRIBUTE]),
                      DataSymbol)


def test_datasymbol_init_errors():
    ''' Test that the Symbol constructor raises appropriate errors if supplied
    with invalid arguments. '''
    # Test with invalid arguments
    with pytest.raises(TypeError) as error:
        DataSymbol('a', 'invalidtype', [], 'local')
    assert ("datatype of a DataSymbol must be specified using a DataType "
            "but got: 'str'" in str(error.value))

    with pytest.raises(TypeError) as error:
        DataSymbol('a', 3, [], 'local')
    assert ("datatype of a DataSymbol must be specified using a DataType "
            "but got:" in str(error.value))

    dim = DataSymbol('dim', DataType.INTEGER, [])
    with pytest.raises(TypeError) as error:
        DataSymbol('a', DataType.REAL, shape=dim)
    assert "DataSymbol shape attribute must be a list." in str(error.value)

    with pytest.raises(TypeError) as error:
        DataSymbol('a', DataType.REAL, ['invalidshape'])
    assert ("DataSymbol shape list elements can only be 'DataSymbol', "
            "'integer' or DataSymbol.Extent.") in str(error.value)

    with pytest.raises(TypeError) as error:
        bad_dim = DataSymbol('dim', DataType.REAL, [])
        DataSymbol('a', DataType.REAL, [bad_dim])
    assert ("Symbols that are part of another symbol shape can "
            "only be scalar integers, but found") in str(error.value)

    with pytest.raises(TypeError) as error:
        bad_dim = DataSymbol('dim', DataType.INTEGER, [3])
        DataSymbol('a', DataType.REAL, [bad_dim])
    assert ("Symbols that are part of another symbol shape can "
            "only be scalar integers, but found") in str(error.value)


def test_datasymbol_precision_errors():
    ''' Check that invalid precision settings raise the appropriate errors in
    the DataSymbol constructor. '''
    with pytest.raises(ValueError) as err:
        DataSymbol('a', DataType.INTEGER, precision=0)
    assert ("The precision of a DataSymbol when specified as an integer number"
            " of bytes must be > 0" in str(err.value))
    with pytest.raises(ValueError) as err:
        DataSymbol('a', DataType.CHARACTER, precision=1)
    assert ("A DataSymbol of DataType.CHARACTER type cannot have an associated"
            " precision" in str(err.value))
    with pytest.raises(ValueError) as err:
        DataSymbol('a', DataType.BOOLEAN, precision=1)
    assert ("A DataSymbol of DataType.BOOLEAN type cannot have an associated "
            "precision" in str(err.value))
    not_int = DataSymbol('b', DataType.REAL)
    with pytest.raises(ValueError) as err:
        DataSymbol('a', DataType.INTEGER, precision=not_int)
    assert ("A DataSymbol representing the precision of another DataSymbol "
            "must be of either 'deferred' or scalar, integer type "
            in str(err.value))
    not_scalar = DataSymbol('b', DataType.INTEGER, [2, 2])
    with pytest.raises(ValueError) as err:
        DataSymbol('a', DataType.INTEGER, precision=not_scalar)
    assert ("A DataSymbol representing the precision of another DataSymbol "
            "must be of either 'deferred' or scalar, integer type but"
            in str(err.value))
    with pytest.raises(TypeError) as err:
        DataSymbol('a', DataType.INTEGER, precision="not-valid")
    assert ("DataSymbol precision must be one of integer, DataSymbol.Precision"
            " or DataSymbol but got" in str(err.value))


def test_datasymbol_can_be_printed():
    '''Test that a DataSymbol instance can always be printed. (i.e. is
    initialised fully.)'''
    symbol = DataSymbol("sname", DataType.REAL)
    assert "sname: <DataType.REAL, Scalar, Local>" in str(symbol)

    sym1 = DataSymbol("s1", DataType.INTEGER)
    assert "s1: <DataType.INTEGER, Scalar, Local>" in str(sym1)

    sym2 = DataSymbol("s2", DataType.REAL,
                      [DataSymbol.Extent.ATTRIBUTE, 2, sym1])
    assert "s2: <DataType.REAL, Array['ATTRIBUTE', 2, s1], Local>" \
        in str(sym2)

    my_mod = ContainerSymbol("my_mod")
    sym3 = DataSymbol("s3", DataType.REAL, interface=GlobalInterface(my_mod))
    assert ("s3: <DataType.REAL, Scalar, Global(container='my_mod')"
            in str(sym3))

    sym2._shape.append('invalid')
    with pytest.raises(InternalError) as error:
        _ = str(sym2)
    assert ("DataSymbol shape list elements can only be 'DataSymbol', "
            "'integer' or 'None', but found") in str(error.value)

    sym3 = DataSymbol("s3", DataType.INTEGER, constant_value=12)
    assert "s3: <DataType.INTEGER, Scalar, Local, constant_value=Literal" \
           "[value:'12', DataType.INTEGER]>" in str(sym3)

    sym4 = DataSymbol("s4", DataType.INTEGER, interface=UnresolvedInterface())
    assert "s4: <DataType.INTEGER, Scalar, Unresolved>" in str(sym4)


def test_datasymbol_constant_value_setter():
    '''Test that a DataSymbol constant value can be set if given a new valid
    constant value.'''

    # Test with valid constant values
    sym = DataSymbol('a', DataType.INTEGER, constant_value=7)
    assert sym.constant_value.datatype == DataType.INTEGER
    assert sym.constant_value.value == "7"
    sym.constant_value = 9
    assert sym.constant_value.datatype == DataType.INTEGER
    assert sym.constant_value.value == "9"

    sym = DataSymbol('a', DataType.REAL, constant_value=3.1415)
    assert sym.constant_value.datatype == DataType.REAL
    assert sym.constant_value.value == "3.1415"
    sym.constant_value = 1.0
    assert sym.constant_value.datatype == DataType.REAL
    assert sym.constant_value.value == "1.0"

    sym = DataSymbol('a', DataType.BOOLEAN, constant_value=True)
    assert sym.constant_value.datatype == DataType.BOOLEAN
    assert sym.constant_value.value == "true"
    sym.constant_value = False
    assert sym.constant_value.datatype == DataType.BOOLEAN
    assert sym.constant_value.value == "false"

    # Test with valid constant expressions
    lhs = Literal('2', DataType.INTEGER)
    rhs = Reference(DataSymbol('constval', DataType.INTEGER))
    ct_expr = BinaryOperation.create(BinaryOperation.Operator.ADD, lhs, rhs)
    sym = DataSymbol('a', DataType.INTEGER, constant_value=ct_expr)
    assert isinstance(sym.constant_value, BinaryOperation)
    assert sym.constant_value is ct_expr


def test_datasymbol_constant_value_setter_invalid():
    '''Test that a DataSymbol constant value setter raises the appropriate
    error if an invalid value and/or datatype are given.'''

    # Test with invalid constant values
    sym = DataSymbol('a', DataType.DEFERRED)
    with pytest.raises(ValueError) as error:
        sym.constant_value = 1.0
    assert ("Error setting constant value for symbol 'a'. Constant values are"
            " not supported for 'DataType.DEFERRED' datatypes."
            in str(error.value))

    # Test with invalid constant expressions
    ct_expr = Return()
    with pytest.raises(ValueError) as error:
        sym = DataSymbol('a', DataType.INTEGER, constant_value=ct_expr)
    assert "Error setting constant value for symbol 'a'. PSyIR static " \
        "expressions can only contain PSyIR literal, operation or reference" \
        " nodes but found:" in str(error.value)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', DataType.INTEGER, interface=ArgumentInterface(),
                   constant_value=9)
    assert ("Error setting constant value for symbol 'a'. A DataSymbol with "
            "an ArgumentInterface can not have a constant value."
            in str(error.value))

    with pytest.raises(ValueError) as error:
        DataSymbol('a', DataType.INTEGER, shape=[DataSymbol.Extent.ATTRIBUTE],
                   constant_value=9)
    assert ("Error setting constant value for symbol 'a'. A DataSymbol with a"
            " constant value must be a scalar but a shape was found."
            in str(error.value))

    with pytest.raises(ValueError) as error:
        DataSymbol('a', DataType.INTEGER, constant_value=9.81)
    assert ("Error setting constant value for symbol 'a'. This DataSymbol "
            "instance datatype is 'DataType.INTEGER' which means the constant"
            " value is expected to be") in str(error.value)
    assert "'int'>' but found " in str(error.value)
    assert "'float'>'." in str(error.value)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', DataType.CHARACTER, constant_value=42)
    assert ("Error setting constant value for symbol 'a'. This DataSymbol "
            "instance datatype is 'DataType.CHARACTER' which means the "
            "constant value is expected to be") in str(error.value)
    assert "'str'>' but found " in str(error.value)
    assert "'int'>'." in str(error.value)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', DataType.BOOLEAN, constant_value="hello")
    assert ("Error setting constant value for symbol 'a'. This DataSymbol "
            "instance datatype is 'DataType.BOOLEAN' which means the constant"
            " value is expected to be") in str(error.value)
    assert "'bool'>' but found " in str(error.value)
    assert "'str'>'." in str(error.value)


def test_datasymbol_is_constant():
    '''Test that the DataSymbol is_constant property returns True if a
    constant value is set and False if it is not.

    '''
    sym = DataSymbol('a', DataType.INTEGER)
    assert not sym.is_constant
    sym.constant_value = 9
    assert sym.is_constant


def test_datasymbol_scalar_array():
    '''Test that the DataSymbol property is_scalar returns True if the
    DataSymbol is a scalar and False if not and that the DataSymbol property
    is_array returns True if the DataSymbol is an array and False if not.

    '''
    sym1 = DataSymbol("s1", DataType.INTEGER)
    sym2 = DataSymbol("s2", DataType.REAL,
                      [DataSymbol.Extent.ATTRIBUTE, 2, sym1])
    assert sym1.is_scalar
    assert not sym1.is_array
    assert not sym2.is_scalar
    assert sym2.is_array


def test_datasymbol_invalid_interface():
    ''' Check that the DataSymbol.interface setter rejects the supplied value
    if it is not a DataSymbolInterface. '''
    sym = DataSymbol("some_var", DataType.REAL)
    with pytest.raises(TypeError) as err:
        sym.interface = "invalid interface spec"
    assert "interface to a DataSymbol must be a DataSymbolInterface but" \
        in str(err.value)


def test_datasymbol_interface():
    ''' Check the interface getter on a DataSymbol. '''
    my_mod = ContainerSymbol("my_mod")
    symbol = DataSymbol("some_var", DataType.REAL,
                        interface=GlobalInterface(my_mod))
    assert symbol.interface.container_symbol.name == "my_mod"


def test_datasymbol_interface_setter():
    ''' Check the interface setter on a DataSymbol. '''
    my_mod = ContainerSymbol("my_mod")
    symbol = DataSymbol("some_var", DataType.REAL,
                        interface=GlobalInterface(my_mod))
    assert symbol.interface.container_symbol.name == "my_mod"

    with pytest.raises(TypeError) as err:
        symbol.interface = "not valid"
    assert ("interface to a DataSymbol must be a DataSymbolInterface but "
            "got 'str'" in str(err.value))


def test_datasymbol_interface_access():
    ''' Tests for the DataSymbolInterface.access setter. '''
    symbol = DataSymbol("some_var", DataType.REAL,
                        interface=ArgumentInterface())
    symbol.interface.access = ArgumentInterface.Access.READ
    assert symbol.interface.access == ArgumentInterface.Access.READ
    # Force the error by supplying a string instead of a SymbolAccess type.
    with pytest.raises(TypeError) as err:
        symbol.interface.access = "read"
    assert "must be a 'ArgumentInterface.Access' but got " in str(err.value)


def test_datasymbol_argument_str():
    ''' Check the __str__ method of the ArgumentInterface class. '''
    # An ArgumentInterface represents a routine argument by default.
    interface = ArgumentInterface()
    assert str(interface) == "Argument(pass-by-value=False)"


def test_fortranglobal_str():
    ''' Test the __str__ method of GlobalInterface. '''
    # If it's not an argument then we have nothing else to say about it (since
    # other options are language specific and are implemented in sub-classes).
    my_mod = ContainerSymbol("my_mod")
    interface = GlobalInterface(my_mod)
    assert str(interface) == "Global(container='my_mod')"


def test_global_modname():
    ''' Test the GlobalInterface.module_name setter error conditions. '''
    with pytest.raises(TypeError) as err:
        _ = GlobalInterface(None)
    assert ("Global container_symbol parameter must be of type"
            " ContainerSymbol, but found ") in str(err.value)


def test_datasymbol_copy():
    '''Test that the DataSymbol copy method produces a faithful separate copy
    of the original symbol.

    '''
    symbol = DataSymbol("myname", DataType.REAL, shape=[1, 2],
                        constant_value=None,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
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
    new_symbol._datatype = DataType.INTEGER
    new_symbol.shape[0] = 3
    new_symbol.shape[1] = 4
    new_symbol._interface = LocalInterface()

    assert symbol.name == "myname"
    assert symbol.datatype == DataType.REAL
    assert symbol.shape == [1, 2]
    assert not symbol.constant_value

    # Now check constant_value
    new_symbol._shape = []
    new_symbol.constant_value = 3

    assert symbol.shape == [1, 2]
    assert not symbol.constant_value


def test_datasymbol_copy_properties():
    '''Test that the DataSymbol copy_properties method works as expected.'''

    symbol = DataSymbol("myname", DataType.REAL, shape=[1, 2],
                        constant_value=None,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))

    # Check an exception is raised if an incorrect argument is passed in
    with pytest.raises(TypeError) as excinfo:
        symbol.copy_properties(None)
    assert ("Argument should be of type 'DataSymbol' but found 'NoneType'."
            "") in str(excinfo.value)

    new_symbol = DataSymbol("other_name", DataType.INTEGER, shape=[],
                            constant_value=7)

    symbol.copy_properties(new_symbol)

    assert symbol.name == "myname"
    assert symbol.datatype == DataType.INTEGER
    assert symbol.shape == []
    assert symbol.is_local
    assert isinstance(symbol.constant_value, Literal)
    assert symbol.constant_value.value == "7"
    assert symbol.constant_value.datatype == DataType.INTEGER


def test_datasymbol_resolve_deferred():
    ''' Test the datasymbol resolve_deferred method '''

    container = Container("dummy_module")
    container.symbol_table.add(DataSymbol('a', DataType.INTEGER))
    container.symbol_table.add(DataSymbol('b', DataType.REAL))
    container.symbol_table.add(DataSymbol('c', DataType.REAL,
                                          constant_value=3.14))
    module = ContainerSymbol("dummy_module")
    module._reference = container  # Manually linking the container

    symbol = DataSymbol('a', DataType.DEFERRED,
                        interface=GlobalInterface(module))
    symbol.resolve_deferred()
    assert symbol.datatype == DataType.INTEGER

    symbol = DataSymbol('b', DataType.DEFERRED,
                        interface=GlobalInterface(module))
    symbol.resolve_deferred()
    assert symbol.datatype == DataType.REAL

    symbol = DataSymbol('c', DataType.DEFERRED,
                        interface=GlobalInterface(module))
    symbol.resolve_deferred()
    assert symbol.datatype == DataType.REAL
    assert isinstance(symbol.constant_value, Literal)
    assert symbol.constant_value.datatype == DataType.REAL
    assert symbol.constant_value.value == "3.14"

    # Test with a symbol not defined in the linked container
    symbol = DataSymbol('d', DataType.DEFERRED,
                        interface=GlobalInterface(module))
    with pytest.raises(SymbolError) as err:
        symbol.resolve_deferred()
    assert ("Error trying to resolve symbol 'd' properties. The interface "
            "points to module 'dummy_module' but could not find the definition"
            " of 'd' in that module." in str(err.value))

    # Test with a symbol which does not have a Global interface
    symbol = DataSymbol('e', DataType.DEFERRED, interface=LocalInterface())
    with pytest.raises(NotImplementedError) as err:
        symbol.resolve_deferred()
    assert ("Error trying to resolve symbol 'e' properties, the lazy "
            "evaluation of 'Local' interfaces is not supported."
            in str(err.value))
