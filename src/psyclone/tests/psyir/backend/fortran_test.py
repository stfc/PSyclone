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
# Author R. W. Ford, STFC Daresbury Lab
# Modified by A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.fortran module'''


from collections import OrderedDict
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.backend.fortran import gen_intent, gen_datatype, \
    FortranWriter, precedence
from psyclone.psyir.nodes import (
    Assignment, Node, CodeBlock, Container, Literal, UnaryOperation,
    BinaryOperation, Reference, Call, KernelSchedule,
    ArrayReference, ArrayOfStructuresReference, Range, StructureReference,
    Schedule, Routine, Return, FileContainer, IfBlock, OMPTaskloopDirective,
    OMPMasterDirective, OMPParallelDirective, Loop, OMPNumTasksClause,
    OMPDependClause, IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, SymbolTable, ContainerSymbol, RoutineSymbol, Symbol,
    ImportInterface, ArgumentInterface, UnresolvedInterface, StaticInterface,
    ScalarType, ArrayType, INTEGER_TYPE, REAL_TYPE, CHARACTER_TYPE,
    BOOLEAN_TYPE, REAL_DOUBLE_TYPE, UnresolvedType,
    UnsupportedType, UnsupportedFortranType, DataTypeSymbol, StructureType)
from psyclone.errors import InternalError
from psyclone.tests.utilities import Compile
from psyclone.psyGen import PSyFactory
from psyclone.nemo import NemoInvokeSchedule


def test_gen_intent():
    '''Check the gen_intent function produces the expected intent
    strings.

    '''
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    assert gen_intent(symbol) is None
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READ))
    assert gen_intent(symbol) == "in"
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.WRITE))
    assert gen_intent(symbol) == "out"
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
    assert gen_intent(symbol) == "inout"
    symbol = DataSymbol("dummy", INTEGER_TYPE)  # Non-argument
    assert gen_intent(symbol) is None


def test_gen_intent_error(monkeypatch):
    '''Check the gen_intent function raises an exception if an unsupported
    access type is found.

    '''
    symbol = DataSymbol("dummy", INTEGER_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    monkeypatch.setattr(symbol.interface, "_access", "UNSUPPORTED")
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_intent(symbol)
    assert "Unsupported access ''UNSUPPORTED'' found." in str(excinfo.value)


def test_gen_indices(fortran_writer):
    '''Check the gen_indices function produces the expected dimension
    strings.

    '''
    arg = DataSymbol("arg", INTEGER_TYPE,
                     interface=ArgumentInterface(
                         ArgumentInterface.Access.UNKNOWN))
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    literal = Literal("4", INTEGER_TYPE)
    one = Literal("1", scalar_type)
    arg_plus_1 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(arg), one)
    array_type = ArrayType(
        INTEGER_TYPE, [Reference(arg), 2, (0, 4), literal, arg_plus_1,
                       (2, arg_plus_1.copy())])
    assert (fortran_writer.gen_indices(array_type.shape) ==
            ["arg", "2", "0:4", "4", "arg + 1_4", "2:arg + 1_4"])
    bray_type = ArrayType(INTEGER_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                         ArrayType.Extent.ATTRIBUTE])
    assert fortran_writer.gen_indices(bray_type.shape) == [":", ":"]


def test_gen_indices_error(fortran_writer):
    '''Check the _gen_indices method raises an exception if a symbol shape
    entry is not supported.

    '''
    with pytest.raises(NotImplementedError) as excinfo:
        _ = fortran_writer.gen_indices(["invalid"])
    assert "unsupported gen_indices index 'invalid'" in str(excinfo.value)


@pytest.mark.parametrize(
    "type_name,result",
    [(ScalarType.Intrinsic.REAL, "real"),
     (ScalarType.Intrinsic.INTEGER, "integer"),
     (ScalarType.Intrinsic.CHARACTER, "character"),
     (ScalarType.Intrinsic.BOOLEAN, "logical")])
def test_gen_datatype_default_precision(type_name, result):
    '''Check for all supported datatype names that the gen_datatype
    function produces the expected Fortran types for scalar and arrays
    when no explicit precision is provided.

    Note, in the future PSyclone should be extended to set default
    precision in a config file.

    '''
    scalar_type = ScalarType(type_name, ScalarType.Precision.UNDEFINED)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol("dummy", my_type)
        assert gen_datatype(symbol.datatype, symbol.name) == result


@pytest.mark.parametrize(
    "type_name,precision,result",
    [(ScalarType.Intrinsic.REAL, ScalarType.Precision.SINGLE, "real"),
     (ScalarType.Intrinsic.REAL, ScalarType.Precision.DOUBLE,
      "double precision"),
     (ScalarType.Intrinsic.INTEGER, ScalarType.Precision.SINGLE, "integer"),
     (ScalarType.Intrinsic.INTEGER, ScalarType.Precision.DOUBLE, "integer"),
     (ScalarType.Intrinsic.CHARACTER, ScalarType.Precision.SINGLE,
      "character"),
     (ScalarType.Intrinsic.CHARACTER, ScalarType.Precision.DOUBLE,
      "character"),
     (ScalarType.Intrinsic.BOOLEAN, ScalarType.Precision.SINGLE, "logical"),
     (ScalarType.Intrinsic.BOOLEAN, ScalarType.Precision.DOUBLE, "logical")])
def test_gen_datatype_relative_precision(type_name, precision, result):
    '''Check for all supported datatype names that the gen_datatype
    function produces the expected Fortran types for scalar and arrays
    when relative precision is provided.

    '''
    scalar_type = ScalarType(type_name, precision=precision)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol("dummy", my_type)
        assert gen_datatype(symbol.datatype, symbol.name) == result


@pytest.mark.parametrize("precision", [1, 2, 4, 8, 16, 32])
@pytest.mark.parametrize("type_name,fort_name",
                         [(ScalarType.Intrinsic.INTEGER, "integer"),
                          (ScalarType.Intrinsic.BOOLEAN, "logical")])
def test_gen_datatype_absolute_precision(type_name, precision, fort_name):
    '''Check for the integer and logical datatype names that the
    gen_datatype function produces the expected Fortran types for
    scalar and arrays when explicit precision is provided.

    All should pass except 32. Other types are tested separately.

    '''
    symbol_name = "dummy"
    scalar_type = ScalarType(type_name, precision=precision)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol(symbol_name, my_type)
        if precision in [32]:
            with pytest.raises(VisitorError) as excinfo:
                gen_datatype(symbol.datatype, symbol.name)
            assert (f"Datatype '{fort_name}' in symbol '{symbol_name}' "
                    f"supports fixed precision of [1, 2, 4, 8, 16] but "
                    f"found '{precision}'."
                    in str(excinfo.value))
        else:
            assert (gen_datatype(symbol.datatype, symbol.name) ==
                    f"{fort_name}*{precision}")


@pytest.mark.parametrize(
    "precision", [1, 2, 4, 8, 16, 32])
def test_gen_datatype_absolute_precision_real(precision):
    '''Check for the real datatype name that the gen_datatype function
    produces the expected Fortran types for scalars and arrays when
    explicit precision is provided.

    All should pass except 1, 2 and 32.

    '''
    symbol_name = "dummy"
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, precision=precision)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol(symbol_name, my_type)
        if precision in [1, 2, 32]:
            with pytest.raises(VisitorError) as excinfo:
                gen_datatype(symbol.datatype, symbol.name)
            assert (f"Datatype 'real' in symbol '{symbol_name}' supports "
                    f"fixed precision of [4, 8, 16] but found '{precision}'."
                    in str(excinfo.value))
        else:
            assert (gen_datatype(symbol.datatype, symbol.name) ==
                    f"real*{precision}")


def test_gen_datatype_absolute_precision_character():
    '''Check for the character datatype name that the
    gen_datatype function produces the expected Fortran types for
    scalars and arrays when explicit precision is provided.

    '''
    symbol_name = "dummy"
    scalar_type = ScalarType(ScalarType.Intrinsic.CHARACTER, precision=4)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        symbol = DataSymbol(symbol_name, my_type)
        with pytest.raises(VisitorError) as excinfo:
            gen_datatype(symbol.datatype, symbol.name)
        assert (f"Explicit precision not supported for datatype 'character' "
                f"in symbol '{symbol_name}' in Fortran backend."
                in str(excinfo.value))


@pytest.mark.parametrize(
    "type_name,result",
    [(ScalarType.Intrinsic.REAL, "real"),
     (ScalarType.Intrinsic.INTEGER, "integer"),
     (ScalarType.Intrinsic.CHARACTER, "character"),
     (ScalarType.Intrinsic.BOOLEAN, "logical")])
def test_gen_datatype_kind_precision(type_name, result):
    '''Check for all supported datatype names that the gen_datatype
    function produces the expected Fortran types for scalars and
    arrays when precision is provided via another symbol.

    '''
    precision_name = "prec_def"
    symbol_name = "dummy"
    precision = DataSymbol(precision_name, INTEGER_TYPE)
    scalar_type = ScalarType(type_name, precision=precision)
    array_type = ArrayType(scalar_type, [10, 10])
    for my_type in [scalar_type, array_type]:
        if type_name == ScalarType.Intrinsic.CHARACTER:
            with pytest.raises(VisitorError) as excinfo:
                gen_datatype(my_type, symbol_name)
            assert (f"kind not supported for datatype 'character' in symbol "
                    f"'{symbol_name}' in Fortran backend."
                    in str(excinfo.value))
        else:
            assert (gen_datatype(my_type, symbol_name) ==
                    f"{result}(kind={precision_name})")


def test_gen_datatype_derived_type():
    ''' Check that gen_datatype handles derived types. '''
    # A symbol representing a single derived type
    tsym = DataTypeSymbol("my_type", UnresolvedType())
    assert gen_datatype(tsym, "my_type") == "type(my_type)"
    # An array of derived types
    atype = ArrayType(tsym, [10])
    assert gen_datatype(atype, "my_list") == "type(my_type)"


def test_gen_datatype_exception_1():
    '''Check that an exception is raised if gen_datatype is called with a
    symbol containing an unsupported datatype.

    '''
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    symbol = DataSymbol("fred", data_type)
    symbol.datatype._intrinsic = None
    with pytest.raises(NotImplementedError) as excinfo:
        _ = gen_datatype(symbol.datatype, symbol.name)
    assert ("Unsupported datatype 'None' for symbol 'fred' found in "
            "gen_datatype()." in str(excinfo.value))


def test_gen_datatype_exception_2():
    '''Check that an exception is raised if gen_datatype is called with a
    symbol containing an unsupported precision.

    '''
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    symbol = DataSymbol("fred", data_type)
    symbol.datatype._precision = None
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_datatype(symbol.datatype, symbol.name)
    assert ("Unsupported precision type 'NoneType' found for symbol 'fred' "
            "in Fortran backend." in str(excinfo.value))


# Commented this test out until #11 is addressed.
# @pytest.mark.xfail(reason="issue #11 backend logging output is affected by "
#                    "some other part PSyclone.")
# def test_gen_datatype_precision_log(caplog):
#     '''Check the gen_datatype function produces a logging warning if
#     relative precision is specified for a Fortran datatype that does
#     not support relative precision (only real/double precision
#     supports it)
#
#     '''
#     import logging
#     with caplog.at_level(logging.WARNING):
#         symbol = Symbol("dummy", INTEGER_TYPE,
#                         precision=Symbol.Precision.DOUBLE)
#         _ = gen_datatype(symbol)
#         assert (
#             "WARNING  Fortran does not support relative precision for the "
#             "'integer' datatype but 'Precision.DOUBLE' was specified for "
#             "variable 'dummy'." in caplog.text)


def test_gen_typedecl_validation(fortran_writer, monkeypatch):
    ''' Test the various validation checks in gen_typedecl(). '''
    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_typedecl("hello")
    assert ("gen_typedecl expects a DataTypeSymbol as argument but got: 'str'"
            in str(err.value))
    # UnsupportedType is abstract so we create an UnsupportedFortranType and
    # then monkeypatch it.
    tsymbol = DataTypeSymbol("my_type", UnsupportedFortranType(
        "type my_type\nend type my_type"))
    monkeypatch.setattr(tsymbol.datatype, "__class__", UnsupportedType)

    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_typedecl(tsymbol)
    assert ("cannot generate code for symbol 'my_type' of type "
            "'UnsupportedType'" in str(err.value))
    # Symbol with an invalid visibility
    dtype = StructureType.create([
        ("flag", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("secret", INTEGER_TYPE, Symbol.Visibility.PRIVATE, None)])
    tsymbol = DataTypeSymbol("my_type", dtype)
    tsymbol._visibility = "wrong"
    with pytest.raises(InternalError) as err:
        fortran_writer.gen_typedecl(tsymbol, include_visibility=True)
    assert ("visibility must be one of Symbol.Visibility.PRIVATE/PUBLIC but "
            "'my_type' has visibility of type 'str'" in str(err.value))
    # Symbol of UnresolvedType.
    tsymbol = DataTypeSymbol("my_type", UnresolvedType())
    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_typedecl(tsymbol)
    assert ("Local Symbol 'my_type' is of UnresolvedType and therefore no "
            "declaration can be created for it." in str(err.value))


def test_gen_typedecl_unsupported_fortran_type(fortran_writer):
    ''' Check that gen_typedecl() works for a symbol of UnsupportedFortranType.
    '''
    tsymbol = DataTypeSymbol("my_type", UnsupportedFortranType(
        "type :: my_type\nend type my_type"),
                             visibility=Symbol.Visibility.PUBLIC)
    assert (fortran_writer.gen_typedecl(tsymbol) ==
            "type, public :: my_type\nend type my_type\n")
    tsymbol.visibility = Symbol.Visibility.PRIVATE
    assert (fortran_writer.gen_typedecl(tsymbol) ==
            "type, private :: my_type\nend type my_type\n")
    assert (fortran_writer.gen_typedecl(tsymbol, include_visibility=False) ==
            "type :: my_type\nend type my_type\n")


def test_gen_typedecl_unsupported_fortran_type_missing_colons(fortran_writer):
    ''' Check that gen_typedecl() raises a NotImplementedError if the original
    declaration is missing a '::'. '''
    tsymbol = DataTypeSymbol("my_type", UnsupportedFortranType(
        "type my_type\nend type my_type"))
    with pytest.raises(NotImplementedError) as err:
        fortran_writer.gen_typedecl(tsymbol)
    assert ("Cannot add accessibility information to an UnsupportedFortranType"
            " that does not have '::' in its original declaration: 'type "
            "my_type" in str(err.value))


def test_gen_typedecl_unsupported_fortran_type_wrong_vis(fortran_writer):
    ''' Check that gen_typedecl() raises the expected error when the visibility
    of a symbol of UnsupportedFortranType does not match what is stored
    in its declaration text. '''
    tsymbol = DataTypeSymbol("my_type", UnsupportedFortranType(
        "type, prIVate :: my_type\nend type my_type"),
                             visibility=Symbol.Visibility.PUBLIC)
    with pytest.raises(InternalError) as err:
        fortran_writer.gen_typedecl(tsymbol)
    assert ("Symbol 'my_type' of UnsupportedFortranType has public visibility "
            "but its associated declaration specifies that it is private: "
            "'type, prIVate ::" in str(err.value))
    tsymbol2 = DataTypeSymbol("my_type", UnsupportedFortranType(
        "type, pUBlic :: my_type\nend type my_type"),
                              visibility=Symbol.Visibility.PRIVATE)
    with pytest.raises(InternalError) as err:
        fortran_writer.gen_typedecl(tsymbol2)
    assert ("Symbol 'my_type' of UnsupportedFortranType has private visibility"
            " but its associated declaration specifies that it is public: "
            "'type, pUBlic ::" in str(err.value))


def test_gen_typedecl(fortran_writer):
    ''' Test normal operation of gen_typedecl(). '''
    atype = ArrayType(REAL_TYPE, [3, 5])
    dynamic_atype = ArrayType(REAL_TYPE, [ArrayType.Extent.DEFERRED])
    tsymbol = DataTypeSymbol("grid_type", UnresolvedType())
    dtype = StructureType.create([
        # Scalar integer
        ("flag", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        # Private, scalar integer
        ("secret", INTEGER_TYPE, Symbol.Visibility.PRIVATE, None),
        # Static array
        ("matrix", atype, Symbol.Visibility.PUBLIC, None),
        # Allocatable array
        ("data", dynamic_atype, Symbol.Visibility.PUBLIC, None),
        # Derived type
        ("grid", tsymbol, Symbol.Visibility.PRIVATE, None)])
    tsymbol = DataTypeSymbol("my_type", dtype)
    assert (fortran_writer.gen_typedecl(tsymbol) ==
            "type, public :: my_type\n"
            "  integer, public :: flag\n"
            "  integer, private :: secret\n"
            "  real, dimension(3,5), public :: matrix\n"
            "  real, allocatable, dimension(:), public :: data\n"
            "  type(grid_type), private :: grid\n"
            "end type my_type\n")
    private_tsymbol = DataTypeSymbol("my_type", dtype,
                                     Symbol.Visibility.PRIVATE)
    gen_code = fortran_writer.gen_typedecl(private_tsymbol)
    assert gen_code.startswith("type, private :: my_type\n")


def test_reverse_map():
    '''Check that the internal _reverse_map function returns a map with
    the expected behaviour

    '''
    result = {}
    FortranWriter._reverse_map(result, OrderedDict([('+', 'PLUS')]))
    assert isinstance(result, dict)
    assert result['PLUS'] == '+'


def test_reverse_map_duplicates():
    '''Check that the internal _reverse_map function returns a map with
    the expected behaviour when there are duplicates in the items of
    the input ordered dictionary. It should use the first one found.

    '''
    result = {}
    FortranWriter._reverse_map(result, OrderedDict([('==', 'EQUAL'),
                                                    ('.eq.', 'EQUAL')]))
    assert isinstance(result, dict)
    assert result['EQUAL'] == '=='
    assert len(result) == 1

    result = {}
    FortranWriter._reverse_map(result, OrderedDict([('.EQ.', 'EQUAL'),
                                                    ('==', 'EQUAL')]))
    assert isinstance(result, dict)
    assert result['EQUAL'] == '.EQ.'
    assert len(result) == 1


@pytest.mark.parametrize("operator,result",
                         [(UnaryOperation.Operator.MINUS, "-"),
                          (BinaryOperation.Operator.MUL, "*")])
def test_get_operator(operator, result):
    '''Check that the get_operator function returns the expected
    values when provided with valid unary, binary and nary operators.

    '''
    assert result == FortranWriter().get_operator(operator)


def test_get_operator_error():
    '''Check that the get_operator function raises the expected
    exception when an unknown operator is provided.

    '''
    with pytest.raises(KeyError):
        _ = FortranWriter().get_operator(None)


def test_precedence():
    '''Check that the precedence function returns the expected relative
    precedence values.

    '''
    assert precedence('.OR.') < precedence('.AND.')
    assert precedence('*') < precedence('**')
    assert precedence('.EQ.') == precedence('==')
    assert precedence('*') == precedence('/')
    assert precedence('.EQV.') == precedence('.NEQV.')


def test_precedence_error():
    '''Check that the precedence function returns the expected exception
    if an unknown operator is provided.

    '''
    with pytest.raises(KeyError):
        _ = precedence('invalid')


def test_gen_arguments_validation(fortran_writer):
    '''Check that the _gen_arguments validation function works as
    expected.

    '''
    # type error
    with pytest.raises(TypeError) as info:
        fortran_writer._gen_arguments(None)
    assert ("The _gen_arguments utility function expects a "
            "Call node, but found 'NoneType'." in str(info.value))
    # visitor error
    call = Call.create(RoutineSymbol("hello"), [
        ("name", Literal("1.0", REAL_TYPE)), Literal("2.0", REAL_TYPE)])
    with pytest.raises(VisitorError) as info:
        fortran_writer._gen_arguments(call)
    assert ("Fortran expects all named arguments to occur after all "
            "positional arguments but this is not the case for "
            "Call[name='hello']" in str(info.value))
    # ok
    call = Call.create(RoutineSymbol("hello"), [
        Literal("1.0", REAL_TYPE), ("name", Literal("2.0", REAL_TYPE))])
    output = fortran_writer._gen_arguments(call)
    assert isinstance(output, str)
    assert output == "1.0, name=2.0"


def test_fw_gen_use(fortran_writer):
    '''Check the FortranWriter class gen_use method produces the expected
    declaration. Also check that an exception is raised if the symbol
    does not describe a Container.

    '''
    symbol_table = SymbolTable()
    container_symbol = ContainerSymbol("my_module")
    symbol_table.add(container_symbol)
    symbol = DataSymbol("dummy1", UnresolvedType(),
                        interface=ImportInterface(
                            container_symbol, orig_name="orig_name"))
    symbol_table.add(symbol)
    symbol = RoutineSymbol(
        "my_sub", interface=ImportInterface(container_symbol))
    symbol_table.add(symbol)
    result = fortran_writer.gen_use(container_symbol, symbol_table)
    assert result == "use my_module, only : dummy1=>orig_name, my_sub\n"

    container_symbol.wildcard_import = True
    result = fortran_writer.gen_use(container_symbol, symbol_table)
    assert "use my_module, only : dummy1=>orig_name, my_sub" not in result
    assert "use my_module\n" in result

    # container2 has no symbols associated with it and has not been marked
    # as having a wildcard import. It should therefore result in a USE
    # with an empty 'ONLY' list (which serves to keep the module in scope
    # while not accessing any data from it).
    container2 = ContainerSymbol("my_mod2")
    symbol_table.add(container2)
    result = fortran_writer.gen_use(container2, symbol_table)
    assert result == "use my_mod2, only :\n"
    # If we now add a wildcard import of this module then that's all we
    # should get from the backend (as it makes the "only:" redundant)
    container2.wildcard_import = True
    result = fortran_writer.gen_use(container2, symbol_table)
    assert result == "use my_mod2\n"
    # Wrong type for first argument
    symbol2 = DataSymbol("dummy2", UnresolvedType(),
                         interface=ImportInterface(container_symbol))
    symbol_table.add(symbol2)
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.gen_use(symbol2, symbol_table)
    assert ("expects a ContainerSymbol as its first argument but got "
            "'DataSymbol'" in str(excinfo.value))
    # Wrong type for second argument
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.gen_use(container2, symbol2)
    assert ("expects a SymbolTable as its second argument but got 'DataSymbol'"
            in str(excinfo.value))
    # Symbol not in SymbolTable
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.gen_use(ContainerSymbol("my_mod3"), symbol_table)
    assert ("the supplied symbol ('my_mod3') is not in the supplied "
            "SymbolTable" in str(excinfo.value))
    # A different ContainerSymbol with the same name as an entry in the
    # SymbolTable should be picked up
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.gen_use(ContainerSymbol("my_mod2"), symbol_table)
    assert ("the supplied symbol ('my_mod2') is not the same object as the "
            "entry" in str(excinfo.value))


def test_fw_gen_vardecl(fortran_writer):
    '''Check the FortranWriter class gen_vardecl method produces the
    expected declarations. Also check that an exception is raised if
    the symbol does not describe a valid variable declaration statement.

    '''
    # Basic entry
    symbol = DataSymbol("dummy1", INTEGER_TYPE)
    result = fortran_writer.gen_vardecl(symbol)
    assert result == "integer :: dummy1\n"

    # Assumed-size array with intent
    array_type = ArrayType(INTEGER_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                          (2, ArrayType.Extent.ATTRIBUTE),
                                          ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READ))
    result = fortran_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(:,2:,:), intent(in) :: dummy2\n"

    # Assumed-size array with unknown intent
    array_type = ArrayType(INTEGER_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    result = fortran_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(:,:,:) :: dummy2\n"

    # Assumed-size array with specified lower bound
    array_type = ArrayType(INTEGER_TYPE,
                           [ArrayType.Extent.ATTRIBUTE,
                            ArrayType.Extent.ATTRIBUTE,
                            (-1, ArrayType.Extent.ATTRIBUTE)])
    symbol = DataSymbol("dummy3", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    result = fortran_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(:,:,-1:) :: dummy3\n"

    # Allocatable array
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.DEFERRED,
                                       ArrayType.Extent.DEFERRED])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
    result = fortran_writer.gen_vardecl(symbol)
    assert (result ==
            "real, allocatable, dimension(:,:), intent(inout) :: dummy2\n")

    # Constant.
    symbol = DataSymbol("dummy3", INTEGER_TYPE, is_constant=True,
                        initial_value=10)
    result = fortran_writer.gen_vardecl(symbol)
    assert result == "integer, parameter :: dummy3 = 10\n"

    # Constant with top level intrinsic
    initval = IntrinsicCall.create(IntrinsicCall.Intrinsic.SIN,
                                   [Literal("10", INTEGER_TYPE)])
    symbol = DataSymbol("dummy3i", INTEGER_TYPE, is_constant=True,
                        initial_value=initval)
    result = fortran_writer.gen_vardecl(symbol)
    assert result == "integer, parameter :: dummy3i = SIN(10)\n"

    # Symbol has initial value but is not constant (static). This is a property
    # of the Fortran language and therefore is only checked for when we attempt
    # to generate Fortran.
    symbol = DataSymbol("dummy3a", INTEGER_TYPE, initial_value=10)
    with pytest.raises(VisitorError) as err:
        _ = fortran_writer.gen_vardecl(symbol)
    assert ("'dummy3a' has an initial value (10) and therefore (in Fortran) "
            "must have a StaticInterface. However it has an interface of "
            "'Automatic'" in str(err.value))
    symbol.interface = StaticInterface()
    result = fortran_writer.gen_vardecl(symbol)
    assert result == "integer, save :: dummy3a = 10\n"

    # Use statement
    symbol = DataSymbol("dummy1", UnresolvedType(),
                        interface=ImportInterface(
                            ContainerSymbol("my_module")))
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.gen_vardecl(symbol)
    assert ("Symbol 'dummy1' has a UnresolvedType and we can not generate "
            "a declaration for UnresolvedTypes." in str(excinfo.value))

    # An unresolved symbol
    symbol = DataSymbol("dummy1", UnresolvedType(),
                        interface=UnresolvedInterface())
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.gen_vardecl(symbol)
    assert ("Symbol 'dummy1' has a UnresolvedType and we can not generate a "
            "declaration for UnresolvedTypes." in str(excinfo.value))


def test_fw_gen_vardecl_visibility(fortran_writer):
    ''' Test the include_visibility argument to gen_vardecl(). '''
    # Simple constant
    symbol = DataSymbol("dummy3", INTEGER_TYPE,
                        visibility=Symbol.Visibility.PUBLIC, is_constant=True,
                        initial_value=10)
    # Expect include_visibility to default to False
    result = fortran_writer.gen_vardecl(symbol)
    assert result == "integer, parameter :: dummy3 = 10\n"
    result = fortran_writer.gen_vardecl(symbol, include_visibility=False)
    assert result == "integer, parameter :: dummy3 = 10\n"
    result = fortran_writer.gen_vardecl(symbol, include_visibility=True)
    assert result == "integer, parameter, public :: dummy3 = 10\n"

    # Known type but invalid visibility
    symbol._visibility = "wrong"
    with pytest.raises(InternalError) as err:
        fortran_writer.gen_vardecl(symbol, include_visibility=True)
    assert ("A Symbol must be either public or private but symbol 'dummy3' "
            "has visibility 'wrong'" in str(err.value))

    # A symbol of unknown Fortran type
    symbol = DataSymbol("var", UnsupportedFortranType(
                                    "type :: var\n"
                                    "  integer, private :: id\n"
                                    "  integer, public :: flag\n"
                                    "end type var"),
                        visibility=Symbol.Visibility.PRIVATE)
    result = fortran_writer.gen_vardecl(symbol)
    assert result == ("type :: var\n"
                      "  integer, private :: id\n"
                      "  integer, public :: flag\n"
                      "end type var\n")
    result = fortran_writer.gen_vardecl(symbol, include_visibility=True)
    assert result == ("type, private :: var\n"
                      "  integer, private :: id\n"
                      "  integer, public :: flag\n"
                      "end type var\n")


def test_gen_default_access_stmt(fortran_writer):
    '''
    Tests for the gen_default_access_stmt method of FortranWriter.
    '''
    symbol_table = SymbolTable()
    # If no default visibility is specified then the Fortran default
    # is 'public'
    symbol_table._default_visibility = None
    assert fortran_writer.gen_default_access_stmt(symbol_table) == "public\n"
    symbol_table.default_visibility = Symbol.Visibility.PUBLIC
    # Test indentation works as expected
    fortran_writer._depth += 1
    assert fortran_writer.gen_default_access_stmt(symbol_table) == "  public\n"
    symbol_table.default_visibility = Symbol.Visibility.PRIVATE
    assert (fortran_writer.gen_default_access_stmt(symbol_table) ==
            "  private\n")
    fortran_writer._depth -= 1
    assert fortran_writer.gen_default_access_stmt(symbol_table) == "private\n"
    # Invalid type (str instead of Symbol.Visibility)
    symbol_table._default_visibility = "public"
    with pytest.raises(InternalError) as err:
        fortran_writer.gen_default_access_stmt(symbol_table)
    assert ("Unrecognised visibility ('public') found when attempting to "
            "generate access statement. Should be either 'Symbol.Visibility."
            "PUBLIC' or 'Symbol.Visibility.PRIVATE'" in str(err.value))


def test_gen_access_stmts(fortran_writer):
    '''
    Tests for the gen_access_stmts method of FortranWriter.
    '''
    symbol_table = SymbolTable()
    symbol_table.add(RoutineSymbol("my_sub1",
                                   visibility=Symbol.Visibility.PUBLIC))
    code = fortran_writer.gen_access_stmts(symbol_table)
    # Default visibility of the table is public so no explicit access
    # statement required
    assert code == ""
    sub2 = RoutineSymbol("my_sub2", visibility=Symbol.Visibility.PRIVATE)
    symbol_table.add(sub2)
    code = fortran_writer.gen_access_stmts(symbol_table)
    assert "private :: my_sub2\n" in code
    # Check that the interface of the symbol does not matter
    symbol_table.add(
        RoutineSymbol("used_sub", visibility=Symbol.Visibility.PRIVATE,
                      interface=ImportInterface(ContainerSymbol("some_mod"))))
    code = fortran_writer.gen_access_stmts(symbol_table)
    assert "private :: my_sub2, used_sub\n" in code
    # Since the default visibility of the table is PUBLIC, we should not
    # generate anything for PUBLIC symbols.
    symbol_table.add(
        Symbol("some_var", visibility=Symbol.Visibility.PUBLIC,
               interface=UnresolvedInterface()))
    code = fortran_writer.gen_access_stmts(symbol_table)
    assert code.strip() == "private :: my_sub2, used_sub"
    # Change the default visibility of the table to be private.
    symbol_table.default_visibility = Symbol.Visibility.PRIVATE
    code = fortran_writer.gen_access_stmts(symbol_table)
    assert code.strip() == "public :: my_sub1, some_var"
    # Check that we don't generate an accessibility statement for a
    # RoutineSymbol tagged with 'own_routine_symbol'
    symbol_table.add(RoutineSymbol("my_routine",
                                   visibility=Symbol.Visibility.PUBLIC),
                     tag='own_routine_symbol')
    code = fortran_writer.gen_access_stmts(symbol_table)
    assert "my_routine" not in code


def test_fw_exception(fortran_writer):
    '''Check the FortranWriter class instance raises an exception if an
    unsupported PSyIR node is found.

    '''
    # 'unsupported' should be a node that neither its generic classes nor
    # itself have a visitor implemented.
    unsupported = Node()

    # Generate Fortran from the given PSyIR
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer(unsupported)
    assert "Unsupported node 'Node' found" in str(excinfo.value)


def test_fw_filecontainer_1(fortran_writer):
    '''Check the FortranWriter class outputs nothing when a
    FileContainer node with no content is found.

    '''
    file_container = FileContainer("None")
    result = fortran_writer(file_container)
    assert not result


def test_fw_filecontainer_2(fortran_writer):
    '''Check that an instance of the FortranWriter class outputs the
    expected code when a FileContainer contains multiple nodes (in
    this case a Container (module) and a Routine (subroutine).

    '''
    container = Container("mod_name")
    routine = Routine("sub_name")
    file_container = FileContainer.create(
        "None", SymbolTable(), [container, routine])
    result = fortran_writer(file_container)
    expected = (
        "module mod_name\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n\n"
        "end module mod_name\n"
        "subroutine sub_name()\n\n\n"
        "end subroutine sub_name\n")
    assert result == expected


def test_fw_filecontainer_error1(fortran_writer):
    '''Check that an instance of the FortranWriter class raises the
    expected exception if the symbol table associated with a
    FileContainer node contains any symbols.

    '''
    symbol_table = SymbolTable()
    symbol_table.add(Symbol("x"))
    file_container = FileContainer.create("None", symbol_table, [])
    with pytest.raises(VisitorError) as info:
        _ = fortran_writer(file_container)
    assert (
        "In the Fortran backend, a file container should not have any "
        "symbols associated with it, but found 1." in str(info.value))


def test_fw_filecontainer_error2(fortran_writer):
    '''Check that an instance of the FortranWriter class raises the
    expected exception if a FileContainer node contains more than one
    Routine node with is_program set (as only one program is allowed).

    '''
    program1 = Routine.create("prog1", SymbolTable(), [], is_program=True)
    program2 = Routine.create("prog2", SymbolTable(), [], is_program=True)
    file_container = FileContainer.create(
        "None", SymbolTable(), [program1, program2])
    with pytest.raises(VisitorError) as info:
        _ = fortran_writer(file_container)
    assert (
        "In the Fortran backend, a file container should contain at most one "
        "routine node that is a program, but found 2." in str(info.value))


def test_fw_container_1(fortran_writer, monkeypatch):
    '''Check the FortranWriter class outputs correct code when a Container
    node with no content is found. Also tests that an exception is
    raised if Container.name does not have a value.

    '''
    container = Container("test")
    result = fortran_writer(container)
    assert (
        "module test\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n\n"
        "end module test\n" in result)

    monkeypatch.setattr(container, "_name", None)
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer(container)
    assert ("Expected Container node name to have a value."
            in str(excinfo.value))


def test_fw_container_2(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class outputs correct code when a Container
    node is found with a subroutine, use statements and
    declarations. Also raise an exception if the Container contains a
    Container.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "use iso_c_binding, only : c_int\n"
        "implicit none\n"
        "real :: c,d\n"
        "contains\n"
        "subroutine tmp()\n"
        "end subroutine tmp\n"
        "end module test")
    container = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(container)

    assert (
        "module test\n"
        "  use iso_c_binding, only : c_int\n"
        "  implicit none\n"
        "  real, public :: c\n"
        "  real, public :: d\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine tmp()\n\n\n"
        "  end subroutine tmp\n\n"
        "end module test\n" in result)
    assert Compile(tmpdir).string_compiles(result)

    container.children[0].children.append(Container("child"))
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer(container)
    assert ("The Fortran back-end requires all children of a Container "
            "to be either CodeBlocks or sub-classes of Routine but found: "
            "['Routine', 'Container']" in str(excinfo.value))


def test_fw_container_3(fortran_reader, fortran_writer, monkeypatch):
    '''Check the FortranWriter class raises an exception when a Container
    node contains a symbol table with an argument declaration (as this
    does not make sense).

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "real :: a\n"
        "implicit none\n"
        "contains\n"
        "subroutine tmp()\n"
        "end subroutine tmp\n"
        "end module test")
    container = fortran_reader.psyir_from_source(code)
    module = container.children[0]
    symbol = module.symbol_table.lookup("a")
    monkeypatch.setattr(symbol, "_interface", ArgumentInterface())

    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer._visit(container)
    assert ("Arguments are not allowed in this context but this symbol table "
            "contains argument(s): '['a']'." in str(excinfo.value))


def test_fw_container_4(fortran_writer):
    '''Check the FortranWriter class outputs correct code when a Container
    symbol table has multiple imported modules.

    '''
    container = Container("test")
    container.symbol_table.add(ContainerSymbol("mod1"))
    container.symbol_table.lookup("mod1").wildcard_import = True
    container.symbol_table.add(ContainerSymbol("mod2"))
    container.symbol_table.lookup("mod2").wildcard_import = True
    container.symbol_table.add(ContainerSymbol("mod3"))
    container.symbol_table.lookup("mod3").wildcard_import = True
    # Default symbol visibility is public
    assert (
        "module test\n"
        "  use mod1\n"
        "  use mod2\n"
        "  use mod3\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n\n"
        "end module test\n" in fortran_writer(container))
    # Change the default visibility to private
    container.symbol_table.default_visibility = Symbol.Visibility.PRIVATE
    assert (
        "module test\n"
        "  use mod1\n"
        "  use mod2\n"
        "  use mod3\n"
        "  implicit none\n"
        "  private\n\n"
        "  contains\n\n"
        "end module test\n" in fortran_writer(container))


def test_fw_binaryoperator_unknown(fortran_reader, fortran_writer,
                                   monkeypatch):
    '''Check the FortranWriter class binary_operation method raises an
    exception if an unknown binary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = 1.0 * 1.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)
    # Remove MUL from the list of supported binary operators
    monkeypatch.delitem(fortran_writer._operator_2_str,
                        BinaryOperation.Operator.MUL)
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer(schedule)
    assert "Unexpected binary op" in str(excinfo.value)


def test_fw_binaryoperator_precedence(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class binary_operation method complies with
    the operator precedence rules. This is achieved by placing the
    operation in brackets.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  real :: a, b, c, d\n"
        "  logical :: e, f, g\n"
        "    a = b * (c + d)\n"
        "    a = b * c + d\n"
        "    a = (b * c) + d\n"
        "    a = b * c * d * a\n"
        "    a = (((b * c) * d) * a)\n"
        "    a = (b * (c * (d * a)))\n"
        "    a = -(a + b)\n"
        "    e = .not.(e .and. (f .or. g))\n"
        "    e = (((.not.e) .and. f) .or. g)\n"
        "    e = (e .and. (f .eqv. g))\n"
        "    e = (e .and. f .neqv. g)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)
    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    expected = (
        "    a = b * (c + d)\n"
        "    a = b * c + d\n"
        "    a = b * c + d\n"
        "    a = b * c * d * a\n"
        "    a = b * c * d * a\n"
        "    a = b * (c * (d * a))\n"
        "    a = -(a + b)\n"
        "    e = .NOT.(e .AND. (f .OR. g))\n"
        "    e = .NOT.e .AND. f .OR. g\n"
        "    e = e .AND. (f .EQV. g)\n"
        "    e = e .AND. f .NEQV. g\n")
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_mixed_operator_precedence(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class unary_operation and binary_operation
    methods complies with the operator precedence rules. This is
    achieved by placing the binary operation in brackets.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  real :: a, b, c, d\n"
        "  logical :: e, f, g\n"
        "    a = -a * (-b + c)\n"
        "    a = (-a) * (-b + c)\n"
        "    a = -a + (-b + (-c))\n"
        "    a = -a + (-b - (-c))\n"
        "    b = c * (-2.0)\n"
        "    a = abs(-b - (-c))\n"
        "    e = .not. f .or. .not. g\n"
        "    a = log(b*c)\n"
        "    a = b**(-c)\n"
        "    a = b**(-b + c)\n"
        "    a = (-b)**c\n"
        "    a = -(-b)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)
    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    expected = (
        "    a = -a * (-b + c)\n"
        "    a = -a * (-b + c)\n"
        "    a = -a + (-b + (-c))\n"
        "    a = -a + (-b - (-c))\n"
        "    b = c * (-2.0)\n"
        "    a = ABS(-b - (-c))\n"
        "    e = .NOT.f .OR. (.NOT.g)\n"
        "    a = LOG(b * c)\n"
        "    a = b ** (-c)\n"
        "    a = b ** (-b + c)\n"
        "    a = -b ** c\n"
        "    a = -(-b)\n")
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_naryoperator(fortran_reader, fortran_writer, tmpdir):
    ''' Check that the FortranWriter class nary_operation method correctly
    prints out the Fortran representation of an intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    assert "a = MAX(1.0, 1.0, 2.0)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_reference(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class reference method prints the
    appropriate information (the name of the reference it points to).

    '''
    # Generate fparser2 parse tree from Fortran code. The line of
    # interest is a(n) = 0.0. The additional a=1 line is added to get
    # round a bug in the parser.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = 1\n"
        "    a(n) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)

    assert (
        "  subroutine tmp(a, n)\n"
        "    integer, intent(in) :: n\n"
        "    real, dimension(n), intent(out) :: a\n"
        "\n"
        "    a = 1\n"
        "    a(n) = 0.0\n"
        "\n"
        "  end subroutine tmp\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_range(fortran_writer):
    '''Check the FortranWriter class range_node and arrayreference_node methods
    produce the expected code when an array section is specified.

    '''
    array_type = ArrayType(REAL_TYPE, [10, 10])
    symbol = DataSymbol("a", array_type)
    one = Literal("1", INTEGER_TYPE)
    two = Literal("2", INTEGER_TYPE)
    three = Literal("3", INTEGER_TYPE)
    dim1_bound_start = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", one.copy())])
    dim1_bound_stop = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(symbol), ("dim", one.copy())])
    dim2_bound_start = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", two.copy())])
    dim3_bound_start = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", three.copy())])
    dim3_bound_stop = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(symbol), ("dim", three.copy())])
    plus = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        Reference(DataSymbol("b", REAL_TYPE)),
        Reference(DataSymbol("c", REAL_TYPE)))
    range1 = Range.create(one.copy(), dim1_bound_stop)
    range2 = Range.create(dim2_bound_start, plus, step=three)
    # Check the ranges in isolation
    result = fortran_writer(range1)
    assert result == "1:UBOUND(a, dim=1)"
    result = fortran_writer(range2)
    assert result == "LBOUND(a, dim=2):b + c:3"
    # Check the ranges in context
    array = ArrayReference.create(
        symbol, [range1, range2])
    result = fortran_writer.arrayreference_node(array)
    assert result == "a(:,:b + c:3)"

    array_type = ArrayType(REAL_TYPE, [10, 10, 10])
    symbol = DataSymbol("a", array_type)
    array = ArrayReference.create(
        symbol,
        [Range.create(dim1_bound_start, dim1_bound_stop.copy()),
         Range.create(one.copy(), two.copy(), step=three.copy()),
         Range.create(dim3_bound_start, dim3_bound_stop, step=three.copy())])
    result = fortran_writer.arrayreference_node(array)
    assert result == "a(:,:2:3,::3)"

    # Make a) lbound and ubound come from a different array and b)
    # switch lbound and ubound round. These bounds should then be
    # output.
    array_type = ArrayType(REAL_TYPE, [10])
    symbol_b = DataSymbol("b", array_type)
    b_dim1_bound_start = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol_b), ("dim", one.copy())])
    b_dim1_bound_stop = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(symbol_b), ("dim", one.copy())])
    array = ArrayReference.create(
        symbol,
        [Range.create(b_dim1_bound_start, b_dim1_bound_stop),
         Range.create(one.copy(), two.copy(), step=three.copy()),
         Range.create(dim3_bound_stop.copy(), dim3_bound_start.copy(),
                      step=three.copy())])
    result = fortran_writer.arrayreference_node(array)
    assert result == ("a(LBOUND(b, dim=1):UBOUND(b, dim=1),:2:3,"
                      "UBOUND(a, dim=3):LBOUND(a, dim=3):3)")


def test_fw_range_structureref(fortran_writer):
    '''
    Check the FortranWriter for Range nodes within structure references.
    '''
    grid_type = DataTypeSymbol("grid_type", UnresolvedType())
    symbol = DataSymbol("my_grid", grid_type)
    grid_array_type = ArrayType(grid_type, [5, 5])
    array_symbol = DataSymbol("my_grids", grid_array_type)
    one = Literal("1", INTEGER_TYPE)
    two = Literal("2", INTEGER_TYPE)
    data_ref = StructureReference.create(symbol, ["data"])
    start = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [data_ref.copy(), ("dim", one.copy())])
    stop = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [data_ref.copy(), ("dim", one.copy())])
    ref = StructureReference.create(symbol, [("data",
                                              [Range.create(start, stop)])])
    result = fortran_writer(ref)
    assert result == "my_grid%data(:)"

    start = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(array_symbol), ("dim", one.copy())])
    stop = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(array_symbol), ("dim", one.copy())])
    range2 = Range.create(start, stop)
    result = fortran_writer(
        ArrayOfStructuresReference.create(array_symbol, [range2],
                                          [("data", [range2.copy()])]))
    assert (result ==
            "my_grids(:)%data(LBOUND(my_grids, dim=1):"
            "UBOUND(my_grids, dim=1))")

    symbol = DataSymbol("field", UnresolvedType())
    int_one = Literal("1", INTEGER_TYPE)
    lbound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [StructureReference.create(symbol, ["first"]),
         ("dim", int_one.copy())])
    ubound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [StructureReference.create(symbol, ["first"]),
         ("dim", int_one.copy())])
    my_range = Range.create(lbound, ubound)
    ref = ArrayOfStructuresReference.create(symbol, [my_range.copy()],
                                            ["first",
                                             ("second", [my_range.copy()])])
    result = fortran_writer(ref)
    assert (result ==
            "field(LBOUND(field%first, dim=1):"
            "UBOUND(field%first, dim=1))%first%second("
            "LBOUND(field%first, dim=1):UBOUND(field%first, dim=1))")

    data_ref = Reference(array_symbol)
    start = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [data_ref.copy(), ("dim", two.copy())])
    stop = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [data_ref.copy(), ("dim", two.copy())])
    aref = ArrayOfStructuresReference.create(
        array_symbol, [one.copy(), Range.create(start, stop)], ["flag"])
    result = fortran_writer(aref)
    assert result == "my_grids(1,:)%flag"


def test_fw_char_literal(fortran_writer):
    ''' Test the FortranWriter support for character literals. '''
    lit = Literal("hello", CHARACTER_TYPE)
    result = fortran_writer(lit)
    assert result == "'hello'"

# literal is already checked within previous tests


def test_fw_ifblock(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class ifblock method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, n)\n"
        "  integer, intent(inout) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    if (n.gt.2) then\n"
        "      n=n+1\n"
        "    end if\n"
        "    if (n.gt.4) then\n"
        "      a = -1\n"
        "    else\n"
        "      a = 1\n"
        "    end if\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    assert (
        "    if (n > 2) then\n"
        "      n = n + 1\n"
        "    end if\n"
        "    if (n > 4) then\n"
        "      a = -1\n"
        "    else\n"
        "      a = 1\n"
        "    end if\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_loop(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class loop method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: i, sum\n"
        "  sum = 0\n"
        "  do i = 1, 20, 2\n"
        "    sum = sum + i\n"
        "  end do\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    assert "do i = 1, 20, 2\n" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_unaryoperator(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class unary_operation method
    correctly prints out the Fortran representation. Uses -1 as the
    example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = -1\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    assert "a = -1" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_unaryoperator2(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class unary_operation method correctly
    prints out the Fortran representation of an intrinsic. Uses sin as
    the example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    assert "a = SIN(1.0)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_unaryoperator_unknown(fortran_reader, fortran_writer, monkeypatch):
    '''Check the FortranWriter class unary_operation method raises an
    exception if an unknown unary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = -1.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)
    # Remove MINUS from the dict of unary operators
    monkeypatch.delitem(fortran_writer._operator_2_str,
                        UnaryOperation.Operator.MINUS)
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer(schedule)
    assert "Unexpected unary op" in str(excinfo.value)


def test_fw_return(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class return method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  return\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    assert "  return\n" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_codeblock_1(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class codeblock method correctly
    prints out the Fortran code contained within it.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: a\n"
        "  a=1\n"
        "  print *,\"I am a code block\"\n"
        "  print *,\"with more than one line\"\n"
        "end subroutine tmp\n"
        "end module test")
    psyir = fortran_reader.psyir_from_source(code)
    # Check a code block exists in the PSyIR
    assert psyir.walk(CodeBlock)
    # Generate Fortran from the PSyIR
    result = fortran_writer(psyir)
    assert (
        "    a = 1\n"
        "    PRINT *, \"I am a code block\"\n"
        "    PRINT *, \"with more than one line\"\n" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_fw_codeblock_2(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class codeblock method correctly prints out
    the Fortran representation when there is a code block that is part
    of a line (not a whole line). In this case the data initialisation
    of the array 'a' "(/ 0.0 /)" is a code block.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  real a(1)\n"
        "  a = (/ 0.0 /)\n"
        "end subroutine tmp\n"
        "end module test")
    psyir = fortran_reader.psyir_from_source(code)

    # Check a code block exists in the PSyIR
    assert psyir.walk(CodeBlock)

    # Generate Fortran from the PSyIR
    result = fortran_writer(psyir)
    assert "a = (/0.0/)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_codeblock_3(fortran_writer):
    '''Check the FortranWriter class codeblock method raises the expected
    exception if an unsupported CodeBlock structure value is found.

    '''
    code_block = CodeBlock([], "unsupported")
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.codeblock_node(code_block)
    assert ("Unsupported CodeBlock Structure 'unsupported' found."
            in str(excinfo.value))


def get_nemo_schedule(parser, code):
    '''Utility function that returns the first schedule for a code with
    the nemo api.

    :param parser: the parser class.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`
    :param str code: the code as a string.

    :returns: the first schedule in the supplied code.
    :rtype: :py:class:`psyclone.nemo.NemoInvokeSchedule`

    '''
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    return psy.invokes.invoke_list[0].schedule


def test_fw_nemoinvokeschedule(fortran_writer, parser):
    '''Check that the FortranWriter class nemoinvokeschedule accepts the
    NemoInvokeSchedule node and prints the expected code (from any
    children of the node as the node itself simply calls its
    children).

    '''
    code = (
        "program test\n"
        "  integer :: a\n"
        "  a=1\n"
        "end program test\n")
    schedule = get_nemo_schedule(parser, code)
    assert isinstance(schedule, NemoInvokeSchedule)
    result = fortran_writer(schedule)
    assert "a = 1\n" in result


def test_fw_query_intrinsics(fortran_reader, fortran_writer, tmpdir):
    ''' Check that the FortranWriter outputs SIZE/LBOUND/UBOUND
    intrinsic calls. '''
    code = ("module test_mod\n"
            "contains\n"
            "subroutine test_kern(a)\n"
            "  real, intent(in) :: a(:,:)\n"
            "  integer :: mysize, lb, ub\n"
            "  mysize = size(a, 2)\n"
            "  lb = lbound(a, 2)\n"
            "  ub = ubound(a, 2)\n"
            "end subroutine test_kern\n"
            "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR
    result = fortran_writer(psyir).lower()
    assert "mysize = size(a, 2)" in result
    assert "lb = lbound(a, 2)" in result
    assert "ub = ubound(a, 2)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_literal_node(fortran_writer):
    ''' Test the PSyIR literals are converted to the proper Fortran format
    when necessary. '''

    # pylint: disable=too-many-statements
    # By default literals are not modified
    lit1 = Literal('a', CHARACTER_TYPE)
    result = fortran_writer(lit1)
    assert result == "'a'"
    # An empty character string is valid
    lit1 = Literal('', CHARACTER_TYPE)
    result = fortran_writer(lit1)
    assert result == "''"

    # Check that we generate the correct quotation marks if the literal itself
    # includes quotation marks within it. (Note that the values of character
    # literals are stored in the PSyIR without any enclosing quotation marks.)
    lit1 = Literal('''('hello ',4A)''', CHARACTER_TYPE)
    result = fortran_writer(lit1)
    assert result == '''"('hello ',4A)"'''
    lit1 = Literal('"a"', CHARACTER_TYPE)
    result = fortran_writer(lit1)
    assert result == """'"a"'"""
    lit1 = Literal("apostrophe's", CHARACTER_TYPE)
    result = fortran_writer(lit1)
    assert result == '''"apostrophe's"'''
    # Literals containing both single and double quotes are not supported.
    lit1 = Literal('''('hello "',4A,'"')''', CHARACTER_TYPE)
    with pytest.raises(NotImplementedError) as err:
        _ = fortran_writer(lit1)
    assert '''supported but found >>('hello "',4A,'"')<<''' in str(err.value)
    # Literals containing both single and double quotes are not supported.
    lit1 = Literal('''("hello '",4A,"'")''', CHARACTER_TYPE)
    with pytest.raises(NotImplementedError) as err:
        _ = fortran_writer(lit1)
    assert '''supported but found >>("hello '",4A,"'")<<''' in str(err.value)

    lit1 = Literal('3.14', REAL_TYPE)
    result = fortran_writer(lit1)
    assert result == '3.14'

    lit1 = Literal('3.14E0', REAL_TYPE)
    result = fortran_writer(lit1)
    assert result == '3.14e0'

    lit1 = Literal('3.14E0', REAL_DOUBLE_TYPE)
    result = fortran_writer(lit1)
    assert result == '3.14d0'

    # Check that BOOLEANS use the FORTRAN formatting
    lit1 = Literal('true', BOOLEAN_TYPE)
    result = fortran_writer(lit1)
    assert result == '.true.'
    lit1 = Literal('false', BOOLEAN_TYPE)
    result = fortran_writer(lit1)
    assert result == '.false.'

    # Check precision symbols are output as expected
    precision_symbol = DataSymbol("rdef", INTEGER_TYPE)
    my_type = ScalarType(ScalarType.Intrinsic.REAL, precision_symbol)
    lit1 = Literal("3.14", my_type)
    result = fortran_writer(lit1)
    assert result == "3.14_rdef"

    # Check character precision symbols are output as expected
    my_type = ScalarType(ScalarType.Intrinsic.CHARACTER, precision_symbol)
    lit1 = Literal("hello", my_type)
    result = fortran_writer(lit1)
    assert result == "rdef_'hello'"

    # Check explicit precision is output as expected
    my_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    lit1 = Literal("3.14", my_type)
    result = fortran_writer(lit1)
    assert result == "3.14_4"

    # Check explicit character precision is output as expected
    my_type = ScalarType(ScalarType.Intrinsic.CHARACTER, 1)
    lit1 = Literal("hello", my_type)
    result = fortran_writer(lit1)
    assert result == "1_'hello'"


def test_fw_call_node(fortran_writer):
    '''Test the PSyIR call node is translated to the required Fortran code. '''
    # No call arguments nor node parent
    routine_symbol = RoutineSymbol("mysub")
    call = Call.create(routine_symbol, [])
    result = fortran_writer(call)
    assert result == "call mysub()\n"

    # If it's inside a Schedule it still show the call keyword and a line break
    schedule = Schedule()
    schedule.addchild(call)
    assert fortran_writer(call) == "call mysub()\n"

    # Inside an expression it will not show the call keyword and line break
    ifblock = IfBlock.create(call.detach(), [Return()])
    assert fortran_writer(call) == "mysub()"
    assert fortran_writer(ifblock) == "if (mysub()) then\n  return\nend if\n"

    # Call with arguments
    args = [Reference(DataSymbol("arg1", REAL_TYPE)),
            Reference(DataSymbol("arg2", REAL_TYPE))]
    call = Call.create(routine_symbol, args)
    result = fortran_writer(call)
    assert result == "call mysub(arg1, arg2)\n"

    symbol_table = SymbolTable()
    symbol_a = DataSymbol("a", REAL_TYPE)
    symbol_table.add(symbol_a)
    ref_a = Reference(symbol_a)
    symbol_b = DataSymbol("b", REAL_TYPE)
    symbol_table.add(symbol_b)
    ref_b = Reference(symbol_b)
    symbol_use = ContainerSymbol("my_mod")
    symbol_table.add(symbol_use)
    symbol_call = RoutineSymbol(
        "my_sub", interface=ImportInterface(symbol_use))
    symbol_table.add(symbol_call)
    mult_ab = BinaryOperation.create(
        BinaryOperation.Operator.MUL, ref_a.copy(), ref_b.copy())
    max_ab = IntrinsicCall.create(IntrinsicCall.Intrinsic.MAX, [ref_a, ref_b])
    call = Call.create(symbol_call, [mult_ab, max_ab])
    schedule = KernelSchedule.create("work", symbol_table, [call])
    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    expected = "  call my_sub(a * b, MAX(a, b))\n"
    assert expected in result


def test_fw_call_node_namedargs(fortran_writer):
    '''Test the PSyIR call node is translated to the required Fortran code
    when there are named arguments and that the expected exception is
    raised if all of the named arguments are not after the positional
    arguments.

    '''
    routine_symbol = RoutineSymbol("mysub")
    call = Call.create(
        routine_symbol,
        [Literal("1.0", REAL_TYPE),
         ("arg2", Literal("2.0", REAL_TYPE)),
         ("arg3", Literal("3.0", REAL_TYPE))])
    result = fortran_writer(call)
    assert result == "call mysub(1.0, arg2=2.0, arg3=3.0)\n"

    call.children[2] = Literal("4.0", REAL_TYPE)

    with pytest.raises(VisitorError) as info:
        _ = fortran_writer(call)
    assert ("Fortran expects all named arguments to occur after all "
            "positional arguments but this is not the case for "
            "Call[name='mysub']" in str(info.value))


def test_fw_call_node_cblock_args(fortran_reader, fortran_writer):
    '''Test that a PSyIR call node with arguments represented by CodeBlocks
    is translated to the required Fortran code.

    '''
    # It's not easy to construct CodeBlocks from scratch as we need bits of
    # an fparser2 parse tree. Therefore just use the frontend.
    psyir = fortran_reader.psyir_from_source(
        "subroutine test()\n"
        "  use my_mod, only : kernel\n"
        "  real :: a, b\n"
        "  call kernel(a, 'not'//'nice', b, name='roo')\n"
        "end subroutine")
    call_node = psyir.walk(Call)[0]
    cblocks = psyir.walk(CodeBlock)
    assert len(cblocks) == 1
    gen = fortran_writer(call_node)
    assert gen == '''call kernel(a, 'not' // 'nice', b, name='roo')\n'''


def test_fw_intrinsic_call_node(fortran_writer):
    '''Test that the backend handles IntrinsicCall nodes correctly, including
    skipping 'call' for allocate/deallocate.'''
    sym = DataSymbol("var", INTEGER_TYPE)
    jsym = DataSymbol("jelly", INTEGER_TYPE)
    call = IntrinsicCall.create(IntrinsicCall.Intrinsic.DEALLOCATE,
                                [Reference(sym)])
    gen = fortran_writer(call)
    assert gen == "DEALLOCATE(var)\n"
    acall = IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                                 [Reference(sym), ("mold", Reference(jsym))])
    gen = fortran_writer(acall)
    assert gen == "ALLOCATE(var, mold=jelly)\n"
    rcall = IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM_NUMBER,
                                 [Reference(sym)])
    gen = fortran_writer(rcall)
    assert gen == "call RANDOM_NUMBER(var)\n"

    for intrinsic_function in [IntrinsicCall.Intrinsic.MINVAL,
                               IntrinsicCall.Intrinsic.MAXVAL,
                               IntrinsicCall.Intrinsic.SUM,
                               IntrinsicCall.Intrinsic.TINY,
                               IntrinsicCall.Intrinsic.HUGE]:
        intrinsic_call = IntrinsicCall.create(
            intrinsic_function, [Reference(sym)])
        assignment = Assignment.create(Reference(sym), intrinsic_call)
        gen = fortran_writer(assignment)
        assert gen == f"var = {intrinsic_function.name}(var)\n"


def test_fw_comments(fortran_writer):
    ''' Test the generation of Fortran from PSyIR with comments. '''

    container = Container("my_container")
    routine = Routine("my_routine")
    container.addchild(routine)
    statement1 = Return()
    statement2 = Return()
    statement3 = Return()
    routine.children = [statement1, statement2, statement3]

    # If the comments are empty, they don't appear at all
    expected = (
        "module my_container\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine my_routine()\n\n"
        "    return\n"
        "    return\n"
        "    return\n\n"
        "  end subroutine my_routine\n\n"
        "end module my_container\n")
    assert expected == fortran_writer(container)

    # Add comments
    container.preceding_comment = "My container preceding comment"
    container.inline_comment = "My container inline comment"
    routine.preceding_comment = "My routine preceding comment"
    routine.inline_comment = "My routine inline comment"
    statement1.preceding_comment = "My statement with a preceding comment"
    statement2.preceding_comment = "My statement with a preceding comment ..."
    statement2.inline_comment = "... and an inline comment"
    statement3.inline_comment = "Statement with only an inline comment"

    # Now they are placed in the appropriate position
    expected = (
        "! My container preceding comment\n"
        "module my_container\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n"
        "  ! My routine preceding comment\n"
        "  subroutine my_routine()\n\n"
        "    ! My statement with a preceding comment\n"
        "    return\n"
        "    ! My statement with a preceding comment ...\n"
        "    return  ! ... and an inline comment\n"
        "    return  ! Statement with only an inline comment\n\n"
        "  end subroutine my_routine  ! My routine inline comment\n\n"
        "end module my_container  ! My container inline comment\n")
    assert expected == fortran_writer(container)


def test_fw_directive_with_clause(fortran_reader, fortran_writer):
    '''Test that a PSyIR directive with clauses is translated to
    the required Fortran code.

    '''
    # Generate PSyIR from Fortran code.
    code = (
        "program test\n"
        "  integer, parameter :: n=20\n"
        "  integer :: i\n"
        "  real :: a(n)\n"
        "  do i=1,n\n"
        "    a(i) = 0.0\n"
        "  end do\n"
        "end program test")
    container = fortran_reader.psyir_from_source(code)
    schedule = container.children[0]
    loops = schedule.walk(Loop)
    loop = loops[0].detach()
    directive = OMPTaskloopDirective(children=[loop], num_tasks=32,
                                     nogroup=True)
    master = OMPMasterDirective(children=[directive])
    parallel = OMPParallelDirective.create(children=[master])
    schedule.addchild(parallel, 0)
    assert '''!$omp parallel default(shared), private(i)
  !$omp master
  !$omp taskloop num_tasks(32), nogroup
  do i = 1, n, 1
    a(i) = 0.0
  enddo
  !$omp end taskloop
  !$omp end master
  !$omp end parallel''' in fortran_writer(container)


def test_fw_clause(fortran_writer):
    '''Test that a PSyIR clause is translated to the correct Fortran code.'''
    clause = OMPNumTasksClause(children=[Literal("32", INTEGER_TYPE)])
    assert "num_tasks(32)" in fortran_writer(clause)


def test_fw_operand_clause(fortran_writer):
    '''Test that a PSyIR operand clause is translated to the correct Fortran
    code.'''
    op_clause = OMPDependClause()
    symbol_table = SymbolTable()
    symbol_a = DataSymbol("a", REAL_TYPE)
    symbol_table.add(symbol_a)
    ref_a = Reference(symbol_a)
    op_clause.addchild(ref_a)
    assert "depend(inout: a)" in fortran_writer(op_clause)


def test_fw_keeps_symbol_renaming(fortran_writer, fortran_reader):
    '''Test that the FortranWriter correctly keeps => in Use statements to
    ensure variable renaming is handle correctly.'''
    code = '''
    module a
    integer, parameter :: a_mod_name = 2
    end module a
    module b
    use a, only: b_mod_name => a_mod_name
    contains
    subroutine X()
       print *, b_mod_name
    end subroutine X
    end module b
    '''
    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)
    assert "b_mod_name=>a_mod_name" in output


def test_componenttype_initialisation(fortran_reader, fortran_writer):
    '''Test that initial values are output for a StructureType which
    contains types that have initial values.

    '''
    test_code = (
        "module test_mod\n"
        "    type :: my_type\n"
        "      integer :: i = 1\n"
        "      integer :: j\n"
        "    end type my_type\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].symbol_table
    symbol = sym_table.lookup("my_type")
    assert isinstance(symbol.datatype, StructureType)
    result = fortran_writer(psyir)
    assert (
        "  type, public :: my_type\n"
        "    integer, public :: i = 1\n"
        "    integer, public :: j\n"
        "  end type my_type\n" in result)
