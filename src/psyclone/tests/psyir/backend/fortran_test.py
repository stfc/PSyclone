# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Modified by A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.fortran module'''

from __future__ import absolute_import

import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.backend.fortran import gen_intent, gen_dims, \
    FortranWriter, gen_datatype
from psyclone.psyir.nodes import Node, CodeBlock, Container, Literal
from psyclone.psyir.symbols import DataSymbol, SymbolTable, ContainerSymbol, \
    GlobalInterface, ArgumentInterface, UnresolvedInterface, DataType
from psyclone.tests.utilities import create_schedule
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.tests.utilities import Compile

@pytest.fixture(scope="function", name="fort_writer")
def fixture_fort_writer():
    '''Create and return a FortranWriter object with default settings.'''
    return FortranWriter()


def test_gen_intent():
    '''Check the gen_intent function produces the expected intent
    strings.

    '''
    symbol = DataSymbol("dummy", DataType.INTEGER,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    assert gen_intent(symbol) is None
    symbol = DataSymbol("dummy", DataType.INTEGER,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READ))
    assert gen_intent(symbol) == "in"
    symbol = DataSymbol("dummy", DataType.INTEGER,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.WRITE))
    assert gen_intent(symbol) == "out"
    symbol = DataSymbol("dummy", DataType.INTEGER,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
    assert gen_intent(symbol) == "inout"


def test_gen_intent_error(monkeypatch):
    '''Check the gen_intent function raises an exception if an unsupported
    access type is found.

    '''
    symbol = DataSymbol("dummy", DataType.INTEGER,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    monkeypatch.setattr(symbol.interface, "_access", "UNSUPPORTED")
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_intent(symbol)
    assert "Unsupported access ''UNSUPPORTED'' found." in str(excinfo.value)


def test_gen_dims():
    '''Check the gen_dims function produces the expected dimension
    strings.

    '''
    arg = DataSymbol("arg", DataType.INTEGER,
                     interface=ArgumentInterface(
                         ArgumentInterface.Access.UNKNOWN))
    symbol = DataSymbol("dummy", DataType.INTEGER,
                        shape=[arg, 2, DataSymbol.Extent.ATTRIBUTE],
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    assert gen_dims(symbol) == ["arg", "2", ":"]


def test_gen_dims_error(monkeypatch):
    '''Check the gen_dims function raises an exception if a symbol shape
    entry is not supported.

    '''
    symbol = DataSymbol("dummy", DataType.INTEGER,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    monkeypatch.setattr(symbol, "_shape", ["invalid"])
    with pytest.raises(NotImplementedError) as excinfo:
        _ = gen_dims(symbol)
    assert "unsupported gen_dims index 'invalid'" in str(excinfo.value)


@pytest.mark.parametrize(
    "datatype,result",
    [(DataType.REAL, "real"),
     (DataType.INTEGER, "integer"),
     (DataType.CHARACTER, "character"),
     (DataType.BOOLEAN, "logical")])
def test_gen_datatype(datatype, result):
    '''Check the gen_datatype function produces the expected datatypes.'''
    symbol = DataSymbol("dummy", datatype)
    assert gen_datatype(symbol) == result


@pytest.mark.parametrize(
    "datatype,precision,result",
    [(DataType.REAL, None, "real"),
     (DataType.INTEGER, 8, "integer*8"),
     (DataType.REAL, 16, "real*16"),
     (DataType.REAL, DataSymbol.Precision.DOUBLE, "double precision"),
     (DataType.INTEGER, DataSymbol("i_def", DataType.INTEGER),
      "integer(kind=i_def)"),
     (DataType.REAL, DataSymbol("r_def", DataType.INTEGER),
      "real(kind=r_def)")])
def test_gen_datatype_precision(datatype, precision, result):
    '''Check the gen_datatype function produces the expected datatypes when
    precision is specified.

    '''
    symbol = DataSymbol("dummy", datatype, precision=precision)
    assert gen_datatype(symbol) == result


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
#         symbol = Symbol("dummy", DataType.INTEGER,
#                         precision=Symbol.Precision.DOUBLE)
#         _ = gen_datatype(symbol)
#         assert (
#             "WARNING  Fortran does not support relative precision for the "
#             "'integer' datatype but 'Precision.DOUBLE' was specified for "
#             "variable 'dummy'." in caplog.text)


def test_gen_datatype_error(monkeypatch):
    '''Check the gen_datatype function raises an exception if the datatype
    information provided is not supported.

    '''
    # unsupported datatype found
    symbol = DataSymbol("dummy", DataType.DEFERRED)
    with pytest.raises(NotImplementedError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("unsupported datatype 'DataType.DEFERRED' for symbol 'dummy' "
            "found in gen_datatype()." in str(excinfo.value))

    # Fixed precision not supported for character
    symbol = DataSymbol("dummy", DataType.INTEGER, precision=4)
    monkeypatch.setattr(symbol, "_datatype", DataType.CHARACTER)
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("Explicit precision not supported for datatype 'character' in "
            "symbol 'dummy' in Fortran backend." in str(excinfo.value))

    # Fixed precision value not supported for real
    symbol = DataSymbol("dummy", DataType.REAL, precision=2)
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("Datatype 'real' in symbol 'dummy' supports fixed precision of "
            "[4, 8, 16] but found '2'." in str(excinfo.value))

    # Fixed precision value not supported for integer
    symbol = DataSymbol("dummy", DataType.INTEGER, precision=32)
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("Datatype 'integer' in symbol 'dummy' supports fixed precision "
            "of [1, 2, 4, 8, 16] but found '32'." in str(excinfo.value))

    # Fixed precision value not supported for logical
    symbol = DataSymbol("dummy", DataType.BOOLEAN)
    # This needs to be monkeypatched as the Fortran front end will not
    # create logicals with a precision
    monkeypatch.setattr(symbol, "precision", 32)
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("Datatype 'logical' in symbol 'dummy' supports fixed precision "
            "of [1, 2, 4, 8, 16] but found '32'." in str(excinfo.value))

    # Kind not supported for character
    symbol = DataSymbol("dummy", DataType.REAL,
                        precision=DataSymbol("c_def", DataType.INTEGER))
    # This needs to be monkeypatched as the Symbol constructor can not
    # create characters with a size dependent on another variable.
    monkeypatch.setattr(symbol, "_datatype", DataType.CHARACTER)
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("kind not supported for datatype 'character' in symbol 'dummy' in "
            "Fortran backend." in str(excinfo.value))

    # Unsupported precision type found
    symbol = DataSymbol("dummy", DataType.REAL)
    monkeypatch.setattr(symbol, "precision", "unsupported")
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_datatype(symbol)
    assert ("Unsupported precision type 'str' found for symbol 'dummy' in "
            "Fortran backend." in str(excinfo.value))


def test_fw_gen_use(fort_writer):
    '''Check the FortranWriter class gen_use method produces the expected
    declaration. Also check that an exception is raised if the symbol
    does not describe a use statement.

    '''
    symbol = DataSymbol("dummy1", DataType.DEFERRED,
                        interface=GlobalInterface(
                            ContainerSymbol("my_module")))
    result = fort_writer.gen_use(symbol)
    assert result == "use my_module, only : dummy1\n"

    symbol = DataSymbol("dummy1", DataType.INTEGER)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_use(symbol)
    assert ("gen_use() requires the symbol interface for symbol 'dummy1' to "
            "be a Global instance but found 'LocalInterface'."
            in str(excinfo.value))


def test_fw_gen_vardecl(fort_writer):
    '''Check the FortranWriter class gen_vardecl method produces the
    expected declarations. Also check that an exception is raised if
    the symbol does not describe a valid variable declaration statement.

    '''
    # Basic entry
    symbol = DataSymbol("dummy1", DataType.INTEGER)
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer :: dummy1\n"

    # Assumed-size array with intent
    symbol = DataSymbol("dummy2", DataType.INTEGER,
                        shape=[2, 2, DataSymbol.Extent.ATTRIBUTE],
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READ))
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(2,2,:), intent(in) :: dummy2\n"

    # Assumed-size array with unknown intent
    symbol = DataSymbol("dummy2", DataType.INTEGER,
                        shape=[2, 2, DataSymbol.Extent.ATTRIBUTE],
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(2,2,:) :: dummy2\n"

    # Allocatable array
    symbol = DataSymbol("dummy2", DataType.REAL,
                        shape=[DataSymbol.Extent.DEFERRED,
                               DataSymbol.Extent.DEFERRED],
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
    result = fort_writer.gen_vardecl(symbol)
    assert result == \
        "real, allocatable, dimension(:,:), intent(inout) :: dummy2\n"

    # Constant
    symbol = DataSymbol("dummy3", DataType.INTEGER, constant_value=10)
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, parameter :: dummy3 = 10\n"

    # Use statement
    symbol = DataSymbol("dummy1", DataType.DEFERRED,
                        interface=GlobalInterface(
                            ContainerSymbol("my_module")))
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("gen_vardecl requires the symbol 'dummy1' to have a Local or "
            "an Argument interface but found a 'GlobalInterface' interface."
            in str(excinfo.value))

    # An unresolved symbol
    symbol = DataSymbol("dummy1", DataType.DEFERRED,
                        interface=UnresolvedInterface())
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("gen_vardecl requires the symbol 'dummy1' to have a Local or "
            "an Argument interface but found a 'UnresolvedInterface' "
            "interface." in str(excinfo.value))

    # An array with a mixture of deferred and explicit extents
    symbol = DataSymbol("dummy1", DataType.INTEGER,
                        shape=[2, DataSymbol.Extent.DEFERRED])
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("Fortran declaration of an allocatable array must have the "
            "extent of every dimension as 'DEFERRED' but symbol 'dummy1' "
            "has shape: [2, " in str(excinfo.value))

    # An assumed-size array must have only the extent of its outermost
    # rank undefined
    symbol = DataSymbol("dummy1", DataType.INTEGER,
                        shape=[2, DataSymbol.Extent.ATTRIBUTE, 2])
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("assumed-size Fortran array must only have its last dimension "
            "unspecified (as 'ATTRIBUTE') but symbol 'dummy1' has shape: [2, "
            in str(excinfo.value))
    # With two dimensions unspecified, even though one is outermost
    symbol = DataSymbol(
        "dummy1", DataType.INTEGER,
        shape=[2, DataSymbol.Extent.ATTRIBUTE, DataSymbol.Extent.ATTRIBUTE])
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("assumed-size Fortran array must only have its last dimension "
            "unspecified (as 'ATTRIBUTE') but symbol 'dummy1' has shape: [2, "
            in str(excinfo.value))


def test_gen_decls(fort_writer):
    '''Check the FortranWriter class gen_decls method produces the
    expected declarations. Also check that an exception is raised if
    an 'argument' symbol exists in the supplied symbol table and the
    optional argument 'args_allowed' is set to False.

    '''
    symbol_table = SymbolTable()
    symbol_table.add(ContainerSymbol("my_module"))
    use_statement = DataSymbol("my_use", DataType.DEFERRED,
                               interface=GlobalInterface(
                                   symbol_table.lookup("my_module")))
    symbol_table.add(use_statement)
    argument_variable = DataSymbol("arg", DataType.INTEGER,
                                   interface=ArgumentInterface())
    symbol_table.add(argument_variable)
    local_variable = DataSymbol("local", DataType.INTEGER)
    symbol_table.add(local_variable)
    result = fort_writer.gen_decls(symbol_table)
    assert (result ==
            "use my_module, only : my_use\n"
            "integer :: arg\n"
            "integer :: local\n")
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_decls(symbol_table, args_allowed=False)
    assert ("Arguments are not allowed in this context but this symbol table "
            "contains argument(s): '['arg']'." in str(excinfo.value))

    # Add a symbol with a deferred (unknown) interface
    symbol_table.add(DataSymbol("unknown", DataType.INTEGER,
                                interface=UnresolvedInterface()))
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_decls(symbol_table)
    assert ("The following symbols are not explicitly declared or imported "
            "from a module (in the local scope) and are not KIND parameters: "
            "'unknown'" in str(excinfo.value))


def test_fw_exception(fort_writer):
    '''Check the FortranWriter class instance raises an exception if an
    unsupported PSyIR node is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: a,b,c\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # pylint: disable=abstract-method
    # modify the reference to b to be something unsupported
    class Unsupported(Node):
        '''A PSyIR node that will not be supported by the Fortran visitor.'''
    # pylint: enable=abstract-method

    unsupported = Unsupported()
    assignment = schedule[0]
    binary_operation = assignment.rhs
    # The assignment.rhs method has no setter so access the reference
    # directly instead via children.
    assignment.children[1] = unsupported
    unsupported.children = binary_operation.children

    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Unsupported node 'Unsupported' found" in str(excinfo.value)


def test_fw_container_1(fort_writer, monkeypatch):
    '''Check the FortranWriter class outputs correct code when a Container
    node with no content is found. Also tests that an exception is
    raised if Container.name does not have a value.

    '''
    container = Container("test")
    result = fort_writer(container)
    assert (
        "module test\n\n"
        "  contains\n\n"
        "end module test\n" in result)

    monkeypatch.setattr(container, "_name", None)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert ("Expected Container node name to have a value."
            in str(excinfo.value))


def test_fw_container_2(fort_writer):
    '''Check the FortranWriter class outputs correct code when a Container
    node is found with a subroutine, use statements and
    declarations. Also raise an exception if the Container contains a
    Container.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "use test2_mod, only : a,b\n"
        "real :: c,d\n"
        "contains\n"
        "subroutine tmp()\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    container = schedule.root

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(container)

    assert (
        "module test\n"
        "  use test2_mod, only : a\n"
        "  use test2_mod, only : b\n"
        "  real :: c\n"
        "  real :: d\n\n"
        "  contains\n"
        "  subroutine tmp()\n\n\n"
        "  end subroutine tmp\n\n"
        "end module test\n" in result)

    container.children.append(Container("child", parent=container))
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert ("The Fortran back-end requires all children of a Container "
            "to be KernelSchedules." in str(excinfo.value))


def test_fw_container_3(fort_writer, monkeypatch):
    '''Check the FortranWriter class raises an exception when a Container
    node contains a symbol table with an argument declaration (as this
    does not make sense).

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "real :: a\n"
        "contains\n"
        "subroutine tmp()\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    container = schedule.root
    symbol = container.symbol_table.symbols[0]
    assert symbol.name == "a"
    monkeypatch.setattr(symbol, "_interface", ArgumentInterface())

    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert ("Arguments are not allowed in this context but this symbol table "
            "contains argument(s): '['a']'." in str(excinfo.value))


def test_fw_kernelschedule(fort_writer, monkeypatch):
    '''Check the FortranWriter class outputs correct code when a
    KernelSchedule node is found. Also tests that an exception is
    raised if KernelSchedule.name does not have a value.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,b,c)\n"
        "  use my_mod, only : d\n"
        "  real, intent(out) :: a(:)\n"
        "  real, intent(in) :: b(:)\n"
        "  integer, intent(in) :: c\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)

    assert(
        "subroutine tmp(a,b,c)\n"
        "  use my_mod, only : d\n"
        "  real, dimension(:), intent(out) :: a\n"
        "  real, dimension(:), intent(in) :: b\n"
        "  integer, intent(in) :: c\n"
        "\n"
        "  a=b / c\n"
        "\n"
        "end subroutine tmp\n") in result

    monkeypatch.setattr(schedule, "_name", None)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Expected node name to have a value." in str(excinfo.value)

# assignment and binaryoperation (not intrinsics) are already checked
# within previous tests


@pytest.mark.parametrize("binary_intrinsic", ["mod", "max", "min",
                                              "sign"])
def test_fw_binaryoperator(fort_writer, binary_intrinsic, tmpdir):
    '''Check the FortranWriter class binary_operation method correctly
    prints out the Fortran representation of an intrinsic. Tests all
    of the binary operators, apart from sum (as it requires different
    data types so is tested separately) and matmul ( as it requires
    its arguments to be arrays).

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = {0}(1.0,1.0)\n"
        "end subroutine tmp\n"
        "end module test").format(binary_intrinsic)
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a={0}(1.0, 1.0)".format(binary_intrinsic.upper()) in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_binaryoperator_sum(fort_writer, tmpdir):
    '''Check the FortranWriter class binary_operation method with the sum
    operator correctly prints out the Fortran representation of an
    intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(array,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: array(n)\n"
        "  integer :: a\n"
        "    a = sum(array,dim=1)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=SUM(array, dim = 1)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_binaryoperator_matmul(fort_writer, tmpdir):
    '''Check the FortranWriter class binary_operation method with the matmul
    operator correctly prints out the Fortran representation of an
    intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,b,c,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(in) :: a(n,n), b(n)\n"
        "  real, intent(out) :: c(n)\n"
        "    c = MATMUL(a,b)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "c=MATMUL(a, b)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_binaryoperator_unknown(fort_writer, monkeypatch):
    '''Check the FortranWriter class binary_operation method raises an
    exception if an unknown binary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sign(1.0,1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove sign() from the list of supported binary operators
    monkeypatch.delitem(Fparser2Reader.binary_operators, "sign")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Unexpected binary op" in str(excinfo.value)


def test_fw_naryopeator(fort_writer, tmpdir):
    ''' Check that the FortranWriter class nary_operation method correctly
    prints out the Fortran representation of an intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=MAX(1.0, 1.0, 2.0)" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_naryopeator_unknown(fort_writer, monkeypatch):
    ''' Check that the FortranWriter class nary_operation method raises
    the expected error if it encounters an unknown operator.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove max() from the list of supported nary operators
    monkeypatch.delitem(Fparser2Reader.nary_operators, "max")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as err:
        _ = fort_writer(schedule)
    assert "Unexpected N-ary op" in str(err.value)


def test_fw_reference(fort_writer):
    '''Check the FortranWriter class reference method prints the
    appropriate information (the name of the reference it points to).
    Also check the method raises an exception if it has children as
    this is not expected.

    '''
    # Generate fparser2 parse tree from Fortran code. The line of
    # interest is a(n) = 0.0. The additional a=1 line is added to get
    # round a bug in the parser.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = 1\n"
        "    a(n) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)

    # The asserts need to be split as the declaration order can change
    # between different versions of Python.
    assert (
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, dimension(n), intent(out) :: a\n"
        "\n"
        "  a=1\n"
        "  a(n)=0.0\n"
        "\n"
        "end subroutine tmp\n") in result

    # Now add a child to the reference node
    reference = schedule[1].lhs.children[0]
    reference.children = ["hello"]

    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        result = fort_writer(schedule)
    assert ("Expecting a Reference with no children but found"
            in str(excinfo.value))


def test_fw_array(fort_writer):
    '''Check the FortranWriter class array method correctly prints
    out the Fortran representation of an array

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a(2,n,3) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a(2,n,3)=0.0" in result

# literal is already checked within previous tests


def test_fw_ifblock(fort_writer):
    '''Check the FortranWriter class ifblock method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
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
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert (
        "  if (n > 2) then\n"
        "    n=n + 1\n"
        "  end if\n"
        "  if (n > 4) then\n"
        "    a=-1\n"
        "  else\n"
        "    a=1\n"
        "  end if\n") in result


def test_fw_loop(fort_writer):
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
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "do i = 1, 20, 2\n" in result


def test_fw_unaryoperator(fort_writer):
    '''Check the FortranWriter class unary_operation method
    correctly prints out the Fortran representation. Uses -1 as the
    example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = -1\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=-1" in result


def test_fw_unaryoperator2(fort_writer):
    '''Check the FortranWriter class unary_operation method correctly
    prints out the Fortran representation of an intrinsic. Uses sin as
    the example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=SIN(1.0)" in result


def test_fw_unaryoperator_unknown(fort_writer, monkeypatch):
    '''Check the FortranWriter class unary_operation method raises an
    exception if an unknown unary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove sin() from the dict of unary operators
    monkeypatch.delitem(Fparser2Reader.unary_operators, "sin")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Unexpected unary op" in str(excinfo.value)


def test_fw_return(fort_writer):
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
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "  return\n" in result


def test_fw_codeblock_1(fort_writer):
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
    schedule = create_schedule(code, "tmp")
    # Check a code block exists in the schedule
    assert schedule.walk(CodeBlock)
    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert (
        "  a=1\n"
        "  PRINT *, \"I am a code block\"\n"
        "  PRINT *, \"with more than one line\"\n" in result)


def test_fw_codeblock_2(fort_writer):
    '''Check the FortranWriter class codeblock method correctly prints out
    the Fortran representation when there is a code block that is part
    of a line (not a whole line). In this case the ":" in the array
    access is a code block.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a(2,n,:) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Check a code block exists in the schedule
    assert schedule.walk(CodeBlock)

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a(2,n,:)=0.0" in result


def test_fw_codeblock_3(fort_writer):
    '''Check the FortranWriter class codeblock method raises the expected
    exception if an unsupported CodeBlock structure value is found.

    '''
    code_block = CodeBlock([], "unsupported")
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.codeblock_node(code_block)
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
    from psyclone.psyGen import PSyFactory
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    return psy.invokes.invoke_list[0].schedule


def test_fw_nemoinvokeschedule(fort_writer, parser):
    '''Check that the FortranWriter class nemoinvokeschedule accepts the
    NemoInvokeSchedule node and prints the expected code (from any
    children of the node as the node itself simply calls its
    children).

    '''
    from psyclone.nemo import NemoInvokeSchedule
    code = (
        "program test\n"
        "  integer :: a\n"
        "  a=1\n"
        "end program test\n")
    schedule = get_nemo_schedule(parser, code)
    assert isinstance(schedule, NemoInvokeSchedule)
    result = fort_writer(schedule)
    assert "a=1\n" in result


def test_fw_nemokern(fort_writer, parser):
    '''Check the FortranWriter class nemokern method prints the
    class information and calls any children. This method is used to
    output nothing for a NemoKern object and simply call its children
    as NemoKern is a collection of PSyIR nodes so needs no
    output itself.

    '''
    from psyclone.nemo import NemoKern
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "program test\n"
        "  integer :: i, j, k, n\n"
        "  real :: a(n,n,n)\n"
        "  do k=1,n\n"
        "    do j=1,n\n"
        "      do i=1,n\n"
        "        a(i,j,k) = 0.0\n"
        "      end do\n"
        "    end do\n"
        "  end do\n"
        "end program test")
    schedule = get_nemo_schedule(parser, code)

    kernel = schedule[0].loop_body[0].loop_body[0].loop_body[0]
    assert isinstance(kernel, NemoKern)

    result = fort_writer(schedule)
    assert (
        "    do i = 1, n, 1\n"
        "      a(i,j,k)=0.0\n"
        "    enddo\n" in result)


def test_fw_nemoimplicitloop(fort_writer, parser):
    '''Check that the FortranWriter class nemoimplicitloop accepts the
    NemoImplicitLoop node and prints the expected code.

    '''
    from psyclone.nemo import NemoImplicitLoop
    code = (
        "program test\n"
        "  real a(10,10,10)\n"
        "  a(:,:,:)=0.0\n"
        "end program test\n")
    schedule = get_nemo_schedule(parser, code)
    implicit_loop = schedule[0]
    assert isinstance(implicit_loop, NemoImplicitLoop)
    result = fort_writer(schedule)
    assert "a(:, :, :) = 0.0\n" in result


def test_fw_query_intrinsics(fort_writer):
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
    schedule = create_schedule(code, "test_kern")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule).lower()
    assert "mysize=size(a, 2)" in result
    assert "lb=lbound(a, 2)" in result
    assert "ub=ubound(a, 2)" in result


def test_fw_literal_node(fort_writer):
    ''' Test the PSyIR literals are converted to the proper Fortran format
    when necessary. '''

    # By default literals are not modified
    lit1 = Literal('a', DataType.CHARACTER)
    result = fort_writer(lit1)
    assert result == 'a'

    lit1 = Literal('3.14', DataType.REAL)
    result = fort_writer(lit1)
    assert result == '3.14'

    # Check that BOOLEANS use the FORTRAN formatting
    lit1 = Literal('true', DataType.BOOLEAN)
    result = fort_writer(lit1)
    assert result == '.true.'
    lit1 = Literal('false', DataType.BOOLEAN)
    result = fort_writer(lit1)
    assert result == '.false.'
