# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the declaration-processing functionality of
the fparser2 PSyIR front-end '''

from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Specification_Part
from psyclone.errors import InternalError
from psyclone.psyir.nodes import KernelSchedule
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import BinaryOperation, Reference, Literal
from psyclone.psyir.symbols import (Symbol, DataSymbol, DeferredType,
                                    ScalarType, UnresolvedInterface,
                                    ArrayType, SymbolError, UnknownFortranType)


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations():
    '''Test that process_declarations method of Fparser2Reader
    converts the fparser2 declarations to symbols in the provided
    parent Kernel Schedule.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Test simple declarations
    reader = FortranStringReader("integer :: l1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = fake_parent.symbol_table.lookup("l1")
    assert l1_var.name == 'l1'
    assert isinstance(l1_var.datatype, ScalarType)
    assert l1_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert l1_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert l1_var.is_local

    reader = FortranStringReader("Real      ::      l2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l2_var = fake_parent.symbol_table.lookup("l2")
    assert l2_var.name == "l2"
    assert isinstance(l2_var.datatype, ScalarType)
    assert l2_var.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert l2_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert l2_var.is_local

    reader = FortranStringReader("LOGICAL      ::      b")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    b_var = fake_parent.symbol_table.lookup("b")
    assert b_var.name == "b"
    # Symbol should be public by default
    assert b_var.visibility == Symbol.Visibility.PUBLIC
    assert isinstance(b_var.datatype, ScalarType)
    assert b_var.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert b_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert b_var.is_local

    # public/private attribute
    reader = FortranStringReader("real, public :: p2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert (fake_parent.symbol_table.lookup("p2").visibility ==
            Symbol.Visibility.PUBLIC)
    reader = FortranStringReader("real, private :: p3, p4")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert (fake_parent.symbol_table.lookup("p3").visibility ==
            Symbol.Visibility.PRIVATE)
    assert (fake_parent.symbol_table.lookup("p4").visibility ==
            Symbol.Visibility.PRIVATE)

    # Initialisations of static constant values (parameters)
    reader = FortranStringReader("integer, parameter :: i1 = 1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    newsymbol = fake_parent.symbol_table.lookup("i1")
    assert newsymbol.is_constant
    assert isinstance(newsymbol.constant_value, Literal)
    assert newsymbol.constant_value.value == "1"

    reader = FortranStringReader("real, parameter :: i2 = 2.2, i3 = 3.3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("i2").constant_value.value == "2.2"
    assert fake_parent.symbol_table.lookup("i3").constant_value.value == "3.3"

    # Initialisation with constant expressions
    reader = FortranStringReader("real, parameter :: i4 = 1.1, i5 = i4 * 2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("i4").constant_value.value == "1.1"
    assert isinstance(fake_parent.symbol_table.lookup("i5").constant_value,
                      BinaryOperation)

    # Initialisation with a constant expression (1) and with a symbol (val1)
    reader = FortranStringReader("integer, parameter :: val1 = 1, val2 = val1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("val1").constant_value.value == "1"
    assert isinstance(
        fake_parent.symbol_table.lookup("val2").constant_value, Reference)
    assert fake_parent.symbol_table.lookup("val2").constant_value.symbol == \
        fake_parent.symbol_table.lookup("val1")

    # Initialisation with a complex constant expression
    reader = FortranStringReader(
        "integer, parameter :: val3 = 2 * (val1 + val2) + 2_precisionkind")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    # Val3 has been given a constant expression
    assert fake_parent.symbol_table.lookup("val3").constant_value
    # The new symbol (precisionkind) has been added to the parent Symbol Table
    assert fake_parent.symbol_table.lookup("precisionkind")

    # Check we catch duplicated symbols
    reader = FortranStringReader("integer :: i2")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(SymbolError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("Symbol 'i2' already present in SymbolTable with a defined "
            "interface" in str(error.value))


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_accessibility():
    ''' Check that process_declarations behaves as expected when a visibility
    map is or is not supplied. '''
    sched = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("private :: x\n"
                                 "real :: x\n")
    fparser2spec = Specification_Part(reader).content
    _, vis_map = processor.process_access_statements(fparser2spec)
    processor.process_declarations(sched, fparser2spec, [], vis_map)
    xsym = sched.symbol_table.lookup("x")
    assert xsym.visibility == Symbol.Visibility.PRIVATE
    # Repeat but change the default visibility in the parent table
    reader = FortranStringReader("real :: y\n")
    fparser2spec = Specification_Part(reader).content
    sched.symbol_table.default_visibility = Symbol.Visibility.PRIVATE
    processor.process_declarations(sched, fparser2spec, [])
    ysym = sched.symbol_table.lookup("y")
    assert ysym.visibility == Symbol.Visibility.PRIVATE
    # Repeat but provide a visibility mapping
    reader = FortranStringReader("real :: z\n")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(
        sched, fparser2spec, [],
        visibility_map={"z": Symbol.Visibility.PRIVATE})
    zsym = sched.symbol_table.lookup("z")
    assert zsym.visibility == Symbol.Visibility.PRIVATE


@pytest.mark.usefixtures("f2008_parser")
def test_process_unsupported_declarations(fortran_reader):
    ''' Check that the frontend handles unsupported declarations by
    creating symbols of UnknownFortranType. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Initial values for variables are not supported so we should get a symbol
    # with unknown type.
    reader = FortranStringReader("real:: a = 1.1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    asym = fake_parent.symbol_table.lookup("a")
    assert isinstance(asym.datatype, UnknownFortranType)
    assert asym.datatype.declaration == "REAL :: a = 1.1"

    reader = FortranStringReader("real:: b = 1.1, c = 2.2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    bsym = fake_parent.symbol_table.lookup("b")
    assert isinstance(bsym.datatype, UnknownFortranType)
    assert bsym.datatype.declaration == "REAL :: b = 1.1"
    csym = fake_parent.symbol_table.lookup("c")
    assert isinstance(csym.datatype, UnknownFortranType)
    assert csym.datatype.declaration == "REAL :: c = 2.2"

    # Multiple symbols with a single attribute
    reader = FortranStringReader("integer, private :: d = 1, e = 2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    dsym = fake_parent.symbol_table.lookup("d")
    assert isinstance(dsym.datatype, UnknownFortranType)
    assert dsym.datatype.declaration == "INTEGER, PRIVATE :: d = 1"
    esym = fake_parent.symbol_table.lookup("e")
    assert isinstance(esym.datatype, UnknownFortranType)
    assert esym.datatype.declaration == "INTEGER, PRIVATE :: e = 2"

    # Multiple attributes
    reader = FortranStringReader(
        "INTEGER, PRIVATE, DIMENSION(3) :: f = 2, g = 3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    fsym = fake_parent.symbol_table.lookup("f")
    assert isinstance(fsym.datatype, UnknownFortranType)
    assert (fsym.datatype.declaration ==
            "INTEGER, PRIVATE, DIMENSION(3) :: f = 2")
    gsym = fake_parent.symbol_table.lookup("g")
    assert isinstance(gsym.datatype, UnknownFortranType)
    assert (gsym.datatype.declaration ==
            "INTEGER, PRIVATE, DIMENSION(3) :: g = 3")

    # Test with unsupported intrinsic type. Note the space before complex
    # below which stops the line being treated as a comment.
    reader = FortranStringReader(" complex     ::      c2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    c2sym = fake_parent.symbol_table.lookup("c2")
    assert isinstance(c2sym.datatype, UnknownFortranType)
    assert c2sym.datatype.declaration == "COMPLEX :: c2"

    # Char lengths are not supported
    psyir = fortran_reader.psyir_from_source("program dummy\n"
                                             "character :: l*4\n"
                                             "end program")
    assert isinstance(psyir.children[0].symbol_table.lookup("l").datatype,
                      UnknownFortranType)
    psyir = fortran_reader.psyir_from_source("program dummy\n"
                                             "character(len=4) :: l\n"
                                             "end program")
    assert isinstance(psyir.children[0].symbol_table.lookup("l").datatype,
                      UnknownFortranType)

    # Unsupported initialisation of a parameter which comes after a valid
    # initialisation
    reader = FortranStringReader(
        "INTEGER, PARAMETER :: happy=1, fbsp = SELECTED_REAL_KIND( 6, 37)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    fbsym = fake_parent.symbol_table.lookup("fbsp")
    assert isinstance(fbsym.datatype, UnknownFortranType)
    assert (fbsym.datatype.declaration ==
            "INTEGER, PARAMETER :: fbsp = SELECTED_REAL_KIND(6, 37)")
    # The first parameter should have been handled correctly
    hsym = fake_parent.symbol_table.lookup("happy")
    assert hsym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert hsym.constant_value.value == "1"
    # The fparser2 parse tree should still be intact
    assert ("INTEGER, PARAMETER :: happy = 1, fbsp = SELECTED_REAL_KIND(6, 37)"
            in str(fparser2spec))


@pytest.mark.usefixtures("f2008_parser")
def test_unsupported_decln_duplicate_symbol():
    ''' Check that we raise the expected error when an unsupported declaration
    of only one symbol clashes with an existing entry in the symbol table. '''
    fake_parent = KernelSchedule("dummy_schedule")
    fake_parent.symbol_table.add(Symbol("var"))
    processor = Fparser2Reader()
    # Note leading white space to ensure fparser doesn't identify a comment
    reader = FortranStringReader(" complex var")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "An entry for symbol 'var' is already in the" in str(err.value)


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("precision", [1, 2, 4, 8, 16, 32])
@pytest.mark.parametrize("type_name,fort_name",
                         [(ScalarType.Intrinsic.INTEGER, "integer"),
                          (ScalarType.Intrinsic.REAL, "real"),
                          (ScalarType.Intrinsic.BOOLEAN, "logical")])
def test_process_declarations_precision(precision, type_name, fort_name):
    '''Test that process_declarations method of Fparser2Reader converts
    the fparser2 declarations with explicit precision of the form
    datatype*precision e.g. real*8, to symbols with the expected
    precision in the provided parent Kernel Schedule.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("{0}*{1} :: l1".format(fort_name, precision))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = fake_parent.symbol_table.lookup("l1")
    assert l1_var.name == 'l1'
    assert isinstance(l1_var.datatype, ScalarType)
    assert l1_var.datatype.intrinsic == type_name
    assert l1_var.datatype.precision == precision
    assert l1_var.is_local


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_double_precision():
    '''Test that process_declarations method of Fparser2Reader converts
    the fparser2 declarations specified as double precision to symbols
    with the expected precision.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("double precision :: x")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    x_var = fake_parent.symbol_table.lookup("x")
    assert x_var.name == 'x'
    assert isinstance(x_var.datatype, ScalarType)
    assert x_var.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert x_var.datatype.precision == ScalarType.Precision.DOUBLE
    assert x_var.is_local


@pytest.mark.usefixtures("f2008_parser")
def test_process_array_declarations():
    ''' Test that Fparser2Reader.process_declarations() handles various forms
    of array declaration.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # RHS array specifications
    reader = FortranStringReader("integer :: l3(l1)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l3_var = fake_parent.symbol_table.lookup("l3")
    assert l3_var.name == 'l3'
    assert isinstance(l3_var.datatype, ArrayType)
    assert l3_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert len(l3_var.datatype.shape) == 1
    assert l3_var.datatype.precision == ScalarType.Precision.UNDEFINED

    reader = FortranStringReader("integer :: l4(l1, 2)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l4_var = fake_parent.symbol_table.lookup("l4")
    assert l4_var.name == 'l4'
    assert isinstance(l4_var.datatype, ArrayType)
    assert l4_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert len(l4_var.datatype.shape) == 2
    assert l4_var.datatype.precision == ScalarType.Precision.UNDEFINED

    reader = FortranStringReader("integer :: l5(2), l6(3)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l5_datatype = fake_parent.symbol_table.lookup("l5").datatype
    assert len(l5_datatype.shape) == 1
    assert isinstance(l5_datatype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(l5_datatype.shape[0].lower, Literal)
    assert isinstance(l5_datatype.shape[0].upper, Literal)
    assert l5_datatype.shape[0].lower.value == '1'
    assert l5_datatype.shape[0].upper.value == '2'
    assert (l5_datatype.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (l5_datatype.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    l6_datatype = fake_parent.symbol_table.lookup("l6").datatype
    assert len(l6_datatype.shape) == 1
    assert isinstance(l6_datatype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(l6_datatype.shape[0].lower, Literal)
    assert isinstance(l6_datatype.shape[0].upper, Literal)
    assert l6_datatype.shape[0].lower.value == '1'
    assert l6_datatype.shape[0].upper.value == '3'
    assert (l6_datatype.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (l6_datatype.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)

    # Test that component-array-spec has priority over dimension attribute
    reader = FortranStringReader("integer, dimension(2) :: l7(3, 2)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l7_datasymbol = fake_parent.symbol_table.lookup("l7")
    assert l7_datasymbol.name == 'l7'
    assert len(l7_datasymbol.shape) == 2
    l7_datatype = l7_datasymbol.datatype
    assert isinstance(l7_datatype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(l7_datatype.shape[0].upper, Literal)
    assert l7_datatype.shape[0].upper.value == '3'
    assert (l7_datatype.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (l7_datatype.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert isinstance(l7_datatype.shape[1], ArrayType.ArrayBounds)
    assert isinstance(l7_datatype.shape[1].upper, Literal)
    assert l7_datatype.shape[1].upper.value == '2'
    assert (l7_datatype.shape[1].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (l7_datatype.shape[1].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)

    # Allocatable
    reader = FortranStringReader("integer, allocatable :: l8(:)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l8")
    assert symbol.name == "l8"
    assert symbol.datatype.precision == ScalarType.Precision.UNDEFINED
    assert symbol.shape == [ArrayType.Extent.DEFERRED]

    reader = FortranStringReader("integer, allocatable, dimension(:,:) :: l9")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l9")
    assert symbol.name == "l9"
    assert symbol.shape == [ArrayType.Extent.DEFERRED,
                            ArrayType.Extent.DEFERRED]

    # Unknown extents but not allocatable
    reader = FortranStringReader("integer :: l10(:, :)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l10")
    assert symbol.name == "l10"
    assert symbol.shape == [ArrayType.Extent.ATTRIBUTE,
                            ArrayType.Extent.ATTRIBUTE]

    # Extent given by variable with UnknownFortranType
    udim = DataSymbol("udim", UnknownFortranType("integer :: udim"),
                      interface=UnresolvedInterface())
    fake_parent.symbol_table.add(udim)
    reader = FortranStringReader("integer :: l11(udim)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l11")
    assert symbol.name == "l11"
    assert len(symbol.shape) == 1
    # Upper bound of extent should be the udim Symbol
    reference = symbol.shape[0].upper
    assert isinstance(reference, Reference)
    assert reference.name == "udim"
    assert reference.symbol is udim
    assert isinstance(reference.symbol.datatype, UnknownFortranType)

    # Extent given by variable with DeferredType
    ddim = DataSymbol("ddim", DeferredType(),
                      interface=UnresolvedInterface())
    fake_parent.symbol_table.add(ddim)
    reader = FortranStringReader("integer :: l12(ddim)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l12")
    assert symbol.name == "l12"
    assert len(symbol.shape) == 1
    assert isinstance(symbol.shape[0].lower, Literal)
    # Upper bound of extent should now be the ddim Symbol
    reference = symbol.shape[0].upper
    assert reference.name == "ddim"
    assert reference.symbol is ddim
    assert isinstance(reference.symbol.datatype, DeferredType)


@pytest.mark.usefixtures("f2008_parser")
def test_process_not_supported_declarations():
    '''Test that process_declarations method raises the proper errors when
    declarations contain unsupported attributes.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("integer, external :: arg1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("arg1").datatype,
                      UnknownFortranType)

    reader = FortranStringReader("real, allocatable :: p3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("p3").datatype,
                      UnknownFortranType)

    # Allocatable but with specified extent. This is invalid Fortran but
    # fparser2 doesn't spot it (see fparser/#229).
    reader = FortranStringReader("integer, allocatable :: l10(5)")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(InternalError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "An array with defined extent cannot have the ALLOCATABLE" \
        in str(err.value)

    reader = FortranStringReader("integer, allocatable, dimension(n) :: l10")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(InternalError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "An array with defined extent cannot have the ALLOCATABLE" \
        in str(err.value)

    reader = FortranStringReader("integer :: l11")
    fparser2spec = Specification_Part(reader).content[0]
    # Break the parse tree
    fparser2spec.items = ("hello", fparser2spec.items[1],
                          fparser2spec.items[2])
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l11sym = fake_parent.symbol_table.lookup("l11")
    assert isinstance(l11sym.datatype, UnknownFortranType)
