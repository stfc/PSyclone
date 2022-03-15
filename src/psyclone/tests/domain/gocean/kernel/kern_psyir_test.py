# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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

'''Module containing tests for the KernelMetadataSymbol
kernel-layer-specific symbol. The tests include
translation of PSyIR to PSyclone Kernel PSyIR and PSyclone
Kernel PSyIR to processed PSyIR.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.gocean.kernel import KernelMetadataSymbol
from psyclone.domain.gocean.transformations import KernTrans
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.symbols import DataTypeSymbol, UnknownFortranType

METADATA = (
    "  type, extends(kernel_type) :: compute_cu\n"
    "     type(go_arg), dimension(3) :: meta_args =                 &\n"
    "          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),            &\n"
    "             go_arg(GO_READ,  GO_CT, GO_STENCIL(000,011,000)), &\n"
    "             go_arg(GO_READ,  GO_GRID_AREA_T)                  &\n"
    "           /)\n"
    "     integer :: ITERATES_OVER = GO_ALL_PTS\n"
    "     integer :: index_offset = GO_OFFSET_SW\n"
    "  contains\n"
    "    procedure, nopass :: code => compute_cu_code\n"
    "  end type compute_cu\n")

PROGRAM = (
    f"program dummy\n"
    f"  use random ! avoid any missing declaration errors\n"
    f"{METADATA}"
    f"end program dummy\n")


def test_kernelmetadatasymbol_create(fortran_reader):
    '''Test we can create a KernelMetadataSymbol and capture the expected
    values.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.apply(kernel_psyir)
    symbol = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    assert isinstance(symbol, KernelMetadataSymbol)
    assert symbol.iterates_over == "GO_ALL_PTS"
    assert symbol.index_offset == "GO_OFFSET_SW"
    assert symbol.code == "compute_cu_code"
    assert isinstance(symbol.args, list)
    assert len(symbol.args) == 3
    assert isinstance(symbol.args[0], KernelMetadataSymbol.FieldArg)
    assert symbol.args[0].access == "GO_WRITE"
    assert symbol.args[0].stagger == "GO_CU"
    assert symbol.args[0].form == "GO_POINTWISE"
    assert symbol.args[0].stencil is None
    assert isinstance(symbol.args[1], KernelMetadataSymbol.FieldArg)
    assert symbol.args[1].access == "GO_READ"
    assert symbol.args[1].stagger == "GO_CT"
    assert symbol.args[1].form == "GO_STENCIL"
    assert isinstance(symbol.args[1].stencil, list)
    assert symbol.args[1].stencil == ["000", "011", "000"]
    assert isinstance(symbol.args[2], KernelMetadataSymbol.GridArg)
    assert symbol.args[2].access == "GO_READ"
    assert symbol.args[2].name == "GO_GRID_AREA_T"

# internal GridArg class
# internal FieldArg class

# _set_property: tested by other tests

# _get_property
# 1 code, 2 property, 3 error
def test_getproperty(fortran_reader):
    '''Test utilitity function that takes metadata in an fparser2 tree and
    returns the value associated with the supplied property name.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.apply(kernel_psyir)
    symbol = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    spec_part = symbol._string_to_fparser(symbol.datatype)
    assert symbol._get_property(spec_part, "code").string == "compute_cu_code"
    assert symbol._get_property(spec_part, "iterates_over").string == \
        "GO_ALL_PTS"
    with pytest.raises(InternalError) as info:
        symbol._get_property(spec_part, "not_found")
    assert ("The property name should always be found in the metadata but "
            "'not_found' was not found in TYPE, EXTENDS(kernel_type) :: "
            "compute_cu" in str(info.value))


# iterates_over get and set
# index_offset get and set
# code get and set
def test_getsetproperties(fortran_reader):
    '''Test that the get and set functions for the "iterates_over",
    "index_offset" and "code" properties work correctly.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.apply(kernel_psyir)
    symbol = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    # get values
    assert symbol.iterates_over == "GO_ALL_PTS"
    assert symbol.index_offset == "GO_OFFSET_SW"
    assert symbol.code == "compute_cu_code"
    # set values
    symbol.iterates_over = "GO_INTERNAL_PTS"
    assert symbol.iterates_over == "GO_INTERNAL_PTS"
    symbol.index_offset = "GO_OFFSET_NE"
    assert symbol.index_offset == "GO_OFFSET_NE"
    symbol.code = "fred"
    assert symbol.code == "fred"
    # set values errors
    with pytest.raises(ValueError) as info:
        symbol.iterates_over = "error"
    assert ("Expected one of ['go_all_pts', 'go_internal_pts', "
            "'go_external_pts'], but found 'error'." in str(info.value))
    with pytest.raises(ValueError) as info:
        symbol.index_offset = "error"
    assert ("Expected one of ['go_offset_se', 'go_offset_sw', "
            "'go_offset_ne', 'go_offset_nw', 'go_offset_any'], but found "
            "'error'." in str(info.value))


# args
def test_args(fortran_reader):
    '''Test that the args function works correctly.'''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.apply(kernel_psyir)
    symbol = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    assert len(symbol.args) == 3
    assert isinstance(symbol.args[0], KernelMetadataSymbol.FieldArg)
    assert isinstance(symbol.args[1], KernelMetadataSymbol.FieldArg)
    assert isinstance(symbol.args[2], KernelMetadataSymbol.GridArg)


# ??? validate

# gridarg
#   access get/set
#   name get/set
def test_gridarg(fortran_reader):
    '''Test the internal GridArg class.'''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.apply(kernel_psyir)
    symbol = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    grid_arg = symbol.args[2]
    assert isinstance(grid_arg, KernelMetadataSymbol.GridArg)
    # get
    assert grid_arg.access == "GO_READ"
    assert grid_arg.name == "GO_GRID_AREA_T"
    # set
    # args error


# gridarg error (number of args)

# fieldarg
#   access get/set
#   stagger get/set
#   form get/set
#   stencil get/set







@pytest.mark.parametrize("content,error", [
    ("integer :: index_offset = go_offset_se", "index_offset"),
    ("integer :: iterates_over = go_inner_pts", "iterates_over"),
    ("type(go_arg), dimension(1) :: meta_args = "
     "(/ go_arg(GO_READ, GO_CU, GO_POINTWISE)/)\n", "meta_args")])
def test_kernelmetadatasymbol_multi(fortran_reader, content, error):
    '''Test the expected exception is raised in KernelMetadataSymbol if
    entries are declared more than once.

    '''
    my_metadata = PROGRAM.replace(
        f"  contains\n",
        f"    {content}\n"
        f"  contains\n")
    kernel_psyir = fortran_reader.psyir_from_source(my_metadata)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(ParseError) as info:
        kern_trans.apply(kernel_psyir)
    assert (f"'{error}' should only be defined once in the metadata, but "
            f"found TYPE(go_arg), DIMENSION(3)" in str(info.value))


def test_kernelmetadatasymbol_invalid(fortran_reader):
    '''Test the expected exception is raised in KernelMetadataSymbol if an
    unexpected entry is found. Here we replace ITERATES_OVER with xxx.

    '''
    my_metadata = PROGRAM.replace("ITERATES_OVER", "xxx")
    kernel_psyir = fortran_reader.psyir_from_source(my_metadata)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(ParseError) as info:
        kern_trans.apply(kernel_psyir)
    assert ("Expecting metadata entries to be one of 'meta_args', "
            "'iterates_over', or 'index_offset', but found 'xxx' in "
            "TYPE(go_arg), DIMENSION(3)" in str(info.value))


@pytest.mark.parametrize("content,error", [
    ("integer :: ITERATES_OVER = GO_ALL_PTS\n", "iterates_over"),
    ("integer :: index_offset = GO_OFFSET_SW\n", "index_offset"),
     ("     type(go_arg), dimension(3) :: meta_args =                 &\n"
      "          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),            &\n"
      "             go_arg(GO_READ,  GO_CT, GO_STENCIL(000,011,000)), &\n"
      "             go_arg(GO_READ,  GO_GRID_AREA_T)                  &\n"
      "           /)\n", "meta_args")])
def test_kernelmetadatasymbol_missing(fortran_reader, content, error):
    '''Test the expected exception is raised in KernelMetadataSymbol if
    entries are missing.

    '''
    my_metadata = PROGRAM.replace(
        f"{content}", "")
    kernel_psyir = fortran_reader.psyir_from_source(my_metadata)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(ParseError) as info:
        kern_trans.apply(kernel_psyir)
    assert (f"Expecting '{error}' to be an entry in the metadata but it was "
            f"not found in " in str(info.value))


def test_kernelmetadatasymbol_contains(fortran_reader):
    '''Test the expected exception is raised in KernelMetadataSymbol if
    both the code and contains are missing.

    '''
    my_metadata = PROGRAM.replace(
        "    procedure, nopass :: code => compute_cu_code\n", "")
    my_metadata = my_metadata.replace("  contains\n", "")
    kernel_psyir = fortran_reader.psyir_from_source(my_metadata)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(ParseError) as info:
        kern_trans.apply(kernel_psyir)
    assert ("The metadata does not have a contains keyword (which is "
            "required to add the code metadata." in str(info.value))


@pytest.mark.parametrize("content,error", [
    ("", 0),
    ("    procedure, nopass :: code => compute_cu_code\n"
     "procedure, nopass :: code => compute_cu_code\n", 2)])
def test_kernelmetadatasymbol_entries_code(fortran_reader, content, error):
    '''Test the expected exception is raised in KernelMetadataSymbol if
    there more or less than one entry after the 'contains' keyword.

    '''
    my_metadata = PROGRAM.replace(
        "    procedure, nopass :: code => compute_cu_code\n", content)
    kernel_psyir = fortran_reader.psyir_from_source(my_metadata)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(ParseError) as info:
        kern_trans.apply(kernel_psyir)
    assert (f"Expecting a single entry after the 'contains' keyword but "
            f"found {error}." in str(info.value))


def test_gridarg_args(fortran_reader):
    '''Test the expected exception is raised if a grid argument does not
    have 2 entries.'''
    pass # Not sure how to test this seeing as we choose a field arg if the number of args is not 3


def test_set_iterates_over(fortran_reader):
    ''' xxx '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.apply(kernel_psyir)
    symbol = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    assert isinstance(symbol, KernelMetadataSymbol)
    assert symbol.iterates_over == "GO_ALL_PTS"
    symbol.iterates_over = "GO_INTERNAL_PTS"
