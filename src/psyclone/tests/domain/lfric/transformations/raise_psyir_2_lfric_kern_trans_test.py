# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab

'''pytest tests for the the lfric kern_trans transformation. This
transformation raises generic PSyIR representing a kernel-layer
routine to PSyclone kernel-layer-specific PSyIR which uses specialised
classes.

'''
import pytest

from psyclone.domain.lfric.kernel.psyir import LFRicContainer
from psyclone.domain.lfric.transformations.raise_psyir_2_lfric_kern_trans \
    import RaisePSyIR2LFRicKernTrans
from psyclone.domain.gocean.transformations.raise_psyir_2_gocean_kern_trans \
    import find_symbol
from psyclone.psyir.nodes import FileContainer, Container, Routine
from psyclone.psyir.symbols import SymbolTable, DataTypeSymbol
from psyclone.psyir.transformations import TransformationError

METADATA = (
    "type, extends(kernel_type) :: testkern_type\n"
    "   type(arg_type), dimension(1) :: meta_args =  (/     &\n"
    "           arg_type(gh_field,    gh_real, gh_inc,  w1) &\n"
    "         /)\n"
    "   integer :: operates_on = cell_column\n"
    " contains\n"
    "   procedure, nopass :: code => testkern_code\n"
    "end type testkern_type\n")


PROGRAM = (
    f"module dummy\n"
    f"{METADATA}"
    f"contains\n"
    f"  subroutine compute_cu_code()\n"
    f"  end subroutine compute_cu_code\n"
    f"end module dummy\n")


# find_symbol function

def test_find_symbol(fortran_reader):
    '''Test that the PSyIR tree walk to find the metadata symbol works if
    multiple symbol tables need to be searched before finding the
    correct one.

    '''
    my_program = (f"module dummy\n"
                  f"contains\n"
                  f"subroutine dummy1()\n"
                  f"end subroutine\n"
                  f"  subroutine dummy2()\n"
                  f"{METADATA}"
                  f"  end subroutine\n"
                  f"end module\n")
    kernel_psyir = fortran_reader.psyir_from_source(my_program)
    symbol, node = find_symbol(kernel_psyir, "testkern_type")
    assert isinstance(symbol, DataTypeSymbol)
    assert symbol.name == "testkern_type"
    assert isinstance(node, Routine)
    assert node.name == "dummy2"
    symbol, node = find_symbol(kernel_psyir, "not_here")
    assert symbol is None
    assert node is None


def test_kerntrans_init():
    '''Test that an instance of RaisePSyIR2LFRicKernTrans can be
    succesfully created and raises the expected exception if the
    supplied argument is invalid.

    '''
    kern_trans = RaisePSyIR2LFRicKernTrans("dummy")
    assert isinstance(kern_trans, RaisePSyIR2LFRicKernTrans)
    assert kern_trans._metadata_name == "dummy"
    with pytest.raises(TransformationError) as info:
        _ = RaisePSyIR2LFRicKernTrans("")
    assert ("The RaisePSyIR2LFRicKernTrans transformation requires the "
            "name of the variable containing the metadata to be set to a "
            "valid value, but found ''." in str(info.value))
    with pytest.raises(TransformationError) as info:
        _ = RaisePSyIR2LFRicKernTrans("1dummy")
    assert ("The RaisePSyIR2LFRicKernTrans transformation requires the "
            "name of the variable containing the metadata to be set to a "
            "valid value, but found '1dummy'." in str(info.value))


def test_validate_nosymbol(fortran_reader):
    '''Test that the validate method raises an exception if the
    metadata_name property does not correspond to a symbol in the
    supplied PSyIR.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = RaisePSyIR2LFRicKernTrans("does_not_exist")
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(kernel_psyir)
    assert ("The metadata name (does_not_exist) provided to the "
            "transformation does not correspond to a symbol in the "
            "supplied PSyIR." in str(info.value))


def test_validate_container1(fortran_reader):
    '''Test that the validate method raises the expected exceptions if the
    metadata symol does not reside within a Container node or resides
    within a FileContainer node.

    '''
    code = (
        f"subroutine test\n"
        f"{METADATA}\n"
        f"end subroutine test\n")
    kernel_psyir = fortran_reader.psyir_from_source(code)
    kern_trans = RaisePSyIR2LFRicKernTrans("testkern_type")
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(kernel_psyir)
    assert ("The Container in which the metadata symbol resides is a "
            "FileContainer, but should be a generic Container."
            in str(info.value))


def test_validate_keyerror(fortran_reader):
    '''Test that the PSyIR tree walk to find the metadata symbol works if
    multiple symbol tables need to be searched before finding the
    correct one.

    '''
    my_program = (f"module dummy\n"
                  f"contains\n"
                  f"subroutine dummy1()\n"
                  f"end subroutine\n"
                  f"  subroutine dummy2()\n"
                  f"{METADATA}"
                  f"  end subroutine\n"
                  f"end module\n")
    kernel_psyir = fortran_reader.psyir_from_source(my_program)
    kern_trans = RaisePSyIR2LFRicKernTrans("testkern_type")
    kern_trans.validate(kernel_psyir)


def test_validate_container2(fortran_reader):
    '''Test that the validate method raises the expected exception if the
    supplied node is not a Container node.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = RaisePSyIR2LFRicKernTrans("testkern_type")
    routine = kernel_psyir.children[0].children[0]
    routine.detach()
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(routine)
    assert ("Error in RaisePSyIR2LFRicKernTrans transformation. The "
            "supplied node should be a Container but found 'Routine'."
            in str(info.value))


def test_validate_parent(fortran_reader):
    '''Test that the validate method raises the expected exception if the
    supplied node has a parent.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    _ = FileContainer.create("test", SymbolTable(), [kernel_psyir])
    kern_trans = RaisePSyIR2LFRicKernTrans("testkern_type")
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(kernel_psyir)
    assert ("Error in RaisePSyIR2LFRicKernTrans transformation. The "
            "supplied node should be the root of a PSyIR tree but this node "
            "has a parent (FileContainer)." in str(info.value))


def test_validate_ok(fortran_reader):
    '''Test that the validate method accepts valid psyir.'''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = RaisePSyIR2LFRicKernTrans("testkern_type")
    kern_trans.validate(kernel_psyir)


def test_apply_validate(fortran_reader, monkeypatch):
    '''Test that the validate method is called from the apply method. This
    is done by monkeypatching the validate method.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = RaisePSyIR2LFRicKernTrans("testkern_type")

    def dummy(_, options=None):
        '''Dummy method used to check that the validate method is called from
        the apply method.

        '''
        raise RuntimeError("dummy called")

    monkeypatch.setattr(kern_trans, "validate", dummy)
    with pytest.raises(RuntimeError) as info:
        kern_trans.apply(kernel_psyir)
    assert "dummy called" in str(info.value)


def test_apply_ok(fortran_reader):
    '''Test that the apply method specialises the expected container node,
    adding the required metadata and removes the original psyir
    symbol.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = RaisePSyIR2LFRicKernTrans("testkern_type")
    container = kernel_psyir.children[0]
    assert isinstance(container, Container)
    assert not isinstance(container, LFRicContainer)
    # The symbol should exist
    _ = container.symbol_table.lookup("testkern_type")
    kern_trans.apply(kernel_psyir)
    container = kernel_psyir.children[0]
    # The symbol should be removed
    with pytest.raises(KeyError):
        _ = container.symbol_table.lookup("testkern_type")
    # The container should now be a LFRicContainer
    assert isinstance(container, LFRicContainer)
    # and should contain the metadata
    expected = (
        "TYPE, PUBLIC, EXTENDS(kernel_type) :: testkern_type\n"
        "  TYPE(arg_type) :: meta_args(1) = (/ &\n"
        "arg_type(GH_FIELD, gh_real, gh_inc, w1)/)\n"
        "  INTEGER :: OPERATES_ON = cell_column\n"
        "  CONTAINS\n"
        "    PROCEDURE, NOPASS :: testkern_code\n"
        "END TYPE testkern_type\n")
    assert container.metadata.fortran_string() == expected
