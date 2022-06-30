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

'''pytest tests for the the gocean kern_trans transformation. This
transformation raises generic PSyIR representing a kernel-layer
routine to PSyclone kernel-layer-specific PSyIR which uses specialised
classes.

'''
import pytest

from psyclone.domain.gocean.transformations import KernTrans
from psyclone.domain.gocean.kernel import GOceanKernel
from psyclone.parse.utils import ParseError
from psyclone.psyir.nodes import FileContainer, Node, Routine
from psyclone.psyir.symbols import SymbolTable
from psyclone.psyir.transformations import TransformationError


METADATA = (
    "  type, extends(kernel_type) :: compute_cu\n"
    "     type(go_arg), dimension(3) :: meta_args =                 &\n"
    "          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),            &\n"
    "             go_arg(GO_READ,  GO_CT, GO_STENCIL(000,011,000)), &\n"
    "             go_arg(GO_READ,  GO_GRID_AREA_T),                 &\n"
    "             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE)       &\n"
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


def test_kerntrans_init():
    ''' Test that an instance of KernTrans can be succesfully created.'''
    kern_trans = KernTrans()
    assert isinstance(kern_trans, KernTrans)
    assert kern_trans._metadata_name is None


def test_validate_noname():
    '''Test that the validate method raises an exception if the
    metadata_name property has not been set (or has been set to
    something that is 'empty')

    '''
    kern_trans = KernTrans()
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(None)
    assert ("The kern_trans transformation requires the metadata name to be "
            "set before applying the transformation." in str(info.value))
    kern_trans.metadata_name = ""
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(None)
    assert ("The kern_trans transformation requires the metadata name to be "
            "set before applying the transformation." in str(info.value))


def test_validate_nosymbol():
    '''Test that the validate method raises an exception if the
    metadata_name property does not correspond to a symbol in the
    supplied PSyIR.

    '''
    kern_trans = KernTrans()
    kern_trans.metadata_name = "does_not_exist"
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(Node())
    assert ("The metadata name (does_not_exist) provided to the "
            "transformation does not correspond to a symbol in the "
            "supplied PSyIR." in str(info.value))


def test_validate_keyerror(fortran_reader):
    '''Test that the PSyIR tree walk to find the metadata symbol works if
    multiple symbol tables need to be searched before finding the
    correct one. A KeyError is raised if the symbol is not found in the
    symbol table lookup method.

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
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.validate(kernel_psyir)


def test_validate_metadata(fortran_reader):
    '''Test that the symbol setup method is called as this checks the
    metadata is valid. To trigger this we make the metadata invalid
    and check to see whethere an exception defined in the symbol setup
    method is raised.

    '''
    modified_program = PROGRAM.replace(
        "     integer :: ITERATES_OVER = GO_ALL_PTS\n", "")
    kernel_psyir = fortran_reader.psyir_from_source(modified_program)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(ParseError) as info:
        kern_trans.validate(kernel_psyir)
    assert ("'iterates_over' was not found in TYPE, EXTENDS(kernel_type) :: "
            "compute_cu" in str(info.value))


def test_validate_container(fortran_reader):
    '''Test that the validate method raises the expected exception if the
    supplied node is not a Container node.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(kernel_psyir.children[0])
    assert ("Error in KernTrans transformation. The supplied call argument "
            "should be a Container node but found 'Routine'."
            in str(info.value))


def test_validate_parent(fortran_reader):
    '''Test that the validate method raises the expected exception if the
    supplied node has a parent.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    _ = FileContainer.create("test", SymbolTable(), [kernel_psyir])
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(TransformationError) as info:
        kern_trans.validate(kernel_psyir)
    assert ("Error in KernTrans transformation. The supplied node should be "
            "the root of a PSyIR tree but this node has a parent."
            in str(info.value))


def test_validate_ok(fortran_reader):
    '''Test that the validate method accepts valid psyir.'''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.validate(kernel_psyir)


def test_metadataname():
    '''Test that the metadata_name getter and setter methods work as
    expected.

    '''
    kern_trans = KernTrans()
    assert kern_trans.metadata_name is None
    with pytest.raises(TypeError) as info:
        kern_trans.metadata_name = 0
    assert ("The kern_trans transformation requires the metadata name to be "
            "a string, but found 'int'." in str(info.value))
    kern_trans.metadata_name = "hello"
    assert kern_trans.metadata_name == "hello"


def test_apply_validate(fortran_reader, monkeypatch):
    '''Test that the validate method is called from the apply method. We
    can't make make the metadata invalid as this would be picked up
    when we specialise the symbol, even if validate were not
    called. Therefore we monkeypatch the validate method.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()

    def dummy(_, options=None):
        '''Dummy method used to check that the validate method is called from
        the apply method.

        '''
        raise RuntimeError("dummy called")

    monkeypatch.setattr(kern_trans, "validate", dummy)
    kern_trans.metadata_name = "compute_cu"
    with pytest.raises(RuntimeError) as info:
        kern_trans.apply(kernel_psyir)
    assert "dummy called" in str(info.value)


def test_apply_keyerror(fortran_reader):
    '''Test that the PSyIR tree walk to find the metadata symbol works if
    multiple symbol tables need to be searched before finding the
    correct one. A KeyError is raised if the symbol is not found in the
    symbol table lookup method.

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
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.apply(kernel_psyir)


def test_apply_ok(fortran_reader):
    '''Test that the apply method modifies the required symbol as
    expected.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    routine = kernel_psyir.children[0]
    assert isinstance(routine, Routine)
    assert not isinstance(routine, GOceanKernel)
    metadata = routine.symbol_table.lookup("compute_cu")
    kern_trans.apply(kernel_psyir)
    # This should not be found
    metadata = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    routine = kernel_psyir.children[0]
    assert isinstance(routine, GOceanKernel)
    assert routine.metadata.iterates_over == "GO_ALL_PTS"
