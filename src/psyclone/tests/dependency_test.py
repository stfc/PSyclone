# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: A. R. Porter, R. W. Ford and S. Siso, STFC Daresbury Lab
# Modified: I. Kavcic and L. Turner, Met Office


''' Module containing py.test tests for dependency analysis.'''

import os
import pytest

from psyclone.core import AccessType, Signature, VariablesAccessMap
from psyclone.domain.lfric import KernStubArgList, LFRicKern, LFRicKernMetadata
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Assignment, IfBlock, Loop
from psyclone.tests.utilities import get_invoke, get_ast

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_assignment(fortran_reader):
    ''' Check that assignments set the right read/write accesses.
    '''
    psyir = fortran_reader.psyir_from_source(
        '''program test_prog
             use some_mod, only: f
             integer :: i, j
             real :: a, b, e, x, y
             real, dimension(5,5) :: c, d
             a = b
             c(i,j) = d(i,j+1)+e+f(x,y)
             c(i) = c(i) + 1
             d(i,j) = sqrt(e(i,j))
           end program test_prog''')
    schedule = psyir.children[0]

    # Simple scalar assignment:  a = b
    scalar_assignment = schedule.children[0]
    assert isinstance(scalar_assignment, Assignment)
    var_accesses = scalar_assignment.reference_accesses()
    # Test some test functions explicitly:
    assert var_accesses.is_written(Signature("a"))
    assert not var_accesses.is_read(Signature("a"))
    assert not var_accesses.is_written(Signature("b"))
    assert var_accesses.is_read(Signature("b"))

    # Array element assignment: c(i,j) = d(i,j+1)+e+f(x,y)
    array_assignment = schedule.children[1]
    assert isinstance(array_assignment, Assignment)
    var_accesses = array_assignment.reference_accesses()
    # We don't know if 'f' is a function or an array (CALLED or READ), so
    # it is catergorised as UNKNOWN. Its arguments take the worst case
    # scenario of being READWRITE (in case it was a function).
    assert (str(var_accesses) ==
            "c: WRITE, d: READ, e: READ, f: UNKNOWN, i: READ, j: READ, "
            "x: READWRITE, y: READWRITE")
    # Increment operation: c(i) = c(i)+1
    increment_access = schedule.children[2]
    assert isinstance(increment_access, Assignment)
    var_accesses = increment_access.reference_accesses()
    assert str(var_accesses) == "c: READ+WRITE, i: READ"

    # Using an intrinsic:
    sqrt_access = schedule.children[3]
    assert isinstance(sqrt_access, Assignment)
    var_accesses = sqrt_access.reference_accesses()
    assert str(var_accesses) == "d: WRITE, e: READ, i: READ, j: READ"


def test_indirect_addressing(fortran_reader):
    ''' Check that we correctly handle indirect addressing, especially
    on the LHS. '''
    psyir = fortran_reader.psyir_from_source(
        '''program test_prog
             integer :: i, h(10)
             real :: a, g(10)
             g(h(i)) = a
           end program test_prog''')

    indirect_addressing = psyir.children[0].children[0]
    assert isinstance(indirect_addressing, Assignment)
    var_accesses = indirect_addressing.reference_accesses()
    assert str(var_accesses) == "a: READ, g: WRITE, h: READ, i: READ"


def test_double_variable_lhs(fortran_reader):
    ''' A variable can appear more than once on the LHS of an assignment.
    '''
    psyir = fortran_reader.psyir_from_source(
        '''program test_prog
             integer :: g(10)
             g(g(1)) = 1
           end program test_prog''')
    lhs = psyir.walk(Assignment)[0].lhs
    assert lhs.symbol.name == "g"
    assert lhs.children[0].symbol.name == "g"
    assert lhs.symbol is lhs.children[0].symbol


def test_if_statement(fortran_reader):
    ''' Tests handling an if statement
    '''
    psyir = fortran_reader.psyir_from_source(
        '''program test_prog
             integer :: a, b, i
             real, dimension(5) :: p, q, r
             if (a .eq. b) then
                p(i) = q(i)
             else
               q(i) = r(i)
             endif
          end program test_prog''')
    schedule = psyir.children[0]

    if_stmt = schedule.children[0]
    assert isinstance(if_stmt, IfBlock)
    var_accesses = if_stmt.reference_accesses()
    assert (str(var_accesses) == "a: READ, b: READ, i: READ, p: WRITE, "
                                 "q: READ+WRITE, r: READ")
    # Test that the two accesses to 'q' indeed show up as
    q_accesses = var_accesses[Signature("q")]
    assert len(q_accesses) == 2
    assert q_accesses[0].access_type == AccessType.READ
    assert q_accesses[1].access_type == AccessType.WRITE


def test_do_loop(fortran_reader):
    ''' Check the handling of do loops.
    '''
    psyir = fortran_reader.psyir_from_source(
        '''program test_prog
             integer :: ji, jj, n
             integer, dimension(10,10) :: s, t
             do jj=1, n
                do ji=1, 10
                   s(ji, jj)=t(ji, jj)+1
                enddo
             enddo
          end program test_prog''')
    schedule = psyir.children[0]

    do_loop = schedule.children[0]
    assert isinstance(do_loop, Loop)
    var_accesses = do_loop.reference_accesses()
    assert (str(var_accesses) ==
            "ji: WRITE+READ, jj: WRITE+READ, n: READ, s: WRITE, t: READ")


def test_nemo_array_range(fortran_reader):
    '''Check the handling of the access information for Fortran
    array notation (captured using Ranges in the PSyiR).

    '''
    psyir = fortran_reader.psyir_from_source(
        '''program test_prog
             integer :: jj, n
             real :: a, s(5,5), t(5,5)
             do jj=1, n
                s(:, jj)=t(:, jj)+a
             enddo
          end program test_prog''')
    schedule = psyir.children[0]

    do_loop = schedule.children[0]
    assert isinstance(do_loop, Loop)
    var_accesses = do_loop.reference_accesses()
    assert (str(var_accesses) ==
            "a: READ, jj: WRITE+READ, n: READ, s: INQUIRY+WRITE, "
            "t: INQUIRY+READ")


@pytest.mark.xfail(reason="Gocean loops boundaries are strings #440")
def test_goloop():
    ''' Check the handling of non-NEMO do loops.
    TODO #440: Does not work atm, GOLoops also have start/stop as
    strings, which are even not defined. Only after lowering is called will
    they be defined.
    '''

    _, invoke = get_invoke("single_invoke_two_kernels_scalars.f90",
                           "gocean", name="invoke_0")
    do_loop = invoke.schedule.children[0]
    assert isinstance(do_loop, Loop)
    var_accesses = do_loop.reference_accesses()
    assert (str(var_accesses) == ": READ, a_scalar: READ, i: READ+WRITE, "
                                 "j: READ+WRITE, " "ssh_fld: READ+WRITE, "
                                 "tmask: READ")
    # TODO #440: atm the return value starts with:  ": READ, cu_fld: WRITE ..."
    # The empty value is caused by not having start, stop, end of the loop
    # defined at this stage.


def test_goloop_partially():
    ''' Check the handling of non-NEMO do loops.
    TODO #440: This test is identical to test_goloop above, but it asserts in a
    way that works before #440 is fixed, so that we make sure we test the rest
    of the gocean variable access handling.
    '''
    _, invoke = get_invoke("single_invoke_two_kernels_scalars.f90",
                           "gocean", name="invoke_0", dist_mem=False)
    do_loop = invoke.schedule.children[0]
    assert isinstance(do_loop, Loop)

    # The third argument is GO_GRID_X_MAX_INDEX, which is scalar
    assert do_loop.args[2].is_scalar
    # The fourth argument is GO_GRID_MASK_T, which is an array
    assert not do_loop.args[3].is_scalar

    var_accesses = do_loop.reference_accesses()
    assert ("a_scalar: READ, i: WRITE+READ, j: WRITE+READ, ssh_fld%data: "
            "WRITE, ssh_fld%grid%subdomain%internal%xstop: READ, "
            "ssh_fld%grid%tmask: READ, ssh_fld%whole%xstart: READ, "
            "ssh_fld%whole%xstop: READ, ssh_fld%whole%ystart: READ, "
            "ssh_fld%whole%ystop: READ" == str(var_accesses))


def test_lfric():
    ''' Test the handling of an LFRic loop. Note that the variable
    accesses are reported based on the user's point of view, not the code
    actually created by PSyclone, e.g. it shows a dependency on 'some_field',
    but not on some_field_proxy etc. Also the dependency is at this stage taken
    from the kernel metadata, not the actual kernel usage.

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "lfric",
                                 "1_single_invoke.f90"),
                    api="lfric")
    psy = PSyFactory("lfric", distributed_memory=False).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    var_accesses = schedule.reference_accesses()
    assert str(var_accesses) == (
        "a: READ, cell: WRITE+READ, f1_data: INC, f2_data: READ, field_type: "
        "CONSTANT, i_def: CONSTANT, m1_data: READ, m2_data: READ, map_w1: "
        "READ, map_w2: READ, map_w3: READ, ndf_w1: READ, ndf_w2: READ, "
        "ndf_w3: READ, nlayers_f1: READ, r_def: CONSTANT, undf_w1: READ, "
        "undf_w2: READ, undf_w3: READ, uninitialised_loop0_start: READ, "
        "uninitialised_loop0_stop: READ")


def test_lfric_kern_cma_args():
    ''' Test the handling of LFRic kernel arguments.

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "lfric",
                                 "27.access_tests.f90"),
                    api="lfric")
    psy = PSyFactory("lfric", distributed_memory=False).create(info)
    invoke_read = psy.invokes.get('invoke_read')
    invoke_write = psy.invokes.get('invoke_write')
    invoke_read.setup_psy_layer_symbols()
    invoke_write.setup_psy_layer_symbols()
    var_accesses_read = VariablesAccessMap()
    for kernel in invoke_read.schedule.coded_kernels():
        var_accesses_read.update(kernel.reference_accesses())
    var_accesses_write = VariablesAccessMap()
    for kernel in invoke_write.schedule.coded_kernels():
        var_accesses_write.update(kernel.reference_accesses())

    # Check the parameters that will change access type according to read or
    # write declaration of the argument:
    assert (var_accesses_read[Signature("cma_op1_cma_matrix")][0].access_type
            == AccessType.READ)
    assert (var_accesses_write[Signature("cma_op1_cma_matrix")][0].access_type
            == AccessType.WRITE)

    # All other parameters are read-only (e.g. sizes, ... - they will not
    # be modified, even if the actual data is written):
    for name in ["nrow", "bandwidth", "alpha", "beta", "gamma_m",
                 "gamma_p"]:
        assert (var_accesses_read[Signature(f"cma_op1_{name}")][0].access_type
                == AccessType.READ)
        assert (var_accesses_write[Signature(f"cma_op1_{name}")][0].access_type
                == AccessType.READ)


def test_user_defined_variables(fortran_reader):
    ''' Test reading and writing to user defined variables.
    '''
    psyir = fortran_reader.psyir_from_source(
        '''program test_prog
             use some_mod, only: my_type
             type(my_type) :: a, e
             integer :: ji, jj, d
             a%b(ji)%c(ji, jj) = d
             e%f = d
           end program test_prog''')
    loops = psyir.children[0]
    var_accesses = loops.reference_accesses()
    assert var_accesses[Signature(("a", "b", "c"))].is_written
    assert var_accesses[Signature(("e", "f"))].is_written


def test_lfric_ref_element():
    '''Test handling of variables if an LFRic's RefElement is used.

    '''
    psy, invoke_info = get_invoke("23.4_ref_elem_all_faces_invoke.f90",
                                  "lfric", idx=0)
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "normals_to_faces: READ," in var_info
    assert "out_normals_to_faces: READ," in var_info
    assert "nfaces_re: READ," in var_info


def test_lfric_operator():
    '''Check if implicit basis and differential basis variables are
    handled correctly.

    '''
    psy, invoke_info = get_invoke("6.1_eval_invoke.f90", "lfric", idx=0)
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "f0_data: INC," in var_info
    assert "cmap_data: READ," in var_info
    assert "basis_w0_on_w0: READ," in var_info
    assert "diff_basis_w1_on_w0: READ," in var_info


def test_lfric_cma(fortran_writer):
    '''Test that parameters related to CMA operators are handled
    correctly in the variable usage analysis.

    '''
    _, invoke_info = get_invoke("20.0_cma_assembly.f90", "lfric", idx=0)
    invoke_info.setup_psy_layer_symbols()
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "ncell_2d: WRITE+READ," in var_info
    assert "cma_op1_alpha: WRITE+READ," in var_info
    assert "cma_op1_bandwidth: WRITE+READ," in var_info
    assert "cma_op1_beta: WRITE+READ," in var_info
    assert "cma_op1_gamma_m: WRITE+READ," in var_info
    assert "cma_op1_gamma_p: WRITE+READ," in var_info
    assert "cma_op1_cma_matrix: WRITE," in var_info
    assert "cma_op1_ncol: WRITE+READ," in var_info
    assert "cma_op1_nrow: WRITE+READ," in var_info
    assert "cbanded_map_ads1_lma_op1: WRITE+READ," in var_info
    assert "cbanded_map_ads2_lma_op1: WRITE+READ," in var_info
    assert "lma_op1_local_stencil: WRITE+READ," in var_info
    assert "lma_op1_proxy%ncell_3d: READ," in var_info


def test_lfric_cma2():
    '''Test that parameters related to CMA operators are handled
    correctly in the variable usage analysis.

    '''
    psy, invoke_info = get_invoke("20.1_cma_apply.f90", "lfric", idx=0)
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "cma_indirection_map_as1_field_a: READ," in var_info
    assert "cma_indirection_map_as2_field_b: READ," in var_info


def test_lfric_stencils():
    '''Test that stencil parameters are correctly detected.

    '''
    psy, invoke_info = get_invoke("14.4_halo_vector.f90", "lfric", idx=0)
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "f2_stencil_size: READ," in var_info
    assert "f2_stencil_dofmap: READ," in var_info


def test_lfric_various_basis():
    ''' Tests that implicit parameters for various basis related
    functionality work as expected.

    '''
    psy, invoke_info = get_invoke("10.3_operator_different_spaces.f90",
                                  "lfric", idx=0)
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "basis_w3_qr: READ," in var_info
    assert "diff_basis_w0_qr: READ," in var_info
    assert "diff_basis_w2_qr: READ," in var_info
    assert "np_xy_qr: READ," in var_info
    assert "np_z_qr: READ," in var_info
    assert "weights_xy_qr: READ," in var_info
    assert "weights_z_qr: READ" in var_info


def test_lfric_field_bc_kernel():
    '''Tests that implicit parameters in case of a boundary_dofs
    array fix are created correctly.

    '''
    psy, invoke_info = get_invoke("12.2_enforce_bc_kernel.f90",
                                  "lfric", idx=0)
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "boundary_dofs_a: READ," in var_info


def test_lfric_stencil_xory_vector():
    '''Test that the implicit parameters for a stencil access of type x
    or y with a vector field are created.

    '''
    psy, invoke_info = get_invoke("14.4.2_halo_vector_xory.f90",
                                  "lfric", idx=0)
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "f2_direction: READ," in var_info


def test_lfric_operator_bc_kernel():
    '''Tests that a kernel that applies boundary conditions to operators
    detects the right implicit paramaters.

    '''
    psy, invoke_info = get_invoke("12.4_enforce_op_bc_kernel.f90",
                                  "lfric", idx=0)
    var_info = str(invoke_info.schedule.reference_accesses())
    assert "boundary_dofs_op_a: READ," in var_info


def test_lfric_stub_args():
    '''Check that correct stub code is produced when there are multiple
    stencils.

    '''
    ast = get_ast("lfric", "testkern_stencil_multi_mod.f90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "field_1_w1: INC," in var_info
    assert "field_2_stencil_dofmap: READ," in var_info
    assert "field_2_stencil_size: READ," in var_info
    assert "field_2_w2: READ," in var_info
    assert "field_3_direction: READ," in var_info
    assert "field_3_stencil_dofmap: READ," in var_info
    assert "field_3_stencil_size: READ," in var_info
    assert "field_3_w2: READ," in var_info
    assert "field_4_stencil_dofmap: READ," in var_info
    assert "field_4_stencil_size: READ," in var_info
    assert "field_4_w3: READ," in var_info
    assert "map_w1: READ," in var_info
    assert "map_w2: READ," in var_info
    assert "map_w3: READ" in var_info
    assert "ndf_w1: READ," in var_info
    assert "ndf_w2: READ," in var_info
    assert "ndf_w3: READ," in var_info
    assert "nlayers: READ," in var_info
    assert "undf_w1: READ," in var_info
    assert "undf_w2: READ," in var_info
    assert "undf_w3: READ" in var_info


def test_lfric_stub_args2():
    '''Check variable usage detection for scalars, basis_name, quad rule
    and mesh properties.

    '''
    ast = get_ast("lfric", "testkern_mesh_prop_face_qr_mod.F90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "rscalar_1: READ," in var_info
    assert "basis_w1_qr_face: READ," in var_info
    assert "nfaces_qr_face: READ," in var_info
    assert "np_xyz_qr_face: READ," in var_info
    assert "weights_xyz_qr_face: READ" in var_info


def test_lfric_stub_args3():
    '''Check variable usage detection for cell position, operator

    '''
    ast = get_ast("lfric",
                  "testkern_any_discontinuous_space_op_1_mod.f90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "cell: READ," in var_info
    assert "op_3: READ," in var_info
    assert "op_3_ncell_3d: READ," in var_info
    assert "op_4: READ," in var_info
    assert "op_4_ncell_3d: READ," in var_info


def test_lfric_stub_boundary_dofs():
    '''Check variable usage detection for boundary dofs.

    '''
    ast = get_ast("lfric", "enforce_bc_kernel_mod.f90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    assert "boundary_dofs_field_1: READ" in str(var_accesses)


def test_lfric_stub_field_vector():
    '''Check variable usage detection field vectors.

    '''
    ast = get_ast("lfric", "testkern_stencil_vector_mod.f90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "field_1_w0_v1: READ," in var_info
    assert "field_1_w0_v2: READ," in var_info
    assert "field_1_w0_v3: READ," in var_info
    assert "field_2_w3_v1: READ," in var_info
    assert "field_2_w3_v2: READ," in var_info
    assert "field_2_w3_v3: READ," in var_info
    assert "field_2_w3_v4: READ," in var_info


def test_lfric_stub_basis():
    '''Check variable usage detection of basis, diff-basis.

    '''
    ast = get_ast("lfric", "testkern_qr_eval_mod.F90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "basis_w1_on_w1: READ," in var_info
    assert "diff_basis_w2_qr_face: READ," in var_info
    assert "diff_basis_w2_on_w1: READ," in var_info
    assert "basis_w3_on_w1: READ," in var_info
    assert "diff_basis_w3_qr_face: READ," in var_info
    assert "diff_basis_w3_on_w1: READ," in var_info


def test_lfric_stub_cma_operators():
    '''Check variable usage detection cma operators.
    mesh_ncell2d, cma_operator

    '''
    ast = get_ast("lfric", "columnwise_op_mul_2scalars_kernel_mod.F90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    for num in ["1", "3", "5"]:
        assert "ncell_2d: READ," in var_info
        assert "cma_op_"+num+": READ," in var_info
        assert "cma_op_"+num+"_nrow: READ," in var_info
        assert "cma_op_"+num+"_ncol: READ," in var_info
        assert "cma_op_"+num+"_bandwidth: READ," in var_info
        assert "cma_op_"+num+"_alpha: READ," in var_info
        assert "cma_op_"+num+"_beta: READ," in var_info
        assert "cma_op_"+num+"_gamma_m: READ," in var_info
        assert "cma_op_"+num+"_gamma_p: READ" in var_info


def test_lfric_stub_banded_dofmap():
    '''Check variable usage detection for banded dofmaps.

    '''
    ast = get_ast("lfric", "columnwise_op_asm_kernel_mod.F90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "cbanded_map_ads1_op_1: READ," in var_info
    assert "cbanded_map_ads2_op_1: READ" in var_info


def test_lfric_stub_indirection_dofmap():
    '''Check variable usage detection in indirection dofmap.
    '''
    ast = get_ast("lfric", "columnwise_op_app_kernel_mod.F90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "cma_indirection_map_as1_field_1: READ," in var_info
    assert "cma_indirection_map_as2_field_2: READ" in var_info


def test_lfric_stub_boundary_dofmap():
    '''Check variable usage detection in boundary_dofs array fix
    for operators.

    '''
    ast = get_ast("lfric", "enforce_operator_bc_kernel_mod.F90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    var_accesses = VariablesAccessMap()
    create_arg_list = KernStubArgList(kernel)
    create_arg_list.generate(var_accesses=var_accesses)
    assert "boundary_dofs_op_1: READ" in str(var_accesses)
