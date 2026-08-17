# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Module containing pytest tests for the reference-element functionality
of the LFRic API.
'''

import pytest
from psyclone.configuration import Config
from psyclone.domain.lfric.kernel import LFRicPropertyMetadata
from psyclone.errors import InternalError
from psyclone.psyGen import Kern
from psyclone.psyir.symbols import DataSymbol
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke

# Constants
TEST_API = "lfric"

REF_ELEM_MDATA = '''
module testkern_refelem_mod
  type, extends(kernel_type) :: testkern_refelem_type
    type(arg_type), dimension(2) :: meta_args =      &
        (/ arg_type(gh_field, gh_real, gh_read, w1), &
           arg_type(gh_field, gh_real, gh_inc, w0) /)
    type(reference_element_data_type), dimension(3) ::               &
      meta_reference_element =                                       &
        (/ reference_element_data_type(outward_normals_to_faces),    &
           reference_element_data_type(normals_to_horizontal_faces), &
           reference_element_data_type(normals_to_vertical_faces) /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_refelem_code
  end type testkern_refelem_type
contains
  subroutine testkern_refelem_code(a, b)
  end subroutine testkern_refelem_code
end module testkern_refelem_mod
'''

# Tests for parsing the metadata


@pytest.fixture(scope="function", autouse=True)
def setup():
    '''Make sure that all tests here use LFRic as API.'''
    Config.get().api = "lfric"


def test_refelem_arglist_err():
    ''' Check that the KernCallArgList.ref_element_properties method raises
    the expected error if it encounters an unsupported property. '''
    psy, _ = get_invoke("23.1_ref_elem_invoke.f90", TEST_API,
                        dist_mem=False, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Get hold of the Kernel object
    kernels = sched.walk(Kern)
    kernel = kernels[0]
    # Break the list of ref-element properties required by the Kernel
    kernel._reference_element = LFRicPropertyMetadata(
        kernel.reference_element.properties + ("Not a property",))
    with pytest.raises(InternalError) as err:
        kernel.arguments.psyir_expressions()
    assert ("Unsupported reference-element property ('Not a property') found "
            "when generating arguments for kernel 'testkern_ref_elem_code'. "
            "Supported properties are: ['Property" in str(err.value))

# Tests for generating the PSy-layer code


def test_refelem_gen(tmpdir):
    ''' Basic test for code-generation for an invoke containing a single
    kernel requiring reference-element properties. '''
    psy, _ = get_invoke("23.1_ref_elem_invoke.f90", TEST_API,
                        dist_mem=False, idx=0)

    gen = str(psy.gen).lower()
    assert "use reference_element_mod, only : reference_element_type" in gen
    assert "integer(kind=i_def) :: nfaces_re_h" in gen
    assert "integer(kind=i_def) :: nfaces_re_v" in gen
    assert ("real(kind=r_def), allocatable, dimension(:,:) :: "
            "normals_to_horiz_faces" in gen)
    assert ("real(kind=r_def), allocatable, dimension(:,:) :: "
            "normals_to_vert_faces" in gen)
    assert ("class(reference_element_type), pointer :: reference_element "
            "=> null()" in gen)
    # We need a mesh object in order to get a reference_element object
    assert "mesh => f1_proxy%vspace%get_mesh()" in gen
    assert "reference_element => mesh%get_reference_element()" in gen
    assert ("nfaces_re_h = reference_element%get_number_horizontal_faces()"
            in gen)
    assert "nfaces_re_v = reference_element%get_number_vertical_faces()" in gen
    assert ("call reference_element%get_normals_to_horizontal_faces("
            "normals_to_horiz_faces)" in gen)
    assert ("call reference_element%get_normals_to_vertical_faces("
            "normals_to_vert_faces)" in gen)
    # The kernel call
    assert ("call testkern_ref_elem_code(nlayers_f1, a, f1_data, "
            "f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces)" in gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_duplicate_refelem_gen(tmpdir):
    ''' Test for code-generation for an invoke containing two kernels that
    require the same properties of the reference-element. '''
    psy, _ = get_invoke("23.2_multi_ref_elem_invoke.f90", TEST_API,
                        dist_mem=False, idx=0)

    gen = str(psy.gen).lower()
    assert gen.count(
        "real(kind=r_def), allocatable, dimension(:,:) :: "
        "normals_to_horiz_faces") == 1
    assert gen.count(
        "real(kind=r_def), allocatable, dimension(:,:) :: "
        "normals_to_vert_faces") == 1
    assert gen.count(
        "reference_element => mesh%get_reference_element") == 1
    assert gen.count(
        "nfaces_re_h = reference_element%get_number_horizontal_faces()") == 1
    assert gen.count(
        "nfaces_re_v = reference_element%get_number_vertical_faces()") == 1
    assert gen.count("call reference_element%get_normals_to_horizontal_faces("
                     "normals_to_horiz_faces)") == 1
    assert gen.count("call reference_element%get_normals_to_vertical_faces("
                     "normals_to_vert_faces)") == 1
    assert ("call testkern_ref_elem_code(nlayers_f1, a, f1_data, "
            "f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces)" in gen)
    assert ("call testkern_ref_elem_code(nlayers_f3, a, f3_data, "
            "f4_data, m3_data, m4_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces)" in gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_union_refelem_gen(tmpdir):
    ''' Check that code generation works for an invoke with kernels that
    only have a sub-set of reference-element properties in common. '''
    psy, _ = get_invoke("23.3_shared_ref_elem_invoke.f90", TEST_API,
                        dist_mem=False, idx=0)

    gen = str(psy.gen).lower()

    assert (
        "    reference_element => mesh%get_reference_element()\n"
        "    nfaces_re_h = reference_element%get_number_horizontal_faces()\n"
        "    nfaces_re_v = reference_element%get_number_vertical_faces()\n"
        "    call reference_element%get_normals_to_horizontal_faces("
        "normals_to_horiz_faces)\n"
        "    call reference_element%get_outward_normals_to_horizontal_faces("
        "out_normals_to_horiz_faces)\n"
        "    call reference_element%get_normals_to_vertical_faces("
        "normals_to_vert_faces)\n"
        "    call reference_element%get_outward_normals_to_vertical_faces("
        "out_normals_to_vert_faces)\n" in gen)
    assert ("call testkern_ref_elem_code(nlayers_f1, a, f1_data, "
            "f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3,"
            " map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces)" in gen)
    assert ("call testkern_ref_elem_out_code(nlayers_f3, a, f3_data, "
            "f4_data, m3_data, m4_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3,"
            " map_w3(:,cell), nfaces_re_v, nfaces_re_h, "
            "out_normals_to_vert_faces, normals_to_vert_faces, "
            "out_normals_to_horiz_faces)" in gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_all_faces_refelem_gen(tmpdir):
    ''' Test for code-generation for an invoke containing a single kernel
    requiring all faces of reference-element (also check that only one
    number of faces is passed to the kernel). '''
    psy, _ = get_invoke("23.4_ref_elem_all_faces_invoke.f90", TEST_API,
                        dist_mem=False, idx=0)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()

    assert (
        "    reference_element => mesh%get_reference_element()\n"
        "    nfaces_re = reference_element%get_number_faces()\n"
        "    call reference_element%get_normals_to_faces(normals_to_faces)\n"
        "    call reference_element%get_outward_normals_to_faces("
        "out_normals_to_faces)\n" in gen)
    assert ("call testkern_ref_elem_all_faces_code(nlayers_f1, a, f1_data, "
            "f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3,"
            " map_w3(:,cell), nfaces_re, out_normals_to_faces, "
            "normals_to_faces)" in gen)


def test_refelem_no_rdef(tmpdir):
    '''Check that the PSy-layer declares r_def if there is a reference
    element specified in the metadata but the arguments are not of
    type r_def. This is required as array arguments associated with the
    reference element have precision r_def. In this example there is a
    field of type r_solver.

    '''
    psy, _ = get_invoke("23.5_ref_elem_mixed_prec.f90", TEST_API,
                        dist_mem=False, idx=0)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()
    assert "use constants_mod" in gen


def test_ref_element_symbols():
    '''Tests that the correct set of symbols are returned.
    '''
    psy, _ = get_invoke("23.5_ref_elem_mixed_prec.f90",
                        TEST_API, dist_mem=False, idx=0)
    ref_element = psy.invokes.invoke_list[0].reference_element_properties
    args_symbols = ref_element.kern_args_symbols()
    args_str = ref_element.kern_args()
    assert args_str == [symbol.name for symbol in args_symbols]

    for symbol in args_symbols:
        assert isinstance(symbol, DataSymbol)
