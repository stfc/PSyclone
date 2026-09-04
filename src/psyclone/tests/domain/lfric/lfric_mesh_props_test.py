# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Module containing pytest tests for the mesh-property functionality
of the LFRic API.
'''

import os
import pytest
import fparser
from psyclone.lfric import LFRicMeshProperties
from psyclone.errors import InternalError
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory, Kern
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import create_lfric_metadata


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"

MESH_PROPS_MDATA = '''
module testkern_mesh_mod
  type, extends(kernel_type) :: testkern_mesh_type
    type(arg_type), dimension(2) :: meta_args =      &
        (/ arg_type(gh_field, gh_real, gh_read, w1), &
           arg_type(gh_field, gh_real, gh_inc,  w0) /)
    type(mesh_data_type), dimension(1) :: meta_mesh = &
        (/ mesh_data_type(adjacent_face) /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_mesh_code
  end type testkern_mesh_type
contains
  subroutine testkern_mesh_code()
end subroutine testkern_mesh_code
end module testkern_mesh_mod
'''

# Tests for parsing the metadata


def test_mdata_parse(fortran_reader):
    ''' Check that we get the correct list of mesh properties. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = MESH_PROPS_MDATA
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_mesh_type"
    dkm = create_lfric_metadata(psyir, name=name)
    assert len(dkm.meta_mesh) == 1
    assert dkm.meta_mesh[0].mesh == "adjacent_face"


@pytest.mark.parametrize("property_name", ["not_a_property", "ncell_2d"])
def test_mdata_invalid_property(property_name, fortran_reader):
    ''' Check that we raise the expected error if an unrecognised mesh
    property is requested. Also test with a value that *is* a valid mesh
    property but is not supported in kernel metadata. '''
    code = MESH_PROPS_MDATA.replace("adjacent_face", property_name)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name=name)
    assert "Expected mesh property to be one of" in str(err.value)
    assert f"'{property_name}'" in str(err.value)


def test_mdata_wrong_arg_count(fortran_reader):
    ''' Check that we raise the expected error if the wrong dimension value
    is specified for the mesh_data_type array. '''
    code = MESH_PROPS_MDATA.replace("mesh_data_type), dimension(1)",
                                    "mesh_data_type), dimension(4)")
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name=name)
    assert ("Metadata component 'meta_mesh' has extent 4 but its constructor "
            "contains 1 values" in str(err.value))


def test_mdata_wrong_name(fortran_reader):
    ''' Check that we raise the expected error if the array holding properties
    of the mesh is given the wrong name. '''
    code = MESH_PROPS_MDATA.replace("meta_mesh =", "meta_meshes =")
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name=name)
    assert "Unexpected LFRic metadata component(s): ['meta_meshes']" in str(
        err.value)


def test_mdata_wrong_type_var(fortran_reader):
    ''' Check that we raise the expected error if the array holding properties
    of the mesh contains an item of the wrong type. '''
    code = MESH_PROPS_MDATA.replace(
        "mesh_data_type(adjacent_face",
        "ref_element_data_type(adjacent_face")
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name=name)
    assert "meta_mesh entries must use the mesh_data_type constructor" in str(
        err.value)


def test_mdata_duplicate_var(fortran_reader):
    ''' Check that we raise the expected error if the array holding properties
    of the mesh contains a duplicate item. '''
    code = MESH_PROPS_MDATA.replace("mesh_data_type), dimension(1)",
                                    "mesh_data_type), dimension(2)")
    code = code.replace("adjacent_face) ",
                        "adjacent_face), mesh_data_type(adjacent_face) ")

    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name=name)
    assert "meta_mesh must not contain duplicates" in str(err.value)


def test_mesh_properties():
    ''' Tests the various checks in the methods of the LFRicMeshProperties
    class. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "24.1_mesh_prop_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    # Check that the kern_args() and stub_declarations() methods raise the
    # expected error if the LFRicMeshProperties class has been created for
    # an Invoke.
    with pytest.raises(InternalError) as err:
        invoke.mesh_properties.kern_args()
    assert ("only be called when LFRicMeshProperties has been instantiated "
            "for a kernel" in str(err.value))
    with pytest.raises(InternalError) as err:
        invoke.mesh_properties.stub_declarations()
    assert ("stub_declarations() can only be called with an "
            "LFRicMeshProperties instantiated for a kernel "
            "(not an invoke)." in str(err.value))
    # Break the list of mesh properties
    invoke.mesh_properties._properties.append("not-a-property")
    with pytest.raises(InternalError) as err:
        invoke.mesh_properties.invoke_declarations()
    assert ("Found unsupported mesh property 'not-a-property' when "
            "generating invoke declarations. Only " in str(err.value))
    sched = invoke.schedule
    # Get hold of the Kernel object
    kernel = sched.walk(Kern)[0]
    # Create an LFRicMeshProperties object just for this kernel
    mesh_props = LFRicMeshProperties(kernel)
    args = mesh_props.kern_args()
    # Check correct operation
    assert args == ["nfaces_re_h", "adjacent_face(:,cell)"]
    # Break the list of mesh properties
    mesh_props._properties.append("not-a-property")
    with pytest.raises(InternalError) as err:
        mesh_props.kern_args()
    assert ("found unsupported mesh property 'not-a-property' when "
            "generating arguments for kernel 'testkern_mesh_prop_code'. "
            "Only members of the MeshProperty Enum are"
            in str(err.value))
    with pytest.raises(InternalError) as err:
        mesh_props.invoke_declarations()
    assert ("invoke_declarations() can only be called with an LFRicMesh"
            "Properties instantiated for an invoke (not a kernel)."
            in str(err.value))
    with pytest.raises(InternalError) as err:
        mesh_props.stub_declarations()
    assert ("Found unsupported mesh property 'not-a-property' when "
            "generating declarations for kernel stub. Only " in str(err.value))


# Tests for generating the PSy-layer code


def test_mesh_gen(tmpdir):
    ''' Basic test for code-generation for an invoke containing a single
    kernel requiring reference-element properties. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "24.1_mesh_prop_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()
    # In order to provide the mesh property we need the reference element
    assert "use reference_element_mod, only : reference_element_type" in gen
    assert "integer(kind=i_def) :: nfaces_re_h" in gen
    assert ("integer(kind=i_def), pointer :: adjacent_face(:,:) => null()"
            in gen)
    assert ("class(reference_element_type), pointer :: reference_element "
            "=> null()" in gen)
    # We need a mesh object in order to get a reference_element object
    assert "mesh => f1_proxy%vspace%get_mesh()" in gen
    assert "reference_element => mesh%get_reference_element()" in gen
    assert ("nfaces_re_h = reference_element%get_number_horizontal_faces()"
            in gen)
    assert "adjacent_face => mesh%get_adjacent_face()" in gen
    assert "nfaces_re_v" not in gen
    # The kernel call
    assert ("call testkern_mesh_prop_code(nlayers_f1, a, f1_data, "
            "ndf_w1, undf_w1, map_w1(:,cell), nfaces_re_h, "
            "adjacent_face(:,cell))" in gen)


def test_duplicate_mesh_gen(tmpdir):
    ''' Test for code-generation for an invoke containing two kernels that
    require the same property of the mesh. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "24.2_duplicate_mesh_prop_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()
    assert gen.count(
        "integer(kind=i_def), pointer :: adjacent_face(:,:) => null()") == 1
    assert gen.count(
        "reference_element => mesh%get_reference_element") == 1
    assert gen.count(
        "nfaces_re_h = reference_element%get_number_horizontal_faces()") == 1
    assert "nfaces_re_v" not in gen
    assert gen.count("adjacent_face => mesh%get_adjacent_face()") == 1
    assert ("call testkern_mesh_prop_code(nlayers_f1, a, f1_data, "
            "ndf_w1, undf_w1, map_w1(:,cell), nfaces_re_h, "
            "adjacent_face(:,cell)" in gen)
    assert ("call testkern_mesh_prop_code(nlayers_f2, b, f2_data, "
            "ndf_w1, undf_w1, map_w1(:,cell), nfaces_re_h, "
            "adjacent_face(:,cell))" in gen)


def test_mesh_prop_plus_ref_elem_gen(tmpdir):
    ''' Check that code generation works for an invoke with a kernel that
    requires properties of both the reference element and the mesh. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "24.3_mesh_plus_ref_elem_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()

    assert (
        "    reference_element => mesh%get_reference_element()\n"
        "    nfaces_re_h = reference_element%get_number_horizontal_faces()\n"
        "    nfaces_re_v = reference_element%get_number_vertical_faces()\n"
        "    call reference_element%get_normals_to_horizontal_faces("
        "normals_to_horiz_faces)\n"
        "    call reference_element%get_normals_to_vertical_faces("
        "normals_to_vert_faces)\n" in gen)
    assert ("call testkern_mesh_ref_elem_props_code(nlayers_f1, a, "
            "f1_data, ndf_w1, undf_w1, map_w1(:,cell), nfaces_re_h, "
            "nfaces_re_v, normals_to_horiz_faces, normals_to_vert_faces, "
            "adjacent_face(:,cell))" in gen)


def test_mesh_plus_face_quad_gen(tmpdir):
    ''' Test that we generate correct code when a kernel requires both a
    mesh property and face quadrature. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "24.4_mesh_plus_face_qr_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()

    assert ("    qr_proxy = qr%get_quadrature_proxy()\n"
            "    np_xyz_qr = qr_proxy%np_xyz\n"
            "    nfaces_qr = qr_proxy%nfaces\n"
            "    weights_xyz_qr => qr_proxy%weights_xyz\n"
            "\n"
            "    ! allocate basis/diff-basis arrays\n"
            "    dim_w1 = f1_proxy%vspace%get_dim_space()\n"
            "    allocate(basis_w1_qr(dim_w1,ndf_w1,np_xyz_qr,"
            "nfaces_qr))" in gen)

    assert ("    reference_element => mesh%get_reference_element()\n"
            "    nfaces_re_h = reference_element%"
            "get_number_horizontal_faces()\n"
            "\n"
            "    ! initialise mesh properties\n"
            "    adjacent_face => mesh%get_adjacent_face()" in gen)

    assert ("call testkern_mesh_prop_face_qr_code(nlayers_f1, a, f1_data, "
            "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr, "
            "nfaces_re_h, adjacent_face(:,cell), "
            "nfaces_qr, np_xyz_qr, weights_xyz_qr)" in gen)


def test_multi_kernel_mesh_props(tmpdir):
    ''' Test code generation when an invoke contains multiple kernels that
    require properties of the mesh, the reference element and face quadrature.
    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "24.5_mesh_plus_ref_elem_plus_qr_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()

    # Declarations
    assert ("real(kind=r_def), pointer, dimension(:,:) :: weights_xyz_qr => "
            "null()\n") in gen
    assert "integer(kind=i_def) :: np_xyz_qr\n" in gen
    assert "integer(kind=i_def) :: nfaces_qr\n" in gen
    assert ("integer(kind=i_def), pointer :: adjacent_face(:,:) => null()\n"
            in gen)
    assert ("real(kind=r_def), allocatable, dimension(:,:) :: "
            "normals_to_horiz_faces" in gen)
    assert ("real(kind=r_def), allocatable, dimension(:,:) :: "
            "normals_to_vert_faces" in gen)
    assert "integer(kind=i_def) :: nfaces_re_h\n" in gen
    assert "integer(kind=i_def) :: nfaces_re_v\n" in gen
    assert ("class(reference_element_type), pointer :: reference_element => "
            "null()\n" in gen)
    # Initialisations
    assert "type(mesh_type), pointer :: mesh => null()" in gen
    assert "nfaces_qr = qr_proxy%nfaces" in gen
    assert (
        "    reference_element => mesh%get_reference_element()\n"
        "    nfaces_re_h = reference_element%get_number_horizontal_faces()\n"
        "    nfaces_re_v = reference_element%get_number_vertical_faces()"
        in gen)
    assert "adjacent_face => mesh%get_adjacent_face()" in gen
    # Call to kernel requiring props of the reference element & adjacent faces
    assert ("call testkern_mesh_ref_elem_props_code(nlayers_f1, a, f1_data,"
            " ndf_w1, undf_w1, map_w1(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces, "
            "adjacent_face(:,cell))" in gen)
    # Call to kernel requiring adjacent faces and face quadrature
    assert ("call testkern_mesh_prop_face_qr_code(nlayers_f2, a, f2_data, "
            "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr, nfaces_re_h, "
            "adjacent_face(:,cell), nfaces_qr, np_xyz_qr, weights_xyz_qr)"
            in gen)
