# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory
# Modified: I. Kavcic and L. Turner, Met Office

'''
Module containing pytest tests for the mesh-property functionality
of the LFRic (Dynamo0.3) API.
'''

import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.domain.lfric import LFRicKernMetadata
from psyclone.dynamo0p3 import LFRicMeshProperties, MeshProperty
from psyclone.errors import InternalError
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory, Kern
from psyclone.tests.lfric_build import LFRicBuild


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

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
  subroutine testkern_mesh_code(a, b)
  end subroutine testkern_mesh_code
end module testkern_mesh_mod
'''

# Tests for parsing the metadata


def test_mdata_parse():
    ''' Check that we get the correct list of mesh properties. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = MESH_PROPS_MDATA
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_mesh_type"
    dkm = LFRicKernMetadata(ast, name=name)
    assert dkm.mesh.properties == [MeshProperty.ADJACENT_FACE]


@pytest.mark.parametrize("property_name", ["not_a_property", "ncell_2d"])
def test_mdata_invalid_property(property_name):
    ''' Check that we raise the expected error if an unrecognised mesh
    property is requested. Also test with a value that *is* a valid mesh
    property but is not supported in kernel metadata. '''
    code = MESH_PROPS_MDATA.replace("adjacent_face", property_name)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        LFRicKernMetadata(ast, name=name)
    assert (f"in metadata: '{property_name}'. Supported values are: "
            f"['ADJACENT_FACE'" in str(err.value))


def test_mdata_wrong_arg_count():
    ''' Check that we raise the expected error if the wrong dimension value
    is specified for the mesh_data_type array. '''
    code = MESH_PROPS_MDATA.replace("mesh_data_type), dimension(1)",
                                    "mesh_data_type), dimension(4)")
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        LFRicKernMetadata(ast, name=name)
    assert ("'meta_mesh' metadata, the number of items in the array "
            "constructor (1) does not match the extent of the array (4)"
            in str(err.value))


def test_mdata_wrong_name():
    ''' Check that we raise the expected error if the array holding properties
    of the mesh is given the wrong name. '''
    code = MESH_PROPS_MDATA.replace("meta_mesh =", "meta_meshes =")
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        LFRicKernMetadata(ast, name=name)
    assert ("No variable named 'meta_mesh' found in the metadata for"
            in str(err.value))


def test_mdata_wrong_type_var():
    ''' Check that we raise the expected error if the array holding properties
    of the mesh contains an item of the wrong type. '''
    code = MESH_PROPS_MDATA.replace(
        "mesh_data_type(adjacent_face",
        "ref_element_data_type(adjacent_face")
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        LFRicKernMetadata(ast, name=name)
    assert ("'meta_mesh' metadata must consist of an array of "
            "structure constructors, all of type 'mesh_data_type'"
            " but found: ['ref_element_data_type']" in str(err.value))


def test_mdata_duplicate_var():
    ''' Check that we raise the expected error if the array holding properties
    of the mesh contains a duplicate item. '''
    code = MESH_PROPS_MDATA.replace("mesh_data_type), dimension(1)",
                                    "mesh_data_type), dimension(2)")
    code = code.replace("adjacent_face) ",
                        "adjacent_face), mesh_data_type(adjacent_face) ")

    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_mesh_type"
    with pytest.raises(ParseError) as err:
        LFRicKernMetadata(ast, name=name)
    assert ("Duplicate mesh property found: "
            "'MeshProperty.ADJACENT_FACE'." in str(err.value))


def test_mesh_properties():
    ''' Tests the various checks in the methods of the LFRicMeshProperties
    class. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "24.1_mesh_prop_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    # Check that the kern_args() and _stub_declarations() methods raise the
    # expected error if the LFRicMeshProperties class has been created for
    # an Invoke.
    with pytest.raises(InternalError) as err:
        invoke.mesh_properties.kern_args()
    assert ("only be called when LFRicMeshProperties has been instantiated "
            "for a kernel" in str(err.value))
    with pytest.raises(InternalError) as err:
        invoke.mesh_properties._stub_declarations(None)
    assert ("cannot be called because LFRicMeshProperties has been "
            "instantiated for an invoke and not a kernel" in str(err.value))
    # Break the list of mesh properties
    invoke.mesh_properties._properties.append("not-a-property")
    with pytest.raises(InternalError) as err:
        invoke.mesh_properties._invoke_declarations(ModuleGen("test_mod"))
    assert ("Found unsupported mesh property 'not-a-property' when "
            "generating invoke declarations. Only " in str(err.value))
    with pytest.raises(InternalError) as err:
        invoke.mesh_properties.initialise(ModuleGen("test_mod"))
    assert ("Found unsupported mesh property 'not-a-property' when generating"
            " initialisation code" in str(err.value))
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
        mesh_props._invoke_declarations(ModuleGen("test_mod"))
    assert ("cannot be called because LFRicMeshProperties has been "
            "instantiated for a kernel and not an invoke." in str(err.value))
    with pytest.raises(InternalError) as err:
        mesh_props._stub_declarations(ModuleGen("test_mod"))
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
    assert "use reference_element_mod, only: reference_element_type" in gen
    assert "integer(kind=i_def) nfaces_re_h" in gen
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
    assert ("call testkern_mesh_prop_code(nlayers, a, f1_data, "
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
    assert ("call testkern_mesh_prop_code(nlayers, a, f1_data, "
            "ndf_w1, undf_w1, map_w1(:,cell), nfaces_re_h, "
            "adjacent_face(:,cell)" in gen)
    assert ("call testkern_mesh_prop_code(nlayers, b, f2_data, "
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
        "      reference_element => mesh%get_reference_element()\n"
        "      nfaces_re_h = reference_element%get_number_horizontal_faces()\n"
        "      nfaces_re_v = reference_element%get_number_vertical_faces()\n"
        "      call reference_element%get_normals_to_horizontal_faces("
        "normals_to_horiz_faces)\n"
        "      call reference_element%get_normals_to_vertical_faces("
        "normals_to_vert_faces)\n" in gen)
    assert ("call testkern_mesh_ref_elem_props_code(nlayers, a, "
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

    assert ("      qr_proxy = qr%get_quadrature_proxy()\n"
            "      np_xyz_qr = qr_proxy%np_xyz\n"
            "      nfaces_qr = qr_proxy%nfaces\n"
            "      weights_xyz_qr => qr_proxy%weights_xyz\n"
            "      !\n"
            "      ! allocate basis/diff-basis arrays\n"
            "      !\n"
            "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
            "      allocate (basis_w1_qr(dim_w1, ndf_w1, np_xyz_qr, "
            "nfaces_qr))" in gen)

    assert ("      reference_element => mesh%get_reference_element()\n"
            "      nfaces_re_h = reference_element%"
            "get_number_horizontal_faces()\n"
            "      !\n"
            "      ! initialise mesh properties\n"
            "      !\n"
            "      adjacent_face => mesh%get_adjacent_face()" in gen)

    assert ("call testkern_mesh_prop_face_qr_code(nlayers, a, f1_data, "
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
    assert (
        "      real(kind=r_def), pointer :: weights_xyz_qr(:,:) => null()\n"
        "      integer(kind=i_def) np_xyz_qr, nfaces_qr\n"
        "      integer(kind=i_def), pointer :: adjacent_face(:,:) => null()\n"
        "      real(kind=r_def), allocatable :: normals_to_horiz_faces(:,:), "
        "normals_to_vert_faces(:,:)\n"
        "      integer(kind=i_def) nfaces_re_h, nfaces_re_v\n"
        "      class(reference_element_type), pointer :: reference_element => "
        "null()\n" in gen)
    # Initialisations
    assert "type(mesh_type), pointer :: mesh => null()" in gen
    assert "nfaces_qr = qr_proxy%nfaces" in gen
    assert (
        "      reference_element => mesh%get_reference_element()\n"
        "      nfaces_re_h = reference_element%get_number_horizontal_faces()\n"
        "      nfaces_re_v = reference_element%get_number_vertical_faces()"
        in gen)
    assert "adjacent_face => mesh%get_adjacent_face()" in gen
    # Call to kernel requiring props of the reference element & adjacent faces
    assert ("call testkern_mesh_ref_elem_props_code(nlayers, a, f1_data,"
            " ndf_w1, undf_w1, map_w1(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces, "
            "adjacent_face(:,cell))" in gen)
    # Call to kernel requiring adjacent faces and face quadrature
    assert ("call testkern_mesh_prop_face_qr_code(nlayers, a, f2_data, "
            "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr, nfaces_re_h, "
            "adjacent_face(:,cell), nfaces_qr, np_xyz_qr, weights_xyz_qr)"
            in gen)
