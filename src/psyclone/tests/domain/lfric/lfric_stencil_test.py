# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
#         I. Kavcic, A. Coughtrie and L. Turner, Met Office,
#         C. M. Maynard, Met Office/University of Reading,
#         J. Henrichs, Bureau of Meteorology.

''' Module containing tests of LFRic stencils through the LFRic API '''

import os

import pytest
import fparser
from fparser import api as fpapi

from psyclone.domain.lfric import (LFRicConstants, LFRicKern,
                                   LFRicKernMetadata, LFRicStencils)
from psyclone.dynamo0p3 import DynKernelArguments
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.tests.lfric_build import LFRicBuild


# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../..", "test_files", "dynamo0p3")

TEST_API = "dynamo0.3"

STENCIL_CODE = '''
module stencil_mod
  type, extends(kernel_type) :: stencil_type
     type(arg_type), meta_args(2) =                                   &
          (/ arg_type(gh_field, gh_real, gh_inc,  w1),                &
             arg_type(gh_field, gh_real, gh_read, w2, stencil(cross)) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => stencil_code
  end type stencil_type
contains
  subroutine stencil_code()
  end subroutine stencil_code
end module stencil_mod
'''


def test_stencil_metadata():
    ''' Check that we can parse Kernels with stencil metadata. '''
    ast = fpapi.parse(STENCIL_CODE, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)

    stencil_descriptor_0 = metadata.arg_descriptors[0]
    assert stencil_descriptor_0.stencil is None
    stencil_descriptor_1 = metadata.arg_descriptors[1]
    assert stencil_descriptor_1.stencil['type'] == 'cross'
    # stencil extent is not provided in the above metadata
    assert stencil_descriptor_1.stencil['extent'] is None

    # Check other LFRicArgDescriptor argument properties for a
    # field stencil argument
    assert stencil_descriptor_1.argument_type == "gh_field"
    assert stencil_descriptor_1.data_type == "gh_real"
    assert stencil_descriptor_1.function_space == "w2"
    assert stencil_descriptor_1.function_spaces == ['w2']
    assert str(stencil_descriptor_1.access) == "READ"
    assert stencil_descriptor_1.mesh is None
    assert stencil_descriptor_1.vector_size == 1


def test_stencil_field_metadata_too_many_arguments():
    ''' Check that we raise an exception if more than 5 arguments
    are provided in the metadata for a 'gh_field' argument type
    with stencil access.

    '''
    result = STENCIL_CODE.replace(
        "(gh_field, gh_real, gh_read, w2, stencil(cross))",
        "(gh_field, gh_real, gh_read, w2, stencil(cross), w1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert ("each 'meta_arg' entry must have at most 5 arguments" in
            str(excinfo.value))


def test_unsupported_second_argument():
    '''Check that we raise an exception if stencil extent is specified, as
    we do not currently support it'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d,1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(NotImplementedError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "Kernels with fixed stencil extents are not currently supported" \
        in str(excinfo.value)


def test_valid_stencil_types():
    ''' Check that we successfully parse all valid stencil types. '''
    const = LFRicConstants()
    for stencil_type in const.VALID_STENCIL_TYPES:
        result = STENCIL_CODE.replace("stencil(cross)",
                                      "stencil(" + stencil_type + ")", 1)
        ast = fpapi.parse(result, ignore_comments=False)
        _ = LFRicKernMetadata(ast)


def test_stencil_read_only():
    ''' Test that an error is raised if a field with a stencil is not
    accessed as 'gh_read'. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = STENCIL_CODE.replace("gh_read, w2, stencil(cross)",
                                "gh_inc, w2, stencil(cross)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name="stencil_type")
    assert ("In the LFRic API a field with a stencil access must be "
            "read-only ('gh_read'), but found 'gh_inc'" in
            str(excinfo.value))


def test_stencil_field_arg_lfricconst_properties(monkeypatch):
    ''' Tests that properties of all supported types of field arguments
    with stencil access ('real'-valued 'field_type' and 'integer'-valued
    'integer_field_type') defined in LFRicConstants are correctly set up
    in the DynKernelArgument class.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "stencil_type"

    # Test 'real'-valued field of 'field_type' with stencil access
    ast = fpapi.parse(STENCIL_CODE, ignore_comments=False)
    metadata = LFRicKernMetadata(ast, name=name)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    stencil_arg = kernel.arguments.args[1]
    assert stencil_arg.module_name == "field_mod"
    assert stencil_arg.data_type == "field_type"
    assert stencil_arg.proxy_data_type == "field_proxy_type"
    assert stencil_arg.intrinsic_type == "real"
    assert stencil_arg.precision == "r_def"

    # Test 'integer'-valued fields of 'integer_field_type' with
    # stencil access
    code = STENCIL_CODE.replace("gh_field, gh_real",
                                "gh_field, gh_integer")
    ast = fpapi.parse(code, ignore_comments=False)
    metadata = LFRicKernMetadata(ast, name=name)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    stencil_arg = kernel.arguments.args[1]
    assert stencil_arg.module_name == "integer_field_mod"
    assert stencil_arg.data_type == "integer_field_type"
    assert stencil_arg.proxy_data_type == "integer_field_proxy_type"
    assert stencil_arg.intrinsic_type == "integer"
    assert stencil_arg.precision == "i_def"

    # Monkeypatch to check with an invalid intrinsic type of a
    # field stencil argument
    const = LFRicConstants()
    monkeypatch.setattr(stencil_arg, "_intrinsic_type", "tortoiseshell")
    with pytest.raises(InternalError) as err:
        stencil_arg._init_data_type_properties(None, False)
    assert (f"Expected one of {const.VALID_FIELD_INTRINSIC_TYPES} intrinsic "
            f"types for a field argument but found 'tortoiseshell'." in
            str(err.value))


def test_single_kernel_any_dscnt_space_stencil(dist_mem, tmpdir):
    ''' Tests for stencils and any_discontinuous_space space within
    a single kernel and between kernels. We test when
    any_discontinuous_space is the same and when it is different
    within kernels and between kernels for the case of different fields.
    When it is the same we should have the same stencil dofmap
    (as all other stencil information is the same) and when it is
    different we should have a different stencil dofmap (as we do not
    know whether they are on the same space).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.24_any_discontinuous_space_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    # Check compilation
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Use the same stencil dofmap
    output1 = (
        "        CALL testkern_same_any_dscnt_space_stencil_code("
        "nlayers, f0_data, f1_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), f2_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), ndf_wtheta, undf_wtheta, "
        "map_wtheta(:,cell), ndf_adspc1_f1, undf_adspc1_f1, "
        "map_adspc1_f1(:,cell))")
    assert output1 in result
    # Use a different stencil dofmap
    output2 = (
        "        CALL testkern_different_any_dscnt_space_stencil_code("
        "nlayers, f3_data, f4_data, f4_stencil_size(cell), "
        "f4_stencil_dofmap(:,:,cell), f5_data, f5_stencil_size(cell), "
        "f5_stencil_dofmap(:,:,cell), ndf_wtheta, undf_wtheta, "
        "map_wtheta(:,cell), ndf_adspc1_f4, "
        "undf_adspc1_f4, map_adspc1_f4(:,cell), "
        "ndf_adspc2_f5, undf_adspc2_f5, map_adspc2_f5(:,cell))")
    assert output2 in result
    # Check for halo exchanges and correct loop bounds
    if dist_mem:
        assert result.count("_proxy%halo_exchange(depth=extent)") == 4
        assert "loop0_stop = mesh%get_last_edge_cell()" in result
        assert "loop1_stop = mesh%get_last_edge_cell()" in result
    else:
        assert "halo_exchange(depth=extent)" not in result
        assert "loop0_stop = f0_proxy%vspace%get_ncell()" in result
        assert "loop1_stop = f3_proxy%vspace%get_ncell()" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    assert "DO cell=loop1_start,loop1_stop" in result


def test_stencil_args_unique_1(dist_mem, tmpdir):
    ''' This test checks that stencil extent and direction arguments do not
    clash with internal names generated in the PSy-layer. f2_stencil_size
    and nlayers are chosen as the names that would clash.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.21_stencil_names_clash.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # we use f2_stencil_size for extent and nlayers for direction
    # as arguments
    output1 = ("    SUBROUTINE invoke_0_testkern_stencil_xory1d_type(f1, "
               "f2, f3, f4, f2_stencil_size, nlayers)")
    assert output1 in result
    output2 = ("      INTEGER(KIND=i_def), intent(in) :: f2_stencil_size\n"
               "      INTEGER(KIND=i_def), intent(in) :: nlayers")
    assert output2 in result
    output3 = ("      INTEGER(KIND=i_def), pointer :: f2_stencil_size_1(:)"
               " => null()")
    assert output3 in result
    # therefore the local variable is now declared as nlayers_1"
    output4 = "      INTEGER(KIND=i_def) nlayers_1"
    assert output4 in result
    output5 = "      nlayers_1 = f1_proxy%vspace%get_nlayers()"
    assert output5 in result
    output6 = (
        "      IF (nlayers .eq. x_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,f2_stencil_size)\n"
        "      END IF\n"
        "      IF (nlayers .eq. y_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,f2_stencil_size)\n"
        "      END IF\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size_1 => f2_stencil_map%get_stencil_sizes()")
    assert output6 in result
    output7 = (
        "        CALL testkern_stencil_xory1d_code(nlayers_1, "
        "f1_data, f2_data, f2_stencil_size_1(cell), nlayers, "
        "f2_stencil_dofmap(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
    assert output7 in result


def test_stencil_args_unique_2(dist_mem, tmpdir):
    '''This test checks that stencil extent and direction arguments are
    unique within the generated PSy-layer when they are accessed as
    indexed arrays, with the same array name, from the algorithm
    layer.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.22_stencil_names_indexed.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = ("    SUBROUTINE invoke_0(f1, f2, f3, f4, f2_info, "
               "f2_info_2, f2_info_1, f2_info_3)")
    assert output1 in result
    output2 = (
        "      INTEGER(KIND=i_def), intent(in) :: f2_info, f2_info_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: f2_info_1, f2_info_3")
    assert output2 in result
    output3 = (
        "      IF (f2_info_1 .eq. x_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,f2_info)\n"
        "      END IF\n"
        "      IF (f2_info_1 .eq. y_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,f2_info)\n"
        "      END IF\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      IF (f2_info_3 .eq. x_direction) THEN\n"
        "        f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,f2_info_2)\n"
        "      END IF\n"
        "      IF (f2_info_3 .eq. y_direction) THEN\n"
        "        f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,f2_info_2)\n"
        "      END IF")
    assert output3 in result
    output4 = (
        "        CALL testkern_stencil_xory1d_code(nlayers, "
        "f1_data, f2_data, f2_stencil_size(cell), f2_info_1, "
        "f2_stencil_dofmap(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
    assert output4 in result
    output5 = (
        "        CALL testkern_stencil_xory1d_code(nlayers, "
        "f1_data, f2_data, f2_stencil_size_1(cell), f2_info_3, "
        "f2_stencil_dofmap_1(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
    assert output5 in result
    if dist_mem:
        assert (
            "IF (f2_proxy%is_dirty(depth=max(f2_info+1,"
            "f2_info_2+1))) THEN" in result)
        assert (
            "CALL f2_proxy%halo_exchange(depth=max(f2_info+1,"
            "f2_info_2+1))" in result)
        assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
        assert "CALL f3_proxy%halo_exchange(depth=1)" in result
        assert "IF (f4_proxy%is_dirty(depth=1)) THEN" in result
        assert "CALL f4_proxy%halo_exchange(depth=1)" in result


def test_stencil_args_unique_3(dist_mem, tmpdir):
    ''' This test checks that stencil extent and direction arguments are
    unique within the generated PSy-layer when they are dereferenced,
    with the same type/class name, from the algorithm layer.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.23_stencil_names_deref.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert (
        "      INTEGER(KIND=i_def), intent(in) :: my_info_f2_info, "
        "my_info_f2_info_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: my_info_f2_info_1, "
        "my_info_f2_info_3\n"
        in result)
    assert (
        "f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap(STENCIL_1DX,"
        "my_info_f2_info)" in result)
    if dist_mem:
        assert (
            "IF (f2_proxy%is_dirty(depth=max(my_info_f2_info+1,"
            "my_info_f2_info_2+1))) THEN" in result)
        assert (
            "CALL f2_proxy%halo_exchange(depth=max(my_info_f2_info+1,"
            "my_info_f2_info_2+1))" in result)
        assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
        assert "CALL f3_proxy%halo_exchange(depth=1)" in result
        assert "IF (f4_proxy%is_dirty(depth=1)) THEN" in result
        assert "CALL f4_proxy%halo_exchange(depth=1)" in result


def test_stencil_vector(dist_mem, tmpdir):
    ''' Test that the expected declarations and lookups are produced when
    we have a stencil access with a vector field. Any stencil could be
    chosen here (other than xory1d) as they all produce the same code
    structure, but STENCIL_CROSS is chosen as it is already used in an
    existing suitable example (14.4).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "14.4_halo_vector.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)
    assert (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n") \
        in str(result)
    assert (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:)"
        " => null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n") \
        in str(result)
    assert (
        "      f2_stencil_map => f2_proxy(1)%vspace%get_stencil_dofmap"
        "(STENCIL_CROSS,f2_extent)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        ) in str(result)
    assert (
        "f2_1_data, f2_2_data, f2_3_data, "
        "f2_4_data, f2_stencil_size(cell), "
        "f2_stencil_dofmap(:,:,cell)") in str(result)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_stencil_xory_vector(dist_mem, tmpdir):
    '''Test that the expected declarations and lookups are produced when
    we have a stencil access of type x or y with a vector field.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "14.4.2_halo_vector_xory.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)
    assert (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
        "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n") \
        in result
    assert (
        "      INTEGER(KIND=i_def), intent(in) :: f2_extent\n"
        "      INTEGER(KIND=i_def), intent(in) :: f2_direction\n") \
        in result
    assert (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:)"
        " => null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n") \
        in result
    assert (
        "      IF (f2_direction .eq. x_direction) THEN\n"
        "        f2_stencil_map => "
        "f2_proxy(1)%vspace%get_stencil_dofmap"
        "(STENCIL_1DX,f2_extent)\n"
        "      END IF\n"
        "      IF (f2_direction .eq. y_direction) THEN\n"
        "        f2_stencil_map => "
        "f2_proxy(1)%vspace%get_stencil_dofmap"
        "(STENCIL_1DY,f2_extent)\n"
        "      END IF\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        ) in result
    assert (
        "f2_1_data, f2_2_data, f2_3_data, "
        "f2_4_data, f2_stencil_size(cell), f2_direction, "
        "f2_stencil_dofmap(:,:,cell)") \
        in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_invalid_stencil_form_1():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by being a literal integer or
    just "stencil" '''
    result = STENCIL_CODE.replace("stencil(cross)", "1", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
           in str(excinfo.value)
    assert "Unrecognised metadata entry" in str(excinfo.value)
    result = STENCIL_CODE.replace("stencil(cross)", "stencil", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
           in str(excinfo.value)
    assert "Expecting format stencil(<type>[,<extent>]) but found stencil" \
           in str(excinfo.value)


def test_invalid_stencil_form_2():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by having an invalid name'''
    result = STENCIL_CODE.replace("stencil(cross)", "stenci(cross)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
           in str(excinfo.value)


def test_invalid_stencil_form_3():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by not having brackets'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
           in str(excinfo.value)


def test_invalid_stencil_form_4():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by containing no values in
    the brackets '''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil()", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "but found stencil()" in str(excinfo.value)


def test_invalid_stencil_form_5():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by containing no values in
    the brackets, with a separator '''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(,)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "Kernel metadata has an invalid format" \
           in str(excinfo.value)


def test_invalid_stencil_form_6():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by containing more than two
    values in in the brackets '''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(cross,1,1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
           in str(excinfo.value)
    assert "there must be at most two arguments inside the brackets" \
           in str(excinfo.value)


def test_invalid_stencil_first_arg_1():
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>[,<extent>]) is not valid and is an integer'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "not one of the valid types" in str(excinfo.value)
    assert "is a literal" in str(excinfo.value)


def test_invalid_stencil_first_arg_2():
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>[,<extent>]) is not valid and is a name'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(cros)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "not one of the valid types" in str(excinfo.value)


def test_invalid_stencil_first_arg_3():
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>[,<extent>]) is not valid and has brackets'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d(xx))", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "the specified <type>" in str(excinfo.value)
    assert "includes brackets" in str(excinfo.value)


def test_invalid_stencil_second_arg_1():
    '''Check that we raise an exception if the value of the stencil extent in
    stencil(<type>[,<extent>]) is not an integer'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d,x1d)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "the specified <extent>" in str(excinfo.value)
    assert "is not an integer" in str(excinfo.value)


def test_invalid_stencil_second_arg_2():
    '''Check that we raise an exception if the value of the stencil extent in
    stencil(<type>[,<extent>]) is less than 1'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d,0)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast)
    assert "the specified <extent>" in str(excinfo.value)
    assert "is less than 1" in str(excinfo.value)


def test_single_stencil_extent(dist_mem, tmpdir):
    ''' Test a single stencil access with an extent value passed from the
    algorithm layer is treated correctly in the PSy layer. Test both
    sequential and distributed memory.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "SUBROUTINE invoke_0_testkern_stencil_type(f1, f2, f3, f4, "
        "f2_extent)")
    assert output1 in result
    assert "USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n" in result
    assert "USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n" in result
    output3 = ("      INTEGER(KIND=i_def), intent(in) :: f2_extent\n")
    assert output3 in result
    output4 = (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output4 in result
    output5 = (
        "      !\n"
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,f2_extent)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output5 in result
    output6 = (
        "        CALL testkern_stencil_code(nlayers, f1_data,"
        " f2_data, f2_stencil_size(cell), f2_stencil_dofmap(:,:,cell),"
        " f3_data, f4_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))")
    assert output6 in result


def test_single_stencil_xory1d(dist_mem, tmpdir):
    ''' Test a single stencil access with an extent and direction value
    passed from the algorithm layer is treated correctly in the PSy
    layer. Test both sequential and distributed memory.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.3_single_stencil_xory1d.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "    SUBROUTINE invoke_0_testkern_stencil_xory1d_type(f1, f2, f3, "
        "f4, f2_extent, f2_direction)")
    assert output1 in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
        "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), intent(in) :: f2_extent\n"
        "      INTEGER(KIND=i_def), intent(in) :: f2_direction\n")
    assert output3 in result
    output4 = (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output4 in result
    output5 = (
        "      !\n"
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      IF (f2_direction .eq. x_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,f2_extent)\n"
        "      END IF\n"
        "      IF (f2_direction .eq. y_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,f2_extent)\n"
        "      END IF\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output5 in result
    output6 = (
        "        CALL testkern_stencil_xory1d_code(nlayers, "
        "f1_data, f2_data, f2_stencil_size(cell), f2_direction, "
        "f2_stencil_dofmap(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
    assert output6 in result


def test_single_stencil_literal(dist_mem, tmpdir):
    ''' Test extent value is used correctly from the algorithm layer when
    it is a literal value so is not passed by argument. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.4_single_stencil_literal.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = ("    SUBROUTINE invoke_0_testkern_stencil_type(f1, f2, "
               "f3, f4)")
    assert output1 in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output3 in result
    output4 = (
        "      !\n"
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,1)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output4 in result
    if dist_mem:
        output5 = (
            "      IF (f2_proxy%is_dirty(depth=2)) THEN\n"
            "        CALL f2_proxy%halo_exchange(depth=2)\n"
            "      END IF\n")
        assert output5 in result
    output6 = (
        "        CALL testkern_stencil_code(nlayers, f1_data, "
        "f2_data, f2_stencil_size(cell), f2_stencil_dofmap(:,:,cell), "
        "f3_data, f4_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))")
    assert output6 in result


def test_stencil_region(dist_mem, tmpdir):
    '''Test that region stencil access with an extent value passed from the
    algorith layer is handled correctly.
    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.12_single_stencil_region.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = ("    SUBROUTINE invoke_0_testkern_stencil_region_type(f1, f2, "
               "f3, f4, f2_extent)")
    assert output1 in result
    output2 = ("      USE stencil_dofmap_mod, ONLY: STENCIL_REGION\n"
               "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output3 in result
    output4 = (
        "      !\n"
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_REGION,f2_extent)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output4 in result
    if dist_mem:
        output5 = (
            "      IF (f2_proxy%is_dirty(depth=f2_extent+1)) THEN\n"
            "        CALL f2_proxy%halo_exchange(depth=f2_extent+1)\n"
            "      END IF\n")
        assert output5 in result
    output6 = (
        "        CALL testkern_stencil_region_code(nlayers, f1_data, "
        "f2_data, f2_stencil_size(cell), f2_stencil_dofmap(:,:,cell), "
        "f3_data, f4_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))")
    assert output6 in result


def test_single_stencil_cross2d(dist_mem, tmpdir):
    ''' Test a single stencil access with an extent value passed from the
    algorithm layer is treated correctly in the PSy layer. Test both
    sequential and distributed memory.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.26_single_stencil_cross2d.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "SUBROUTINE invoke_0_testkern_stencil_cross2d_type(f1, f2, f3, f4, "
        "f2_extent)")
    assert output1 in result
    output2 = ("USE stencil_2D_dofmap_mod, ONLY: stencil_2D_dofmap_type, "
               "STENCIL_2D_CROSS\n")
    assert output2 in result
    output3 = ("      INTEGER(KIND=i_def), intent(in) :: f2_extent\n")
    assert output3 in result
    output4 = (
        "      INTEGER(KIND=i_def) f2_max_branch_length\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:,:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:,:) => "
        "null()\n"
        "      TYPE(stencil_2D_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output4 in result
    output5 = (
        "      !\n"
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_2D_dofmap("
        "STENCIL_2D_CROSS,f2_extent)\n"
        "      f2_max_branch_length = f2_extent + 1_i_def\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output5 in result
    output6 = (
        "        CALL testkern_stencil_cross2d_code(nlayers, f1_data,"
        " f2_data, f2_stencil_size(:,cell), f2_max_branch_length,"
        " f2_stencil_dofmap(:,:,:,cell), f3_data, f4_data,"
        " ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell),"
        " ndf_w3, undf_w3, map_w3(:,cell))")
    assert output6 in result


def test_single_stencil_xory1d_literal(dist_mem, tmpdir):
    ''' Test extent value is used correctly from the algorithm layer when
    it is a literal value so is not passed by argument.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.5_single_stencil_xory1d_literal.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = ("    SUBROUTINE invoke_0_testkern_stencil_xory1d_type("
               "f1, f2, f3, f4)")
    assert output1 in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
        "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output3 in result
    output4 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      IF (x_direction .eq. x_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,2)\n"
        "      END IF\n"
        "      IF (x_direction .eq. y_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,2)\n"
        "      END IF\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output4 in result
    if dist_mem:
        output5 = (
            "      IF (f2_proxy%is_dirty(depth=3)) THEN\n"
            "        CALL f2_proxy%halo_exchange(depth=3)\n"
            "      END IF\n")
        assert output5 in result
    output6 = (
        "        CALL testkern_stencil_xory1d_code(nlayers, "
        "f1_data, f2_data, f2_stencil_size(cell), x_direction, "
        "f2_stencil_dofmap(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
    assert output6 in result


def test_single_stencil_xory1d_literal_mixed(dist_mem, tmpdir):
    ''' Test extent value is used correctly from the algorithm layer when
    it is a literal value so is not passed by argument and the case of the
    literal is specified in mixed case.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.5.1_single_stencil_xory1d_literal.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = ("    SUBROUTINE invoke_0_testkern_stencil_xory1d_type("
               "f1, f2, f3, f4)")
    assert output1 in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
        "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output3 in result
    output4 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      IF (x_direction .eq. x_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,2)\n"
        "      END IF\n"
        "      IF (x_direction .eq. y_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,2)\n"
        "      END IF\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output4 in result
    if dist_mem:
        output5 = (
            "      IF (f2_proxy%is_dirty(depth=3)) THEN\n"
            "        CALL f2_proxy%halo_exchange(depth=3)\n"
            "      END IF\n")
        assert output5 in result
    output6 = (
        "        CALL testkern_stencil_xory1d_code(nlayers, "
        "f1_data, f2_data, f2_stencil_size(cell), x_direction, "
        "f2_stencil_dofmap(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
    assert output6 in result


def test_multiple_stencils(dist_mem, tmpdir):
    ''' Test for correct output when there is more than one stencil in a
    kernel. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.7_multiple_stencils.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "    SUBROUTINE invoke_0_testkern_stencil_multi_type(f1, f2, f3, "
        "f4, f2_extent, f3_extent, f3_direction)")
    assert output1 in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
        "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
        "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), intent(in) :: f2_extent, f3_extent\n"
        "      INTEGER(KIND=i_def), intent(in) :: f3_direction\n")
    assert output3 in result
    output4 = (
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f4_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output4 in result
    output5 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,f2_extent)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      IF (f3_direction .eq. x_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,f3_extent)\n"
        "      END IF\n"
        "      IF (f3_direction .eq. y_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,f3_extent)\n"
        "      END IF\n"
        "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
        "      f3_stencil_size => f3_stencil_map%get_stencil_sizes()\n"
        "      f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,1)\n"
        "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
        "      f4_stencil_size => f4_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output5 in result
    if dist_mem:
        output6 = (
            "      IF (f2_proxy%is_dirty(depth=f2_extent+1)) THEN\n"
            "        CALL f2_proxy%halo_exchange(depth=f2_extent+1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (f3_proxy%is_dirty(depth=f3_extent+1)) THEN\n"
            "        CALL f3_proxy%halo_exchange(depth=f3_extent+1)\n"
            "      END IF\n")
        assert output6 in result
    output7 = (
        "        CALL testkern_stencil_multi_code(nlayers, f1_data, "
        "f2_data, f2_stencil_size(cell), f2_stencil_dofmap(:,:,cell), "
        "f3_data, f3_stencil_size(cell), f3_direction, "
        "f3_stencil_dofmap(:,:,cell), f4_data, f4_stencil_size(cell), "
        "f4_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))")
    assert output7 in result


def test_multiple_stencils_int_field(dist_mem, tmpdir):
    ''' Test for correct output when there is more than one stencil in a
    kernel that contains integer-valued fields. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.25_multiple_stencils_int_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "    USE integer_field_mod, ONLY: integer_field_type, "
        "integer_field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_stencil_multi_int_field_type(f1, "
        "f2, f3, f4, f2_extent, f3_extent, f3_direction)")
    assert output1 in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
        "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
        "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n"
        "      TYPE(integer_field_type), intent(in) :: f1, f2, f3, f4\n"
        "      INTEGER(KIND=i_def), intent(in) :: f2_extent, f3_extent\n"
        "      INTEGER(KIND=i_def), intent(in) :: f3_direction\n")
    assert output2 in result
    output3 = (
        "      TYPE(integer_field_proxy_type) f1_proxy, f2_proxy, "
        "f3_proxy, f4_proxy\n")
    assert output3 in result
    output4 = (
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f4_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output4 in result
    output5 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,f2_extent)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      IF (f3_direction .eq. x_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,f3_extent)\n"
        "      END IF\n"
        "      IF (f3_direction .eq. y_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,f3_extent)\n"
        "      END IF\n"
        "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
        "      f3_stencil_size => f3_stencil_map%get_stencil_sizes()\n"
        "      f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,2)\n"
        "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
        "      f4_stencil_size => f4_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output5 in result
    if dist_mem:
        output6 = (
            "      !\n"
            "      IF (f3_proxy%is_dirty(depth=f3_extent)) THEN\n"
            "        CALL f3_proxy%halo_exchange(depth=f3_extent)\n"
            "      END IF\n"
            "      !\n"
            "      IF (f4_proxy%is_dirty(depth=2)) THEN\n"
            "        CALL f4_proxy%halo_exchange(depth=2)\n"
            "      END IF\n")
        assert output6 in result
    output7 = (
        "        CALL testkern_stencil_multi_int_field_code(nlayers, "
        "f1_data, f2_data, f2_stencil_size(cell), "
        "f2_stencil_dofmap(:,:,cell), f3_data, f3_stencil_size(cell), "
        "f3_direction, f3_stencil_dofmap(:,:,cell), f4_data, "
        "f4_stencil_size(cell), f4_stencil_dofmap(:,:,cell), ndf_w2broken, "
        "undf_w2broken, map_w2broken(:,cell), ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w0, undf_w0, map_w0(:,cell), ndf_w2v, "
        "undf_w2v, map_w2v(:,cell))")
    assert output7 in result


def test_multiple_stencil_same_name(dist_mem, tmpdir):
    ''' Test the case when there is more than one stencil in a kernel with
    the same name for extent. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.8_multiple_stencils_same_name.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "    SUBROUTINE invoke_0_testkern_stencil_multi_type(f1, f2, f3, "
        "f4, extent, f3_direction)")
    assert output1 in result
    output2 = (
        "      INTEGER(KIND=i_def), intent(in) :: extent\n"
        "      INTEGER(KIND=i_def), intent(in) :: f3_direction\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f4_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output3 in result
    output4 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,extent)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      IF (f3_direction .eq. x_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,extent)\n"
        "      END IF\n"
        "      IF (f3_direction .eq. y_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,extent)\n"
        "      END IF\n"
        "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
        "      f3_stencil_size => f3_stencil_map%get_stencil_sizes()\n"
        "      f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,extent)\n"
        "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
        "      f4_stencil_size => f4_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output4 in result
    output5 = (
        "        CALL testkern_stencil_multi_code(nlayers, f1_data, "
        "f2_data, f2_stencil_size(cell), f2_stencil_dofmap(:,:,cell), "
        "f3_data, f3_stencil_size(cell), f3_direction, "
        "f3_stencil_dofmap(:,:,cell), f4_data, f4_stencil_size(cell), "
        "f4_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))")
    assert output5 in result


def test_multi_stencil_same_name_direction(dist_mem, tmpdir):
    ''' Test the case where there is more than one stencil in a kernel
    with the same name for direction.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.9_multiple_stencils_same_name.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    output1 = (
        "SUBROUTINE invoke_0_testkern_stencil_multi_2_type(f1, f2, f3, "
        "f4, extent, direction)")
    assert output1 in result
    output2 = (
        "      INTEGER(KIND=i_def), intent(in) :: extent\n"
        "      INTEGER(KIND=i_def), intent(in) :: direction\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f4_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output3 in result
    output4 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      IF (direction .eq. x_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,extent)\n"
        "      END IF\n"
        "      IF (direction .eq. y_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,extent)\n"
        "      END IF\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      IF (direction .eq. x_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,extent)\n"
        "      END IF\n"
        "      IF (direction .eq. y_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,extent)\n"
        "      END IF\n"
        "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
        "      f3_stencil_size => f3_stencil_map%get_stencil_sizes()\n"
        "      IF (direction .eq. x_direction) THEN\n"
        "        f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,extent)\n"
        "      END IF\n"
        "      IF (direction .eq. y_direction) THEN\n"
        "        f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,extent)\n"
        "      END IF\n"
        "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
        "      f4_stencil_size => f4_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output4 in result
    output5 = (
        "     CALL testkern_stencil_multi_2_code(nlayers, f1_data, "
        "f2_data, f2_stencil_size(cell), direction, "
        "f2_stencil_dofmap(:,:,cell), "
        "f3_data, f3_stencil_size(cell), direction, "
        "f3_stencil_dofmap(:,:,cell), "
        "f4_data, f4_stencil_size(cell), direction, "
        "f4_stencil_dofmap(:,:,cell), "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_adspc1_f4, undf_adspc1_f4, "
        "map_adspc1_f4(:,cell))")
    assert output5 in result

    # Check compilation
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_multi_kerns_stencils_diff_fields(dist_mem, tmpdir):
    ''' Test the case where we have multiple kernels with stencils and
    different fields for each. We also test extent names by having both
    shared and individual names.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.20_multiple_kernels_stencils.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "    SUBROUTINE invoke_0(f1, f2a, f3, f4, f2b, f2c, f2a_extent, "
        "extent)")
    assert output1 in result
    assert "USE testkern_stencil_mod, ONLY: testkern_stencil_code\n" in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), intent(in) :: f2a_extent, extent\n")
    assert output3 in result
    output4 = (
        "      INTEGER(KIND=i_def), pointer :: f2b_stencil_size(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2b_stencil_dofmap(:,:,:) "
        "=> null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2b_stencil_map "
        "=> null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2a_stencil_size(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2a_stencil_dofmap(:,:,:) "
        "=> null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2a_stencil_map "
        "=> null()\n")
    assert output4 in result
    output5 = (
        "      !\n"
        "      f2a_stencil_map => f2a_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,f2a_extent)\n"
        "      f2a_stencil_dofmap => f2a_stencil_map%get_whole_dofmap()\n"
        "      f2a_stencil_size => f2a_stencil_map%get_stencil_sizes()\n"
        "      f2b_stencil_map => f2b_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,extent)\n"
        "      f2b_stencil_dofmap => f2b_stencil_map%get_whole_dofmap()\n"
        "      f2b_stencil_size => f2b_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output5 in result
    output6 = (
        "        CALL testkern_stencil_code(nlayers, f1_data, "
        "f2a_data, f2a_stencil_size(cell), "
        "f2a_stencil_dofmap(:,:,cell), f3_data, f4_data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))")
    assert output6 in result
    output7 = (
        "        CALL testkern_stencil_code(nlayers, f1_data, "
        "f2b_data, f2b_stencil_size(cell), "
        "f2b_stencil_dofmap(:,:,cell), f3_data, f4_data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))")
    assert output7 in result
    output8 = (
        "        CALL testkern_stencil_code(nlayers, f1_data, "
        "f2c_data, f2b_stencil_size(cell), "
        "f2b_stencil_dofmap(:,:,cell), f3_data, f4_data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))")
    assert output8 in result


def test_extent_name_clash(dist_mem, tmpdir):
    ''' Test we can deal with name clashes for stencils. We have a single
    kernel with argument names passed from the algorithm layer that
    would clash with stencil-name, stencil-dofmap and stencil-size
    variables.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.13_single_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "    SUBROUTINE invoke_0(f2_stencil_map, f2, f2_stencil_dofmap, "
        "stencil_cross_1, f3_stencil_map, f3, f3_stencil_dofmap, "
        "f2_extent, f3_stencil_size)")
    assert output1 in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type")
    assert output2 in result
    assert ("INTEGER(KIND=i_def), intent(in) :: f2_extent, f3_stencil_size\n"
            in result)
    output3 = (
        "      TYPE(field_type), intent(in) :: f2_stencil_map, f2, "
        "f2_stencil_dofmap, stencil_cross_1, f3_stencil_map, f3, "
        "f3_stencil_dofmap\n")
    assert output3 in result
    output4 = (
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_size_1(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_dofmap_1(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map_1 => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap_1(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map_1 => "
        "null()\n")
    assert output4 in result
    output5 = (
        "      TYPE(field_proxy_type) f2_stencil_map_proxy, f2_proxy, "
        "f2_stencil_dofmap_proxy, stencil_cross_1_proxy, "
        "f3_stencil_map_proxy, f3_proxy, f3_stencil_dofmap_proxy\n")
    assert output5 in result
    output6 = (
        "      stencil_cross_1_proxy = stencil_cross_1%get_proxy()")
    assert output6 in result
    output7 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,f2_extent)\n"
        "      f2_stencil_dofmap_1 => "
        "f2_stencil_map_1%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map_1%get_stencil_sizes()\n"
        "      f3_stencil_map_1 => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,f3_stencil_size)\n"
        "      f3_stencil_dofmap_1 => "
        "f3_stencil_map_1%get_whole_dofmap()\n"
        "      f3_stencil_size_1 => f3_stencil_map_1%get_stencil_sizes()\n"
        "      !\n")
    assert output7 in result
    output8 = (
        "        CALL testkern_stencil_code(nlayers, "
        "f2_stencil_map_data, f2_data, f2_stencil_size(cell), "
        "f2_stencil_dofmap_1(:,:,cell), f2_stencil_dofmap_data, "
        "stencil_cross_1_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))")
    assert output8 in result
    output9 = (
        "        CALL testkern_stencil_code(nlayers, "
        "f3_stencil_map_data, f3_data, f3_stencil_size_1(cell), "
        "f3_stencil_dofmap_1(:,:,cell), f3_stencil_dofmap_data, "
        "stencil_cross_1_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))")
    assert output9 in result


def test_two_stencils_same_field(tmpdir, dist_mem):
    ''' Test two Kernels within an invoke, with the same field having a
    stencil access in each kernel. f2_w2 is the field we care
    about.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.14_two_stencils_same_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    output1 = (
        "    SUBROUTINE invoke_0(f1_w1, f2_w2, f3_w2, f4_w3, f1_w3, "
        "f2_extent, extent)")
    assert output1 in result
    output2 = (
        "      INTEGER(KIND=i_def), pointer :: f2_w2_stencil_size_1(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_w2_stencil_dofmap_1(:,:,:) "
        "=> null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_w2_stencil_map_1 "
        "=> null()")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), pointer :: f2_w2_stencil_size(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_w2_stencil_dofmap(:,:,:) "
        "=> null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_w2_stencil_map "
        "=> null()")
    assert output3 in result
    output4 = (
        "      f2_w2_stencil_map => f2_w2_proxy%vspace%get_stencil_dofmap"
        "(STENCIL_CROSS,f2_extent)\n"
        "      f2_w2_stencil_dofmap => "
        "f2_w2_stencil_map%get_whole_dofmap()\n"
        "      f2_w2_stencil_size => f2_w2_stencil_map%get_stencil_sizes()\n")
    assert output4 in result
    output5 = (
        "      f2_w2_stencil_map_1 => "
        "f2_w2_proxy%vspace%get_stencil_dofmap(STENCIL_CROSS,extent)\n"
        "      f2_w2_stencil_dofmap_1 => "
        "f2_w2_stencil_map_1%get_whole_dofmap()\n"
        "      f2_w2_stencil_size_1 => "
        "f2_w2_stencil_map_1%get_stencil_sizes()\n")
    assert output5 in result
    output6 = (
        "        CALL testkern_stencil_code(nlayers, f1_w1_data, "
        "f2_w2_data, f2_w2_stencil_size(cell), "
        "f2_w2_stencil_dofmap(:,:,cell), "
        "f3_w2_data, f4_w3_data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))")
    assert output6 in result
    output7 = (
        "        CALL testkern_stencil_depth_code(nlayers, "
        "f1_w3_data, f1_w1_data, f1_w1_stencil_size(cell), "
        "f1_w1_stencil_dofmap(:,:,cell), f2_w2_data, "
        "f2_w2_stencil_size_1(cell), "
        "f2_w2_stencil_dofmap_1(:,:,cell), f4_w3_data, "
        "f4_w3_stencil_size(cell), "
        "f4_w3_stencil_dofmap(:,:,cell), ndf_w3, undf_w3, map_w3(:,cell), "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell))")
    assert output7 in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_stencils_same_field_literal_extent(dist_mem, tmpdir):
    ''' Test three Kernels within an invoke, with the same field having a
    stencil access in each kernel and the extent being passed as a
    literal value. Extent is the same in two kernels and different in
    the third.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.15_stencils_same_field_literal_extent.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size_1(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap_1(:,:,:) "
        "=> null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map_1 "
        "=> null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) "
        "=> null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map "
        "=> null()")
    assert output1 in result
    output2 = (
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,1)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,2)\n"
        "      f2_stencil_dofmap_1 => "
        "f2_stencil_map_1%get_whole_dofmap()\n"
        "      f2_stencil_size_1 => f2_stencil_map_1%get_stencil_sizes()\n"
        "      !")
    assert output2 in result
    output3 = (
        "        CALL testkern_stencil_code(nlayers, f1_data, "
        "f2_data, f2_stencil_size(cell), f2_stencil_dofmap(:,:,cell), "
        "f3_data, f4_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))")
    assert result.count(output3) == 2
    output4 = (
        "        CALL testkern_stencil_code(nlayers, f1_data, "
        "f2_data, f2_stencil_size_1(cell), "
        "f2_stencil_dofmap_1(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))")
    assert result.count(output4) == 1

    if dist_mem:
        assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
        assert "CALL f2_proxy%halo_exchange(depth=3)" in result
        assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
        assert "CALL f3_proxy%halo_exchange(depth=1)" in result
        assert "IF (f4_proxy%is_dirty(depth=1)) THEN" in result
        assert "CALL f4_proxy%halo_exchange(depth=1)" in result


def test_stencils_same_field_literal_direct(dist_mem, tmpdir):
    ''' Test three Kernels within an invoke, with the same field having a
    stencil access in each kernel and the direction being passed as a
    literal value. In two kernels the direction value is the same and
    in the third it is different.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.16_stencils_same_field_literal_direction.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size_1(:) => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap_1(:,:,:) "
        "=> null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map_1 "
        "=> null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) "
        "=> null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map "
        "=> null()")
    assert output1 in result
    output2 = (
        "      !\n"
        "      IF (x_direction .eq. x_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,2)\n"
        "      END IF\n"
        "      IF (x_direction .eq. y_direction) THEN\n"
        "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,2)\n"
        "      END IF\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      IF (y_direction .eq. x_direction) THEN\n"
        "        f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,2)\n"
        "      END IF\n"
        "      IF (y_direction .eq. y_direction) THEN\n"
        "        f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,2)\n"
        "      END IF\n"
        "      f2_stencil_dofmap_1 => "
        "f2_stencil_map_1%get_whole_dofmap()\n"
        "      f2_stencil_size_1 => f2_stencil_map_1%get_stencil_sizes()\n"
        "      !")
    assert output2 in result
    output3 = (
        "        CALL testkern_stencil_xory1d_code(nlayers, "
        "f1_data, f2_data, f2_stencil_size(cell), x_direction, "
        "f2_stencil_dofmap(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
    assert result.count(output3) == 2
    output4 = (
        "        CALL testkern_stencil_xory1d_code(nlayers, "
        "f1_data, f2_data, f2_stencil_size_1(cell), y_direction, "
        "f2_stencil_dofmap_1(:,:,cell), f3_data, f4_data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
    assert result.count(output4) == 1

    if dist_mem:
        assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
        assert "CALL f2_proxy%halo_exchange(depth=3)" in result
        assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
        assert "CALL f3_proxy%halo_exchange(depth=1)" in result
        assert "IF (f4_proxy%is_dirty(depth=1)) THEN" in result
        assert "CALL f4_proxy%halo_exchange(depth=1)" in result


def test_stencil_extent_specified():
    ''' The function stencil_unique_str() raises an error if a stencil
    with an extent provided in the metadata is passed in. This is because
    this is not currently supported. This test checks that the appropriate
    error is raised.

    '''
    # load an example with an argument that has stencil metadata
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    # access the argument with stencil metadata
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.children[4].loop_body[0]
    stencil_arg = kernel.arguments.args[1]
    # artificially add an extent to the stencil metadata info
    stencil_arg.descriptor.stencil['extent'] = 1
    stencils = LFRicStencils(psy.invokes.invoke_list[0])
    with pytest.raises(GenerationError) as err:
        stencils.stencil_unique_str(stencil_arg, "")
    assert ("Found a stencil with an extent specified in the metadata. "
            "This is not coded for." in str(err.value))


def test_one_kern_multi_field_same_stencil(tmpdir, dist_mem):
    ''' This test checks for the case where we have the same stencil used
    by more than one field in a kernel. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.17_single_kernel_multi_field_same_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "    SUBROUTINE invoke_0_testkern_multi_field_same_stencil_type("
        "f0, f1, f2, f3, f4, extent, direction)")
    assert output1 in result
    output2 = (
        "      INTEGER(KIND=i_def), intent(in) :: extent\n"
        "      INTEGER(KIND=i_def), intent(in) :: direction\n")
    assert output2 in result
    output3 = (
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f1_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f1_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f1_stencil_map => "
        "null()\n")
    assert output3 in result
    output4 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f1_stencil_map => f1_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,extent)\n"
        "      f1_stencil_dofmap => f1_stencil_map%get_whole_dofmap()\n"
        "      f1_stencil_size => f1_stencil_map%get_stencil_sizes()\n"
        "      IF (direction .eq. x_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,extent)\n"
        "      END IF\n"
        "      IF (direction .eq. y_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,extent)\n"
        "      END IF\n"
        "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
        "      f3_stencil_size => f3_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output4 in result
    output5 = (
        "        CALL testkern_multi_field_same_stencil_code(nlayers, "
        "f0_data, f1_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), f2_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), f3_data, f3_stencil_size(cell), "
        "direction, f3_stencil_dofmap(:,:,cell), f4_data, "
        "f3_stencil_size(cell), direction, f3_stencil_dofmap(:,:,cell), "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell))")
    assert output5 in result


def test_single_kernel_any_space_stencil(dist_mem, tmpdir):
    ''' This is a test for stencils and any_space within a single kernel
    and between kernels. We test when any_space is the same and when
    it is different within kernels and between kernels for the case of
    different fields. When it is the same we should have the same
    stencil dofmap (as all other stencil information is the same) and
    when it is different we should have a different stencil dofmap (as
    we do not know whether they are on the same space).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.18_anyspace_stencil_1.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "      f1_stencil_map => f1_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,extent)\n"
        "      f1_stencil_dofmap => f1_stencil_map%get_whole_dofmap()\n"
        "      f1_stencil_size => f1_stencil_map%get_stencil_sizes()\n"
        "      f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,extent)\n"
        "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
        "      f4_stencil_size => f4_stencil_map%get_stencil_sizes()\n"
        "      f5_stencil_map => f5_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,extent)\n"
        "      f5_stencil_dofmap => f5_stencil_map%get_whole_dofmap()\n"
        "      f5_stencil_size => f5_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output1 in result
    # Use the same stencil dofmap
    output2 = (
        "        CALL testkern_same_anyspace_stencil_code(nlayers, "
        "f0_data, f1_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), f2_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_aspc1_f1, undf_aspc1_f1, map_aspc1_f1(:,cell))")
    assert output2 in result
    output3 = (
        "        CALL testkern_different_anyspace_stencil_code(nlayers, "
        "f3_data, f4_data, f4_stencil_size(cell), "
        "f4_stencil_dofmap(:,:,cell), f5_data, f5_stencil_size(cell), "
        "f5_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_aspc1_f4, undf_aspc1_f4, map_aspc1_f4(:,cell), ndf_aspc2_f5, "
        "undf_aspc2_f5, map_aspc2_f5(:,cell))")
    # Use a different stencil dofmap
    assert output3 in result


@pytest.mark.xfail(reason="stencils and any_space produces too many dofmaps")
def test_multi_kernel_any_space_stencil_1(dist_mem):
    ''' This is a test for stencils and any_space with two kernels. We test
    when any_space is the same and when it is different for the same
    field. In our example we should have a single dofmap. However, at
    the moment we produce two. This is valid but not optimal. It is
    not a big deal at the moment as the Met Office do not plan to use
    any_space but it should be able to be fixed when we get dependence
    analysis within invokes working. Therefore making it xfail for the
    moment.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "19.19_anyspace_stencil_2.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    output1 = (
        "      f1_stencil_map => f1_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,extent)\n"
        "      f1_stencil_dofmap => f1_stencil_map%get_whole_dofmap()\n"
        "      f1_stencil_size => f1_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output1 in result
    output2 = (
        "        CALL testkern_same_anyspace_stencil_code(nlayers, "
        "f0_data, f1_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), f2_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_aspc1_f1, undf_aspc1_f1, map_aspc1_f1)")
    assert output2 in result
    output3 = (
        "        CALL testkern_different_anyspace_stencil_code(nlayers, "
        "f3_data, f1_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), f2_data, f1_stencil_size(cell), "
        "f1_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_aspc1_f1, undf_aspc1_f1, map_aspc1_f1, "
        "ndf_aspc2_f2, undf_aspc2_f2, map_aspc2_f2)")
    assert output3 in result


def test_dynkernargs_unexpect_stencil_extent():
    '''This test checks that we raise an error in DynKernelArguments if
    metadata is provided with an extent value. This is a litle tricky
    to raise as the parser does not allow this to happen. We therefore
    modify the results from the parser to raise the error.

    '''
    # parse some valid code with a stencil
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api=TEST_API)
    # find the parsed code's call class
    call = invoke_info.calls[0].kcalls[0]
    # add an extent to the stencil metadata
    kernel_metadata = call.ktype
    kernel_metadata._arg_descriptors[1].stencil['extent'] = 2
    # remove the extra argument (as the extent value no longer needs
    # to be passed so an associated error will be raised)
    del call.args[2]
    # finally call our object to raise the error
    with pytest.raises(GenerationError) as err:
        _ = DynKernelArguments(call, None)
    assert "extent metadata not yet supported" in str(err.value)


def test_lfricstencils_extent_vars_err(monkeypatch):
    ''' Check that the _unique_extent_vars method of LFRicStencils raises
    the expected internal error. '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    stencils = LFRicStencils(invoke)
    # Monkeypatch it to break internal state
    monkeypatch.setattr(stencils, "_invoke", None)
    with pytest.raises(InternalError) as err:
        _ = stencils._unique_extent_vars
    assert ("_unique_extent_vars: have neither Invoke or Kernel"
            in str(err.value))


def test_lfricstencils_err():
    ''' Check that LFRicStencils.initialise and
    LFRicStencils._declare_maps_invoke raises the expected
    InternalError if an unsupported stencil type is encountered. '''
    _, info = parse(os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    stencils = LFRicStencils(invoke)
    # Break internal state
    stencils._kern_args[0].descriptor.stencil['type'] = "not-a-type"
    with pytest.raises(GenerationError) as err:
        stencils.initialise(ModuleGen(name="testmodule"))
    assert "Unsupported stencil type 'not-a-type' supplied." in str(err.value)
    with pytest.raises(GenerationError) as err:
        stencils._declare_maps_invoke(ModuleGen(name="testmodule"))
    assert "Unsupported stencil type 'not-a-type' supplied. Supported " \
        "mappings are" in str(err.value)
