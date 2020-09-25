# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author: I. Kavcic, Met Office

'''
Module containing pytest tests for the LFRic fields and the related
functionality.
'''

# Imports
from __future__ import absolute_import, print_function
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.domain.lfric import LFRicArgDescriptor
from psyclone.dynamo0p3 import DynKernMetadata
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.parse.utils import ParseError
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.errors import InternalError


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

# General field checks (argument type, data type, etc)

FIELD_CODE = '''
module testkern_field_mod
  type, extends(kernel_type) :: testkern_field_type
     type(arg_type), meta_args(6) =                             &
          (/ arg_type(gh_scalar, gh_real,    gh_read),          &
             arg_type(gh_field,  gh_real,    gh_inc,   w1),     &
             arg_type(gh_field,  gh_real,    gh_read,  w2),     &
             arg_type(gh_field,  gh_integer, gh_write, wtheta), &
             arg_type(gh_field,  gh_integer, gh_read,  w3),     &
             arg_type(gh_scalar, gh_integer, gh_read)           &
           /)
     type(func_type), dimension(2) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_field_code
  end type testkern_field_type
contains
  subroutine testkern_field_code()
  end subroutine testkern_field_code
end module testkern_field_mod
'''


def test_ad_field_invalid_data_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a field has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_field_type"
    # Check real field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,   w1)",
        "arg_type(gh_field,  gh_unreal,  gh_inc,   w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the LFRic API the 2nd argument of a 'meta_arg' entry should "
            "be a valid data type (one of ['gh_real', 'gh_integer']), but "
            "found 'gh_unreal' in 'arg_type(gh_field, gh_unreal, gh_inc, w1)'."
            in str(excinfo.value))
    # Check real field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_integer, gh_read,  w3)",
        "arg_type(gh_field,  gh_double,  gh_read,  w3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the LFRic API the 2nd argument of a 'meta_arg' entry should "
            "be a valid data type (one of ['gh_real', 'gh_integer']), but "
            "found 'gh_double' in 'arg_type(gh_field, gh_double, gh_read, "
            "w3)'." in str(excinfo.value))


def test_ad_field_init_wrong_data_type(monkeypatch):
    ''' Test that an error is raised if an invalid data type
    is passed to the LFRicArgDescriptor._init_field() method. '''
    ast = fpapi.parse(FIELD_CODE, ignore_comments=False)
    name = "testkern_field_type"
    metadata = DynKernMetadata(ast, name=name)
    # Get a real field argument descriptor and set a wrong data type
    real_field_arg = metadata._inits[1]
    real_field_arg.args[1].name = "gh_double"
    # Get an integer field argument descriptor and set a wrong data type
    int_field_arg = metadata._inits[2]
    int_field_arg.args[1].name = "gh_double"
    # Now try to trip the error by making the initial test think
    # that 'gh_double' is actually a valid data type
    monkeypatch.setattr(
        target=LFRicArgDescriptor, name="VALID_ARG_DATA_TYPES",
        value=LFRicArgDescriptor.VALID_ARG_DATA_TYPES + ["gh_double"])
    # Check real field
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            real_field_arg, metadata.iterates_over)._init_field(
                real_field_arg, metadata.iterates_over)
    assert ("Expected one of {0} as the field data type but got 'gh_double'.".
            format(LFRicArgDescriptor.VALID_SCALAR_DATA_TYPES) in
            str(excinfo.value))
    # Check integer field
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            int_field_arg, metadata.iterates_over)._init_field(
                int_field_arg, metadata.iterates_over)
    assert ("Expected one of {0} as the field data type but got 'gh_double'.".
            format(LFRicArgDescriptor.VALID_SCALAR_DATA_TYPES) in
            str(excinfo.value))


# Tests for integer-valued fields


def test_int_field_fs(tmpdir):
    ''' Tests that a call with a set of integer-vaued fields making use of
    all function spaces and no basis functions produces correct code. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.5.5_single_invoke_write_multi_fs_int_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)
    output = (
        "  MODULE single_invoke_fs_int_field_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type, "
        "columnwise_operator_type, columnwise_operator_proxy_type\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_fs_int_field_type(f1, f2, m1, m2, "
        "f3, f4, m3, m4, f5, f6, m5, m6, f7, f8, m7)\n"
        "      USE testkern_fs_int_field_mod, ONLY: "
        "testkern_fs_int_field_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(integer_field_type), intent(in) :: f1, f2, m1, m2, f3, "
        "f4, m3, m4, f5, f6, m5, m6, f7, f8, m7\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(integer_field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, f3_proxy, f4_proxy, m3_proxy, m4_proxy, f5_proxy, "
        "f6_proxy, m5_proxy, m6_proxy, f7_proxy, f8_proxy, m7_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_adspc1_m7(:,:) => null(), "
        "map_any_w2(:,:) => null(), map_aspc1_f8(:,:) => null(), "
        "map_w0(:,:) => null(), map_w1(:,:) => null(), map_w2(:,:) => "
        "null(), map_w2broken(:,:) => null(), map_w2h(:,:) => null(), "
        "map_w2htrace(:,:) => null(), map_w2trace(:,:) => null(), "
        "map_w2v(:,:) => null(), map_w2vtrace(:,:) => null(), map_w3(:,:) "
        "=> null(), map_wchi(:,:) => null(), map_wtheta(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w0, "
        "undf_w0, ndf_w3, undf_w3, ndf_wtheta, undf_wtheta, ndf_w2h, "
        "undf_w2h, ndf_w2v, undf_w2v, ndf_w2broken, undf_w2broken, "
        "ndf_w2trace, undf_w2trace, ndf_w2htrace, undf_w2htrace, "
        "ndf_w2vtrace, undf_w2vtrace, ndf_wchi, undf_wchi, ndf_any_w2, "
        "undf_any_w2, ndf_aspc1_f8, undf_aspc1_f8, ndf_adspc1_m7, "
        "undf_adspc1_m7\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n")
    assert output in generated_code
    output = (
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      f4_proxy = f4%get_proxy()\n"
        "      m3_proxy = m3%get_proxy()\n"
        "      m4_proxy = m4%get_proxy()\n"
        "      f5_proxy = f5%get_proxy()\n"
        "      f6_proxy = f6%get_proxy()\n"
        "      m5_proxy = m5%get_proxy()\n"
        "      m6_proxy = m6%get_proxy()\n"
        "      f7_proxy = f7%get_proxy()\n"
        "      f8_proxy = f8%get_proxy()\n"
        "      m7_proxy = m7%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => m1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => f3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => f4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => m3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2broken => m4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2trace => f5_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2htrace => f6_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2vtrace => m5_proxy%vspace%get_whole_dofmap()\n"
        "      map_wchi => m6_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => f7_proxy%vspace%get_whole_dofmap()\n"
        "      map_aspc1_f8 => f8_proxy%vspace%get_whole_dofmap()\n"
        "      map_adspc1_m7 => m7_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = m1_proxy%vspace%get_ndf()\n"
        "      undf_w0 = m1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wtheta\n"
        "      !\n"
        "      ndf_wtheta = f3_proxy%vspace%get_ndf()\n"
        "      undf_wtheta = f3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2h\n"
        "      !\n"
        "      ndf_w2h = f4_proxy%vspace%get_ndf()\n"
        "      undf_w2h = f4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2v\n"
        "      !\n"
        "      ndf_w2v = m3_proxy%vspace%get_ndf()\n"
        "      undf_w2v = m3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2broken\n"
        "      !\n"
        "      ndf_w2broken = m4_proxy%vspace%get_ndf()\n"
        "      undf_w2broken = m4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2trace\n"
        "      !\n"
        "      ndf_w2trace = f5_proxy%vspace%get_ndf()\n"
        "      undf_w2trace = f5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2htrace\n"
        "      !\n"
        "      ndf_w2htrace = f6_proxy%vspace%get_ndf()\n"
        "      undf_w2htrace = f6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2vtrace\n"
        "      !\n"
        "      ndf_w2vtrace = m5_proxy%vspace%get_ndf()\n"
        "      undf_w2vtrace = m5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wchi\n"
        "      !\n"
        "      ndf_wchi = m6_proxy%vspace%get_ndf()\n"
        "      undf_wchi = m6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = f7_proxy%vspace%get_ndf()\n"
        "      undf_any_w2 = f7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for aspc1_f8\n"
        "      !\n"
        "      ndf_aspc1_f8 = f8_proxy%vspace%get_ndf()\n"
        "      undf_aspc1_f8 = f8_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for adspc1_m7\n"
        "      !\n"
        "      ndf_adspc1_m7 = m7_proxy%vspace%get_ndf()\n"
        "      undf_adspc1_m7 = m7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m3_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m3_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f8_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f8_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_fs_int_field_code(nlayers, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, f3_proxy%data, "
        "f4_proxy%data, m3_proxy%data, m4_proxy%data, f5_proxy%data, "
        "f6_proxy%data, m5_proxy%data, m6_proxy%data, f7_proxy%data, "
        "f8_proxy%data, m7_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, map_w0(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, undf_wtheta, "
        "map_wtheta(:,cell), ndf_w2h, undf_w2h, map_w2h(:,cell), ndf_w2v, "
        "undf_w2v, map_w2v(:,cell), ndf_w2broken, undf_w2broken, "
        "map_w2broken(:,cell), ndf_w2trace, undf_w2trace, "
        "map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell), ndf_aspc1_f8, "
        "undf_aspc1_f8, map_aspc1_f8(:,cell), ndf_adspc1_m7, "
        "undf_adspc1_m7, map_adspc1_m7(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f2_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      CALL f8_proxy%set_dirty()\n"
        "      CALL m7_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_clean(1)\n"
        "      CALL m7_proxy%set_clean(1)\n"
        "      !\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_fs_int_field_type\n"
        "  END MODULE single_invoke_fs_int_field_psy")
    assert output in generated_code
