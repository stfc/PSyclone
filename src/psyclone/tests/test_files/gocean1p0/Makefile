# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council
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
# Author J. Henrichs, Bureau of Meteorology


# Test files to see if the source files work with dl_esm_info:
TEST_FILES = continuity_mod.f90 model_mod.f90 momentum_mod.f90     \
             boundary_conditions_ne_offset_mod.f90                 \
             kernel_no_fld_args.f90 kernel_requires_grid_props.f90 \
             kernel_scalar_float.f90 kernel_scalar_int.f90 \
             kernel_missing_iterates_over.f90 kernel_missing_offset.f90 \
             kernel_ne_offset_mod.f90  \
             compute_cu_mod.f90 compute_cv_mod.f90  time_smooth_mod.f90 \
             kernel_stencil.f90 kernel_sw_offset_cu_mod.f90 kernel_sw_offset_cv_mod.f90 \
             kernel_sw_offset_cf_mod.f90 kernel_ne_offset_cf_mod.f90 kernel_ne_offset_ct_mod.f90 \
             kernel_any_offset_cu_mod.f90 kernel_sw_offset_ct_mod.f90 kernel_field_copy_mod.f90 \
             kernel_unsupported_offset_mod.f90 kernel_missing_stencil.f90
             

# These files can not be compiled because of invoke statements, which causes errors
# such as:
# Error: Component ‘no_op’ at (1) is a PRIVATE component of ‘kernel_type’
# single_invoke_two_identical_kernels.f90  single_invoke_two_kernels.f90 single_invoke_two_kernels_scalars.f90
# test14_module_inline_same_kernel.f90 single_invoke_write_to_read.f90 test14_ne_offset_cf_updated_one_invoke.f90
# test00.1_invoke_kernel_using_const_scalar.f90 test15_ne_offset_ct_updated_one_invoke.f90
# test16_ne_offset_cu_updated_one_invoke.f90 test17_ne_offset_cv_updated_one_invoke.f90
# test18_ne_offset_cf_updated_one_invoke.f90 test01_different_grid_offsets_one_invoke.f90
# test19.1_sw_offset_cf_updated_one_invoke.f90 test02_different_grid_offsets_two_invokes.f90 
# test19.2_sw_offset_all_cf_updated_one_invoke.f90  test20_sw_offset_ct_updated_one_invoke.f90
# test21_sw_offset_all_ct_updated_one_invoke.f90 test22_sw_offset_all_cu_updated_one_invoke.f90
# nemolite2d_alg_mod.f90 test05_invoke_kernel_missing_iterates_over.f90 single_invoke.f90 
# test23_sw_offset_all_cv_updated_one_invoke.f90 test24_any_offset_all_update_one_invoke.f90 
# test25_any_offset_all_cu_update_one_invoke.f90 test27_loop_swap.f90 single_invoke_three_kernels.f90 
# test08.1_invoke_kernel_no_fld_args.f90 test26_const_bounds_invalid_offset.f90 
# single_invoke_grid_props.f90 single_invoke_scalar_float_arg.f90 single_invoke_scalar_int_arg.f90 
# test09_invoke_kernel_missing_stencil.f90 test28_invoke_kernel_stencil.f90 
# test11_different_iterates_over_one_invoke.f90 


# New iteration space has no symbol defined in dl_esm_inf module for the new iteration space.
# So PSyclone accepts it, but it can't be compiled
# new_iteration_space_kernel.f90 new_iteration_space.f90  


# Invalid Fortran (directly, or module with invalid Fortran used, which prevents the
# files from being compiled)
# kernel_wrong_meta_arg_arg_count.f90 test00.1_invoke_kernel_wrong_meta_arg_count.f90
# test00.2_invoke_kernel_invalid_meta_args.f90 kernel_invalid_grid_property.f90 
# kernel_invalid_meta_arg_type.f90 test00.3_invoke_kernel_invalid_meta_arg_type.f90
# kernel_invalid_iterates_over.f90 kernel_wrong_access.f90 kernel_invalid_fortran.f90
# kernel_invalid_meta_args.f90 kernel_wrong_gridpt_type.f90
# kernel_wrong_meta_arg_arg_count.f90 test03_invoke_kernel_missing_offset.f90
# kernel_invalid_offset.f90 test04_invoke_kernel_invalid_offset.f90
# kernel_invalid_stencil.f90 test05.1_invoke_kernel_invalid_iterates_over.f90 
# test06_invoke_kernel_wrong_access.f90 test08_invoke_kernel_invalid_grid_property.f90 
# test07_invoke_kernel_wrong_gridpt_type.f90 test10_invoke_kernel_invalid_stencil.f90   

TEST_OBJ = $(patsubst %.f90, %.o, ${TEST_FILES})

F90 = gfortran
INFRA_NAME=dl_esm_inf
INFRASTRUCTURE=../../../../../external/$(INFRA_NAME)/finite_difference/src

.PHONY: tests default

default:
	make F90=$(F90) API_LIB=lib$(INFRA_NAME).a -C $(INFRASTRUCTURE)
	make tests

tests: $(TEST_OBJ)

clean:
	rm -f *.o *.mod

allclean:
	make -C $(INFRASTRUCTURE) clean
	make clean

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(INFRASTRUCTURE) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -I $(INFRASTRUCTURE) -c $<
