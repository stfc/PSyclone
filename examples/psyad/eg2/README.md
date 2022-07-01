# PSyclone PSyAD Example 2: creating an LFRic adjoint kernel and test harness.

**Authors:** A. R. Porter and R. W. Ford, STFC Daresbury Lab

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

This example demonstrates the use of PSyAD to create the adjoint of an
LFRic tangent-linear kernel (contained in
`tl_hydrostatic_kernel_mod.f90`) as well as a test harness for this
adjoint.

PSyAD can be run in the directory containing this file by executing:

```sh
make
```

This will construct the adjoint of the kernel (written to
`tl_hydrostatic_kernel_mod_adj.x90`) and a test harness in the form of an
Algorithm (`main_alg.x90`). The Makefile then proceeds to process the test
harness Algorithm using PSyclone to generate an algorithm (`alg.f90`) and
PSy layer (`psy.f90`).

There is no `compile` target for this example because the generated code
requires the full LFRic infrastructure. However, it is straightforward
to modify the LFRic "skeleton" mini-app to call the algorithm subroutine
that has been generated.

Alternatively, PSyAD may be run from the command line as:

```sh
psyad tl_hydrostatic_kernel_mod.F90 -a r_u exner theta moist_dyn_gas moist_dyn_tot moist_dyn_fac grad_term theta_v_e theta_v_at_quad exner_e exner_at_quad
```

In this case, the adjoint of the tangent-linear kernel is written to
`stdout`.

## Testing Other LFRic Kernels

The Makefile is written such that it may be used for other
tangent-linear kernels that follow LFRic naming conventions. For
instance, some of those that are of immediate interest in the LFRic
adjoint project may be tested as follows (assuming that the specified
kernel has been put in the current working directory):

```sh
ACTIVE_VAR_LIST="rhs_eos exner rho theta moist_dyn_gas eos exner_quad theta_vd_quad rho_quad rho_e exner_e theta_vd_e" TL_KERNEL_NAME=tl_rhs_eos_kernel make compile
```

```sh
ACTIVE_VAR_LIST="xi u res_dot_product curl_u" TL_KERNEL_NAME=strong_curl_kernel make run
```

```sh
ACTIVE_VAR_LIST="lhs x lhs_e x_e" TL_KERNEL_NAME=matrix_vector_kernel make run
```

```sh
ACTIVE_VAR_LIST="lhs x lhs_e x_e" TL_KERNEL_NAME=transpose_matrix_vector_kernel make run
```

```sh
ACTIVE_VAR_LIST="lhs x lhs_e x_e" TL_KERNEL_NAME=dg_matrix_vector_kernel make run
```

```sh
ACTIVE_VAR_LIST="r_u vorticity wind res_dot_product vorticity_term cross_product1 cross_product2 j_vorticity u_at_quad mul2 vorticity_at_quad" TL_KERNEL_NAME=tl_vorticity_advection_kernel make run
```

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2021-2022, Science and Technology Facilities Council.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

------------------------------------------------------------------------------
