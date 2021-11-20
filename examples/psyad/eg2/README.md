# PSyclone PSyAD Example 2: creating an LFRic adjoint kernel and test harness.

**Authors:** A. R. Porter and R. W. Ford, STFC Daresbury Lab

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

This example demonstrates the use of PSyAD to create the adjoint of an
LFRic tangent-linear kernel (contained in
`tl_hydrostatic_kernel_mod.f90`). Test harness generation is currently
not supported for LFRic kernels.

PSyAD can be run in the directory containing this file by executing:

```sh
make
```

Alternatively, PSyAD may be run from the command line as:

```sh
psyad tl_hydrostatic_kernel_mod.F90 -a r_u exner theta moist_dyn_gas moist_dyn_tot moist_dyn_fac grad_term theta_v_e theta_v_at_quad exner_e exner_at_quad
```

In both cases the adjoint of the tangent-linear kernel is written to
`stdout`.

The Makefile `compile` and `run` targets will eventually build the
kernel, its adjoint and the test harness and then run the resultant
code. However, at the moment, the harness generation does not support
LFRic tangent-linear kernels so these targets do nothing.

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2021, Science and Technology Facilities Council.
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
