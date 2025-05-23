<!--
BSD 3-Clause License

Copyright (c) 2023-2025, Science and Technology Facilities Council.
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

Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
-->

This directory contains fully working adjoint kernels. At the moment
all kernels generated by `psyad` require manual tweaks so the kernels
in this directory have been manually modified and are kept in the
repository. In the future, any kernels that `psyad` is able to
generate without needing any tweaks will be created here and a copy of
the adjoint kernel will no longer be kept in the repository.

Manual tweaks are performed by first generating the adjoint code `cd
..;make` and then the relevant adjoint kernel is copied from the
`adjoint_partial` directory and is updated manually in an editor.

The tweaks made to the adjoint kernel code can be easily seen using
`diff`. For example, after creating the adjoint code (`cd ..;make`),
run `diff adj_matrix_vector_kernel_mod.F90
../adjoint_tweaked/adj_matrix_vector_kernel_mod.F90`

For those kernels that have stencil accesses on 'active' variables,
the "line-by-line" adjoint generated by PSyAD is not a valid
PSyKAl kernel as it ends up writing to cell columns other than
the one passed to the kernel. This then means it cannot be run in
parallel (but may be run in serial). This type of kernel is
indicated by the "lbl_adj_" prefix . For some of these kernels, a
valid PSyKAl version of the adjointed kernel has been created
through a manual process. These then have the standard "adj_"
prefix.
