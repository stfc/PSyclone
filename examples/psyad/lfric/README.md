<!--
BSD 3-Clause License

Copyright (c) 2023-2024, Science and Technology Facilities Council.
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

This directory contains the tangent-linear kernels for which the Met
Office require adjoint versions and Makefile's to generate the adjoint
versions of these kernels using `psyad`.

The `Makefile` in this directory creates adjoint kernels from the
tangent-linear kernels in the `tangent_linear` and
`tangent_linear_tweaked` directories and places the generated adjoint
kernels in either the `adjoint_partial` or `adjoint` directories.

The reason for having two directories containing the tangent-linear
kernels is that some required manual modification before `psyad` is
able to process them. These kernels are copied from the
`tangent_linear` directory, manually modified and placed in the
`tangent_linear_tweaked` directory.

The reason for having two directories containing the adjoint kernels
is that some require manual modification after 'psyad' has processed
them. These kernels are copied from the `adjoint_partial` directory,
manually modified and placed in the `adjoint` directory. At the
present time all generated adjoint kernels need some manual
modification so none are placed directly into the `adjoint` directory
by `psyad`.

The original tangent-linear kernels (stored in the `tangent_linear`
directory) are copied from the LFRic repository with no changes and
are stored here purely for convenience. These kernels are taken from
the branch
https://code.metoffice.gov.uk/svn/lfric/LFRic/branches/dev/christinejohnson/r36316_tl_example
(last changed date: Wednesday the 6th of July 2022), as that is what
the Met Office are also working from at this time (January 2023).
