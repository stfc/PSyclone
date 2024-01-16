<!--
BSD 3-Clause License

Copyright (c) 2021-2024, Science and Technology Facilities Council.
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

# PSyAD Examples

This directory contains various examples of the use of PSyAD to
transform tangent linear code to its adjoint. See the READMEs in the
individual example directories for further details.

## Example 1

Simple, generic tangent linear example which is translated to its
adjoint form. A test harness is also generated which may be compiled
and executed to validate the adjoint.

## Example 2

An LFRic example using the kernel that computes the tangent-linear of
the hydrostatic balance term. A test harness for this kernel can also
be generated. However, it must be incorporated into an LFRic mini-app
in order to be compiled and executed.

## lfric

Creates and/or stores adjoint versions of all of the LFRic
tangent-linear kernels requested by the Met Office. At the current
time a test harness is not created for these kernels so they are not
automatically validated. As some of the tangent-linear kernels require
tweaks before `psyad` is able to translate them, this example stores
some manually modified tangent-linear kernels as well as the original
unmodified kernels. Further, as the adjoint kernels automatically
created by `psyad` require tweaks before they are valid adjoint
kernels, this example also stores manually modified adjoint kernels.
