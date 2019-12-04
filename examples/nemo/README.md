<!--
BSD 3-Clause License

Copyright (c) 2018-2019, Science and Technology Facilities Council.
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

Author A. R. Porter, STFC Daresbury Lab
-->

# PSyclone NEMO Examples

This directory contains various examples of the use of PSyclone
to transform source code from the NEMO ocean model. See the READMEs
in the individual example directories for further details.

## Code

Contains

1. the Tracer advection benchmark routine (tra_adv), as provided by
Silvia Mocavero of CMCC and
2. an unmodified NEMO subroutine computing the horizontal component of
the lateral tracer mixing trend (traldf_iso).

## Example 1

OpenMP parallelisation of tra_adv over levels.

## Example 2

OpenMP parallelisation of traldf_iso over levels.

## Example 3

OpenACC parallelisation of tra_adv using the 'data' and 'kernels'
directives.

## Example 4

SIR gemeration and transformation to CUDA using Dawn with simple
examples.
