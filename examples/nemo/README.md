<!--
BSD 3-Clause License

Copyright (c) 2018-2026, Science and Technology Facilities Council.
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
Modified by R. W. Ford, STFC Daresbury Lab
Modified by J. Henrichs, Bureau of Meteorology
Modified by S. Siso, STFC Daresbury Lab

-->

# PSyclone NEMO Examples

This directory contains various examples of the use of PSyclone
to transform source code from the NEMO ocean model. See the READMEs
in the individual example directories for further details.

## Code

Contains:

1. the Tracer advection benchmark routine (tra_adv), as provided by
   Silvia Mocavero of CMCC and
2. an unmodified NEMO subroutine computing the horizontal component of
   the lateral tracer mixing trend (traldf_iso).

## Scripts

Contains a collection of example scripts and the instructions to process the NEMO code. These
are testend in our integration test against NEMOv4.0.2 and NEMOv5.0.

## Example 1

OpenMP parallelisation (for CPU and GPU) of tra_adv over levels.

## Example 2

OpenMP parallelisation of traldf_iso over levels.

## Example 3

OpenACC parallelisation of tra_adv. Contains a local transformation
script that adds both 'data' and 'kernels' directives to the
code. Also demonstrates the use of the `kernels_trans.py` script from
the `scripts` directory which adds 'kernels' and 'loop' directives as
well as profiling instrumentation. This script is designed for use
with NVIDIA's managed memory technology and therefore does not insert
data regions.

## Example 4

SIR generation and transformation to CUDA using Dawn with simple
examples and a cut down version of the tracer advection (tra_adv)
benchmark.

## Example 5

A simple stand-alone example that shows how data can be extracted for
each loop nest using PSyclone's kernel extraction feature PSyKE. Note
that creation of a driver program (which reads the data files,
execute the original loop and then compares the results) is not yet
supported for generic transformations.

## Example 6

A simple stand-alone example that shows verification that read-only data
is not modified, e.g. by out-of-bounds accesses to other variables.
This uses the PSyData interface to instrument generic Fortran code.
