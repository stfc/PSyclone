<!--
BSD 3-Clause License

Copyright (c) 2018-2025, Science and Technology Facilities Council.
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

Contains the scripts used to process the NEMO code base and to add profiling
instrumentation (https://psyclone.readthedocs.io/en/latest/user_guide/profiling.html)
and OpenACC or OpenMP directives:

1. `process_nemo.py` is a driver script that allows the user to specify
   which files to process with PSyclone, the transformation script to use
   and where to put the outputs:

       $ ./process_nemo.py -h
       usage: process_nemo.py [-h] [-o OUT_DIR] [-s SCRIPT_FILE] [-x]
                              input_file [input_file ...]

       Process the specified NEMO source files using PSyclone

       positional arguments:
         input_file      One or more NEMO pre-processed source files

       optional arguments:
         -h, --help      show this help message and exit
         -o OUT_DIR      Destination directory for processed source files
         -s SCRIPT_FILE  PSyclone transformation script
         -x              exit immediately if PSyclone fails
         -p              add profiling instrumentation to the PROFILE_ONLY file
                         list. Note that files processed by the SCRIPT_FILE may
                         be introducing profiling instrumentation as part of
                         that script.

   In addition to the command-line flags, the script itself contains two
   variables that may be used to control its behaviour:

   - `EXCLUDED_FILES`: list of filenames that PSyclone will not attempt to process.
   - `PROFILE_ONLY`: list of filenames to add profiling instrumentation but
      do not attempt to further process by PSyclone.

   Finally, the precise invocation to use when running PSyclone may be
   specified by setting the `PSYCLONE` environment variable. If this is not set
   then `psyclone` must be in the user's PATH.

2. PSyclone transformation scripts:
   - `kernels_trans.py` adds OpenACC kernel directives and places fine-grained
     profiling instrumentation around any regions that haven't had OpenACC
     added.
   - `omp_cpu_trans.py` adds OpenMP directives for CPU threading parallelism.
   - `omp_gpu_trans.py` adds OpenMP offloading directives for GPU acceleration.

These scripts are a *work in progress* and are being developed to work on the
MO_GO8 configuration of NEMO supplied by the Met Office. This configuration is
based on version 4.0.2 of NEMO and is compiled using:

    ./makenemo -n MO_GO8_GPU -r SPITZ12 -m linux_nvfortran_gpu \
        del_key "key_iomput key_mpp_mpi" add_key "key_nosignedzero"

(where you will need an `arch/arch-linux_nvfortran_gpu.fcm` FCM configuration
file specifying how to use the NVIDIA compiler).

If you are applying PSyclone to any other version or configuration of NEMO then
these scripts should serve as a useful starting point.

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
