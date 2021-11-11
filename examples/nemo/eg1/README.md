# PSyclone NEMO Example 1

**Authors:** A. R. Porter, STFC Daresbury Lab
**Modified by:** R. W. Ford, STFC Daresbury Lab
**Modified by:** S. Siso, STFC Daresbury Lab

This directory contains the PSyclone example scripts: `openmp_trans.py` and
`openmp_gpu_trans.py`, that demonstrates the use of PSyclone to parallelise
all loops over levels for the `tra_adv.F90` code, using OpenMP. Once you have
installed PSyclone, these scripts may be run by doing:

```sh
psyclone -api nemo -s ./openmp_{gpu_}trans.py ../code/traadv.f90
```

This will output the generated Fortran code with the OpenMP directives added.

`tra_adv.F90` - is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2018-2021, Science and Technology Facilities Council
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

-----------------------------------------------------------------------------
