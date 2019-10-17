# PSyclone NEMO Example 3

**Authors:** R. W. Ford and A. R. Porter, STFC Daresbury Lab

This directory contains two example transformation scripts,
`kernels_trans.py` and `kernels_managed_mem_trans.py`.  The first of these
demonstrates the use of PSyclone to add OpenACC Kernel and Data directives for
NEMO code. The second script is similar but is designed to work with the PGI
compiler's 'managed memory' (`-ta=tesla:managed`) option and thus no attempt is
made to control data movement to/from the GPU. Note, the transformations are
indicative of what could be done - no claim is made as to the performance of
the resulting code.

Once you have installed PSyclone, either of the scripts may be supplied to
PSyclone via the -s option:

```sh
psyclone -api nemo -s ./kernels_trans.py ../code/tra_adv.F90
```

Executing this will output 1) the PSyclone invokes found in the code,
2) PSyclone's Schedule view of the original code, 3) PSyclone's
Schedule view of the code after adding OpenACC Kernels directives, 4)
PSyclone's Schedule view of the code after adding OpenACC Kernels and
Data directives, and 5) the transformed Fortran code with the OpenACC
directives added.

Running PSyclone with the kernels_managed_mem_trans.py script will
produce similar output but the Schedule will no longer contain Data
directives.

`tra_adv.F90` is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.

## Compiling and Execution

If desired this example may be compiled and executed on a GPU device
provided a suitable compiler with OpenACC support is available. Note
that this example is only provided to demonstrate how one adds OpenACC
directives using PSyclone with the NEMO API. It is not intended to
demonstrate how to obtain good performance.

Since `tra_adv.F90` is instrumented for use with the dl_timer library,
this library is also required. It is available from
[bitbucket.org/apeg/dl_timer](https://bitbucket.org/apeg/dl_timer).

Once dl_timer has been downloaded, the supplied Makefile must be
edited to supply the location of the library. The compiler and flags
must be specified via the F90 and F90FLAGS environment variables, e.g.
to use PGI and OpenACC:

```sh
export F90=pgf90
export F90FLAGS="-O1 -acc -ta=tesla,cc70 -Minfo=all"
export LDFLAGS="-acc -ta=tesla,cc70"
```

The size of domain and number of time-steps are also picked-up from
environment variables. Some example settings are provided in the
domain_setup.sh file.

## Licence

-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------
