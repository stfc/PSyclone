# PSyclone NEMO Example 5 - Kernel Data Extraction

**Author:** J. Henrichs, Bureau of Meteorology


This example demonstrates the use of PSyclone to add code for kernel
extraction, i.e. writing input- and output-data of a kernel into a data file.

Once you have installed PSyclone, either script may be supplied to
PSyclone via the -s option, e.g.:

```sh
psyclone -l all -api nemo -s ./extract_kernels.py ../code/tra_adv.F90
```

Executing this will output the transformed Fortran code with the 
kernel extraction code added. Note that some of the lines in this
Fortran code will exceed the 132-character limit. This may be remedied
by supplying the `-l all` flag to PSyclone (as is done in the Makefile).


The stand-alone extraction library in
``../../../lib/extract/standalone/nemo`` is used as default, and
will also be automatically compiled. You can also use the NetCDF based
extraction library by setting the environment variable `TYPE` to `netcdf`
when calling `make`, e.g.:

    $ TYPE=netcdf make compile

This requires NetCDF to be available (including ``nf-config`` to detect
installation-specific paths). The NetCDF-based extraction library in
``../../../../lib/extract/netcdf/nemo``
will also be automatically compiled.

The binary  instrumented for extraction will either be called
``traadv-standalone.exe`` or ``traadv-netcdf.exe``.
More details on compiling these libraries are in the corresponding
subdirectories. To create and compile the example, type ``make compile``.

`tra_adv.F90` is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.

## Compiling and Execution

This example can be compiled and executed, resulting in several
data files created. The size of domain and number of time-steps are also
picked-up from environment variables. Some example settings are provided
in the `domain_setup.sh` file.

Note that driver creation is not yet supported in NEMO, see issue #2058.

## Licence

-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------
