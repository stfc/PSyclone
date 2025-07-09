# PSyclone NEMO Example 7 - Kernel Data Extraction

**Author:** J. Henrichs, Bureau of Meteorology

This example demonstrates the use of PSyclone to add code for kernel
extraction, i.e. writing input- and output-data of a kernel into a data file.

Once you have installed PSyclone, you can create the version with
data instructions using the ``-s`` option:

```sh
psyclone -l all  -s ./extract_kernels.py ../code/tra_adv.F90
```

Or you can just execute ``make``, which will generate a file ``psy.f90``.

Executing this command will output the transformed Fortran code with the 
kernel extraction code added. Note that some of the lines in this
Fortran code would exceed the 132-character limit, which is remedied
by supplying the `-l all` flag to PSyclone (as is done in the Makefile).


## Compilation

This example can be compiled by using ``make compile``, and then executed.
The following environment variables can be set to define the compiler you
want to use:

```shell
export F90=gfortran
export F90FLAGS="-Wall -g -fcheck=bound"
```
They default to use ``gfortran`` with ``-g -O0``.

The three different extraction libraries, as described in
[PSyKE extraction libraries](https://psyclone.readthedocs.io/en/stable/psyke.html#extraction-libraries)
can be linked in.  The stand-alone extraction library in
``../../../lib/extract/binary/nemo`` is used as default, and
will also be automatically compiled. You can set the ``TYPE`` environment
variable to either ``ascii``
or ``netcdf`` when compiling to use the other libraries::

    $ TYPE=netcdf make compile
    $ TYPE=ascii make compile

 To compile the example, the following dependencies are needed:
- one of the LFRic PSyData wrapper libraries, either:
    - ``lib_kernel_data_netcdf`` from
      ``<PSYCLONEHOME>/lib/extract/netcdf/lfric`` and NetCDF,
    - ``lib_kernel_data_binary`` from
      ``<PSYCLONEHOME>/lib/extract/binary/lfric``, or
    - ``lib_kernel_data_ascii`` from
      ``<PSYCLONEHOME>/lib/extract/ascii/lfric``

The infrastructure and PSyData wrapper libraries will be compiled
if they are not available, the NetCDF library requires NetCDF to
be available (including ``nf-config`` to detect installation-specific
paths).

The binary  instrumented for extraction will either be called
``traadv-binary.exe``, ``traadv-ascii.exe`` or
``traadv-netcdf.exe``.

`tra_adv.F90` is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.

## Execution

This example can be compiled and executed, resulting in several
data files created. The size of domain and number of time-steps are also
picked-up from environment variables. Some example settings are provided
in the `domain_setup.sh` file. Using ``make run`` will run a small
test using:

```shell
export F90=gfortran
IT=2 JPI=10 JPJ=10 JPK=5  ./traadv-binary.exe
```
which is a very fast run. This example will create 14 data files, e.g.
``tra_adv-r0.binary``, ..., ``tra_adv-r13.binary`` for the stand-alone
binary format. The files will be called ``.ascii`` for the stand-alone
ASCII library, and ``.nc`` for the NetCDF format.


Note that driver creation is not yet supported in NEMO, see issue #2058.

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2025, Science and Technology Facilities Council.
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
