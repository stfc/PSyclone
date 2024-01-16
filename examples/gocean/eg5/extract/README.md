# PSyclone GOcean PSyData Kernel-Extraction Example

**Author** J. Henrichs, Bureau of Meteorology

## Introduction

This is a very simple test that shows how to use the kernel-data extraction
support in PSyclone. It is a stand-alone program that can be compiled
and run.

## Compilation
This example needs two libraries: the gocean infrastructure library
dl_esm_inf, and a corresponding extraction library. By default
it will use the infrastructure library in ``../../../../external/dl_esm_inf``
and automatically compile this library
(see https://psyclone-dev.readthedocs.io/en/latest/working_practises.html
for the correct way of checking out all required software) . You can set
the environment variable ``INF_DIR`` to point to a different directory.

The stand-alone extraction library in
``../../../../lib/extract/standalone/dl_esm_inf`` is used as default, and
will also be automatically compiled. You can also use the NetCDF based
extraction library by setting the environment variable `TYPE` to `netcdf`
when calling `make`, e.g.:

    $ TYPE=netcdf make compile

This requires NetCDF to be available (including ``nf-config`` to detect
installation-specific paths). The NetCDF-based extraction library in
``../../../../lib/extract/netcdf/dl_esm_inf``
will also be automatically compiled.

The binary  instrumented for extraction will either be called
``extract_test.standalone`` or ``extract_test.netcdf``.
More details on compiling these libraries are in the corresponding
subdirectories. To create and compile the example, type ``make compile``.

This example uses the ``extract_transform.py`` transformation script
which will add extract regions around the invokes:
```
psyclone -nodm -l -api "gocean1.0"             \
         --config ../../../../config/psyclone.cfg \
         -s ./extract_transform.py             \
         -opsy psy.f90 -oalg alg.f90 test.x90
```

This will also create two driver files, which can read the corresponding
output files, call the kernel, and verify that the same output values are
computed. These drivers will be compiled by the Makefile as well and will
be named ``driver-main-init.standalone/netcdf`` and
``driver-main-update.standalone/netcdf``.


## Running
When running the instrumented program, you should see:
```
parallel_init: Not running with MPI
go_decompose: using grid of   1x  1
Tile width =    3, tile height =    3
...
Allocating C-T field with bounds: (1: 12,1:  6)
Internal region is:(2:  4,2:  4)
Grid has bounds:  (1: 12,1:  6)
   15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000     
```

When running the driver program, all the output variables will be listed
together with either 'correct', or 'incorrect' (and the actual and
expected) values:
```
./driver-main-init.standalone
 a_fld correct
 b_fld correct
 c_fld correct
 d_fld correct
 i correct
 j correct
```

When using the stand-alone extraction library, two binary files called
``main-update.binary`` and ``main-init.binary`` will be created, one for
each of the instrumented invoke regions. When using the NetCDF-based
extraction library, these files will be called ``main-update.nc`` and
``main-init.nc``. The NetCDF files can be inspected, for example with
``ncdump``:

    ncdump ./main-update.nc  | less
    netcdf main-update {
    dimensions:
            a_flddim%1 = 6 ;
            a_flddim%2 = 6 ;
            b_flddim%1 = 6 ;
            b_flddim%2 = 6 ;
            c_flddim%1 = 6 ;
            c_flddim%2 = 6 ;
            d_flddim%1 = 6 ;
            d_flddim%2 = 6 ;
            a_fld_postdim%1 = 6 ;
            a_fld_postdim%2 = 6 ;
    variables:
            double a_fld(a_flddim%2, a_flddim%1) ;
    ...


## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2020-2024, Science and Technology Facilities Council.
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

------------------------------------------------------------------------------
