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
The NetCDF extraction library in ``../../../../lib/extract/netcdf`` is used
as default, and will also be automatically compiled. You can set the
environment variable ``EXTRACT_DIR`` to point to a different library if 
required. More details on compiling these libraries are in the
corresponding subdirectories.
To create and compile the example, modify the Makefile if required
and type ``make``.

PSyclone is supplied with the ``extract_transform.py`` transformation script
which will add extract regions around the invokes:
```
psyclone -nodm -l -api "gocean1.0"             \
         --config ../../../../config/psyclone.cfg \
         -s ./extract_transform.py             \
         -opsy psy.f90 -oalg alg.f90 test.x90
```

This will also create two driver files, but because of #644 the
driver will not compile.

## Running
When running the program, you should see:
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

Two NetCDF files will be produced, one for each of the two invokes
instrumented with an extract region.

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
