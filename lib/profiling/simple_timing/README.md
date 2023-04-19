# Simple Stand-alone Timer Library

This library is a simple stand-alone timer library (see the ["Profiling"](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling) section
in the PSyclone [User Guide](https://psyclone.readthedocs.io/en/stable/) for
more details). It counts the number of calls for each region, and reports
minumum, maximum and average times. This library is **not thread-safe**, and
**not MPI-aware** (e.g. maximum reported is per process, not across all
processes).

## Dependencies

This stand-alone profiling library uses the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html) to interface with
the application. The library is based on the [PSyData base class](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psydata-base-class),
which is included in PSyclone as a Jinja template, ``psy_data_base.jinja``.
Full documentation on using this template is provided in the PSyclone
[Developer Guide](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja). The
script [``process.py``](./../../README.md#psydata-base-class) is used to
process this template.

Since the profiling API does not need access to any fields or variables,
only the static subroutines and ``PreStart`` and ``PostEnd`` are implemented;
the ``PreDeclare`` and ``ProvideVariable`` methods are not created at all.

The library uses the ``ProfileData`` type to store the module/region name
(done by the base class) and then stores timing and counting information in
this derived type.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use, e.g.

```shell
F90=gfortran F90FLAGS=-g make
```

The compiler flags default to ``gfortran`` and the empty string.

The location of the PSyData base class Jinja template,
``psy_data_base.jinja`` is specified using the environment variable
``$PSYDATA_LIB_DIR``. It defaults to the relative path to the
top-level [``lib``](./../../) directory.

The compilation process will create the wrapper library ``libsimple_timing.a``.

### Linking the wrapper library

In order to link this timer library with your application, the location of
this library must be provided as an ``include`` path (so that the module
file is found). Also, the library name, ``simple_timing``, must be specified
at link time:

```shell
$(F90) -c  -I $(PSYDATA_LIB_DIR)/profiling/simple_timing some_file.f90
$(F90) some_file.o -L $(PSYDATA_LIB_DIR)/profiling/simple_timing -lsimple_timing
```

## Output

The output is written to the command line. A sample output is below:

```
===========================================
module::region                                         count           sum                     min             average                 max
psy_inputoutput::eliminate_one_node_islands_code           1     0.128906250             0.128906250             0.128906250             0.128906250    
psy_time_step_mod::swlon_adjust_code                      11      1.19921875             0.105468750             0.109019883             0.113281250    
psy_time_step_mod::swlon_code                             11      4.38281250             0.394531250             0.398437500             0.406250000    
psy_time_step_mod::swlon_update_code                      11      1.86718750             0.167968750             0.169744313             0.171875000    
psy_time_step_mod::swlat_adjust_code                      11      1.23828125             0.109375000             0.112571023             0.117187500    
psy_time_step_mod::swlat_code                             11      4.87890625             0.437500000             0.443536937             0.445312500    
psy_time_step_mod::swlat_update_code                      11      1.87500000             0.167968750             0.170454547             0.179687500    
===========================================
```

<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2019-2021, Science and Technology Facilities Council.
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

-------------------------------------------------------------------------------
Authors: J. Henrichs, Bureau of Meteorology,
         I. Kavcic, Met Office
-->
