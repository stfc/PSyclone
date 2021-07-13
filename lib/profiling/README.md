# Libraries for Use with PSyclone Profiling

This directory contains wrapper libraries that can be used with [PSyclone
profiling API](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling). All
profiling-library interfaces use the the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html). The profiling
wrappers included in PSyclone are: ``template``,
``simple_timing``, ``dl_timer``, ``drhook``, ``nvidia`` and
``lfric_timer``. the overview is given below (for more information please
refer to the linked individual ``README.md`` documents).

## Profiling Wrappers

### [``template``](./template)

This is a very simple example library that just prints the name of the
subroutines used (e.g. ``ProfileStart``) and name of the module and region.
It uses the ``ProfileData`` variables to make the module and region name
available in the ``ProfileEnd`` call.

Detailed building and linking instructions are in
[``template/README.md``](./template/README.md).

### [``simple_timing``](./simple_timing)

This is a simple, stand-alone library that measures the real time of
a region, and prints a summary at the end. It is **NOT thread-safe**.
The ``ProfileData`` type is used to store module name and region,
and time accumulators.

Detailed building and linking instructions are in
[``simple_timing/README.md``](./simple_timing/README.md).

Example output:

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

### [``dl_timer``](./dl_timer)

This is a wrapper library that maps the [PSyclone profiling API](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling) to the
dl_timer API. A copy of dl_timer can be downloaded from
https://bitbucket.org/apeg/dl_timer.

The PSyclone dl_timer wrapper library uses the ``ProfileData`` type and
dl_timer's timer_register function to store the module/region name and
the index used by dl_timer. This library is **thread-safe**.

Detailed building and linking instructions are in [``dl_timer/README.md``](
./dl_timer/README.md).

Example output:

```
    =============================== Timing report ===============================
    Timed using POSIX timer. Units are seconds.
    Reported resolution =  0.1000E-08 (s)
    Effective clock granularity =  0.25997E-07 (s)
    Measured systematic error in dl_timer API =  0.37790E-07 +/- 0.789E-09 (s)
    Measured overhead in calling start/stop =  0.9411E-07 (s)
    Measured overhead in calling start/stop for registered timer =  0.4725E-07 (s)
    -----------------------------------------------------------------------------
    Region                          Counts     Total       Average*     Std Err
    -----------------------------------------------------------------------------
    psy_inputoutput:eliminate_one_no     1  0.12603E+00   0.12603E+00  0.00E+00
    psy_time_step_mod:swlon_adjust_c    11  0.12201E+01   0.11092E+00  0.28E-02
    psy_time_step_mod:swlon_code        11  0.44050E+01   0.40046E+00  0.25E-02
    psy_time_step_mod:swlon_update_c    11  0.18761E+01   0.17056E+00  0.45E-03
    psy_time_step_mod:swlat_adjust_c    11  0.12325E+01   0.11204E+00  0.53E-03
    psy_time_step_mod:swlat_code        11  0.50031E+01   0.45483E+00  0.26E-02
    psy_time_step_mod:swlat_update_c    11  0.19000E+01   0.17272E+00  0.24E-02
    -----------------------------------------------------------------------------
    * corrected for systematic error
    =============================================================================
```

### [Dr Hook](./drhook)

This wrapper library interfaces with the ECMWF Dr Hook library. This
library appears not to be available as open source on a public
server. It provides more functionality than just profiling, see the Dr
Hook documentation for details. The version tested here is 1.0.0.

Detailed building and linking instructions are in [``drhook/README.md``](
./drhook/README.md).

Example profiling output (some spaces removed to shorten the lines):

```
    No. of instrumented routines called : 9
    Instrumentation started : 20190124 191207
    Instrumentation   ended : 20190124 191319
    Instrumentation overhead: 0.00%
    Memory usage : 20 MBytes (heap), 20 MBytes (rss), 0 MBytes (stack), 0 (paging)
    Wall-time is 29.17 sec on proc#1 (1 procs, 1 threads)
    Thread#1:       29.17 sec (100.00%)
    
    # % Time   Cumul     Self   Total  # of calls   Self       Total    Routine@<thread-id>
                                                     (Size; Size/sec; Size/call; MinSize; MaxSize)
       (self)  (sec)    (sec)   (sec)            ms/call     ms/call

    1  31.69    9.243   9.243    9.243    11   840.27    840.27   swlat_mod:swlat_code@1
    2  28.50   17.554   8.312    8.312    11   755.59    755.59   swlon_mod:swlon_code@1
    3  12.47   21.191   3.636    3.636    11   330.58    330.59   swlat_update_mod:swlat_update_code@1
    4  11.73   24.612   3.421    3.421    11   311.01    311.02   swlon_update_mod:swlon_update_code@1
    5   7.74   26.869   2.257    2.257    11   205.15    205.15   swlat_adjust_mod:swlat_adjust_code@1
    6   7.59   29.083   2.214    2.214    11   201.27    201.27   swlon_adjust_mod:swlon_adjust_code_1@1
    7   0.28   29.165   0.082    0.082     1    82.32     82.32   eliminate_one_node_islands_mod:eliminate_one_node_islands_code_1@1
    8   0.00   29.165   0.000   29.083    11     0.01   2643.90   swlon_adjust_mod:swlon_adjust_code@1
    9   0.00   29.165   0.000    0.082     1     0.01     82.33   eliminate_one_node_islands_mod:eliminate_one_node_islands_code@1
```

### [NVIDIA](./nvidia)

This wrapper library uses the NVIDIA Tools Extension (NVTX) to mark-up
profiling regions so that they appear in the NVIDIA profiling tools
(``nvprof`` or the visual profiler, ``nvvp``).  This is then very useful for
identifying regions of an application that are not running on the GPU.

Detailed building and linking instructions are in
[``nvidia/README.md``](./nvidia/README.md).

Example output (from ``nvprof``):

```
    ==1678== NVTX result:
    ==1678==   Thread "<unnamed>" (id = 66653056)
    ==1678==     Domain "<unnamed>"
    ==1678==       Range "inc_field_mod:inc_field_code"
                Type  Time(%)      Time  Calls       Avg       Min       Max  Name
              Range:  100.00%  758.78us     10  75.878us  43.322us  348.23us  inc_field_mod:inc_field_code
     GPU activities:   72.71%  58.911us     34  1.7320us  1.3760us  8.3520us  [CUDA memcpy HtoD]
                       22.12%  17.920us     10  1.7920us  1.6320us  3.0080us  invoke_0_inc_field_36_gpu
                        5.17%  4.1920us      3  1.3970us  1.3760us  1.4080us  [CUDA memset]
          API calls:   59.10%  122.02us     34  3.5880us  2.4540us  14.057us  cuMemcpyHtoDAsync
                       34.73%  71.711us     10  7.1710us  4.6830us  25.625us  cuLaunchKernel
                        6.17%  12.729us      3  4.2430us  2.3700us  7.7330us  cuMemsetD32Async
```

### [LFRic timer](./lfric_timer)

This wrapper library uses the LFRic timer object. It can not only be
used with LFRic, but also with any other program - detailed linking
instructions are in [``lfric_timer/README.md``](./lfric_timer/README.md).

The output is written to the file ``timer.txt``, which will be overwritten if
it should already exist. For example:

```
    ||=           Routine            =||=   min time(s)     =||=   mean time(s)    =||=   max time(s)     =||=     No. calls     =||=       %time       =||= time per call(s)  =||
    ||            psy_test:invoke_0:r0||                 0.00||                 0.00||                 0.00||                    1||               100.00||                 0.00||
    ||psy_test:invoke_1_update_field:u||                 0.00||                 0.00||                 0.00||                    1||                44.47||                 0.00||
```

## Compilation

The top level ``Makefile`` can be used to compile the profiling-library
interfaces included in PSyclone. The command ``make TARGET`` where ``TARGET``
is one of ``template``, ``simple_timing``, ``dl_timer``, ``drhook``, ``nvidia``
or ``lfric_timer`` will only compile the corresponding library interface.

**Note** that compilation currently does not include ``dl_timer``, ``drhook``
and ``nvidia`` profiling-library interfaces since they require external
libraries to be available. For the same reason, the ``all`` target

```shell
make all
```

will only compile ``template``, ``simple_timing`` and ``lfric_timer``
libraries.

The following ``Makefile`` variables are used and can be overwritten on the
command line (e.g. ``F90=mpif90 make``):

 Variable   |  Default         | Description
 ---------- | ---------------- | -----------------------------
F90         |  gfortran        | Name of the compiler.
F90FLAGS    |  -g              | Flags to use when compiling.

Using ``make clean`` will clean all compiled library interfaces. Each
profiling library interface can be compiled individually by changing into
the corresponding directory and invoking ``make`` with the library-specific
flags (see the individual ``README.md`` documents for reference).

Since any source code instrumented for profiling will now contain ``use
profile_mod`` statements, the location of the ``profile_mod.mod`` file
must be provided as an ``include`` path when compiling the application
source.

<!--
## Licence

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
Authors: J. Henrichs, Bureau of Meteorology,
         I. Kavcic, Met Office
-->
