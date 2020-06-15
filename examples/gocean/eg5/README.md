# PSyclone GOcean Example 5

**Author:** J. Henrichs, Bureau of Meteorology

## Introduction

This is a very simple test that shows how to use the profling
support in PSyclone. It is a stand alone program that can be compiled
and run. 

## Compilation
The makefile supports compiling and linking with the following PSyclone
profile wrapper libraries:
- template
- simple_timing
- dl_timer
- drhook

By default (``make`` without an argument) the ``template`` library will 
be used, which just prints the name of the regions called.
In order to test any of the other libraries, just use the
command ``make <wrapper library name>`` and use the name listed above
for ``<wrapper library name>``. The name of the executable will be
``profile_test.<wrapper library name>``. There is also a target ``make all``
which will create executables for all libraries listed above.    

You also have to compile the GOcean infrastructure library
dl_esm_inf (which is included in ``external/dl_esm_inf``), and
the corresponding profile wrapper library in ``lib/profiling``.
Instructions for those are are given in the corresponding subdirectories.
If you are using ``dl_timer`` or ``drhook``, you also need to compile these
libraries yourself, and modify the ``Makefile`` in this directory
to specify the required linking parameters. The ``Makefile``
supports the following environment variables that can be defined
to find the various software packages:

### DL_DIR:
   The location of the dl_esm_inf library, it defaults to
   ``../../../external/dl_esm_inf/finite_difference/src``,
   which is the version included in PSyclone.
### DL_TIMER_ROOT:
    The location of the apeg-dl_timer library. It defaults to
    ``../../../../apeg-dl_timer``, i.e. it is assumed that apeg-dl_timer
    is installed next to PSyclone.
    Note that until Issue #730 is complete, executing this example
    will fail as the labels produced by PSyclone are longer than
    permitted by the dl_timer library.
### DRHOOK_DIR:
    The location of DrHook. It defaults to
    ``../../../../drhook``, i.e. it is assumed that DrHook is
    installed next to PSyclone.

The makefile here will invoke psyclone with the ``--profile invokes``
flag, which will add profiling around the two invokes used in the example.

### Note:
The actual runtime is extremly short, so likely the profiling
tool used will report 0 seconds for each of the invokes.

## Running
The output will depend on the wrapper library used. For the ``template``
library, you should see:
```
 ...
 profile_PSyDataInit called
 ...
 PreStart called for module 'psy_test' region 'invoke_0:r0'
 PostEnd called for module 'psy_test' region 'invoke_0:r0'
 PreStart called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
 PostEnd called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
 ...  
 profile_PSyDataShutdown called
```

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2020, Science and Technology Facilities Council.
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
