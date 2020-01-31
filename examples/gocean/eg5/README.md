# PSyclone GOcean Example 5

**Author:** J. Henrichs, Bureau of Meteorology

## Introduction

This is a very simple test that shows how to use the profling
support in PSyclone. It is a stand alone program that can be compiled
and run. 

## Compilation
You have to compile dl_esm_inf (which is included in external/dl_esm_inf)
and the simple template profiling library in lib/profiling/template.
Instructions for those are are given in the corresponding subdirectories.

The makefile here will invoke psyclone with the ``--profile invokes``
flag, which will add profiling around both invokes.

## Running
When running the program, you should see:
```
 ...
 ProfileInit called
 PreStart called for module 'psy_test' region 'invoke_0:r0'
 PostEnd called for module 'psy_test' region 'invoke_0:r0'
 PreStart called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
 PostEnd called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
 ...  
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
