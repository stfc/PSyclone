# PSyclone NEMO Example 9 - Value Range Check

**Author:** J. Henrichs, Bureau of Meteorology


This example demonstrates the use of PSyclone to add code to verify
variable value ranges, i.e. ensuring that variables before and after
an instrumented region have values in a user-specified range.

 
```sh
psyclone -l all -s ./value_range_check_transformation.py ../code/tra_adv.F90
```

Executing this will output the transformed Fortran code with the 
value range code added. Note that some of the lines in this
Fortran code will exceed the 132-character limit. This may be remedied
by supplying the `-l all` flag to PSyclone (as is done in the Makefile).


The generic value range check library in
``../../../lib/value_range_check/generic`` is used. The binary
instrumented for range_check will be called ``traadv.exe``.
To create and compile the example, type ``make compile``.

`tra_adv.F90` is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.

## Compiling and Execution

This example can be compiled and executed. At execution time, the
environment variable `PSY_VALUE_RANGE` *must* be specified.
Example:

```sh
    PSY_VALUE_RANGE="umask=0.0:0.9" IT=2 JPI=10 JPJ=10 JPK=5 ./traadv.exe  
    PSyData: Variable 'umask' has the value '1.0000000000000000' at index/indices 10 10 5 in module 'tra_adv', region 'r0', which is not between '0.0000000000000000' and '0.90000000000000002'.

```
Note that umask is expected to be between 0 and 1, the range was only specified
to be between 0 and 0.9 to show the warning message that would be printed.

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2026, Science and Technology Facilities Council.
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
