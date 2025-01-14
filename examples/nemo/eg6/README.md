# PSyclone NEMO Example 6 - Read-only Verification

**Author:** J. Henrichs, Bureau of Meteorology

This example demonstrates the use of PSyclone to add code that checks
if variables that are only read are actually modified (e.g. because
of memory overwrite).

Once you have installed PSyclone, you can transform the file using:

```sh
psyclone -s ./read_only_check.py dummy.f90
```

Executing this will output the transformed Fortran code with the 
read-only-verification code added. 

The generic read-only verification library in
``../../../lib/read_only/generic`` is used, and will also be
automatically compiled if required.

## Compiling and Execution

To create and compile the example, type ``make compile``.
This example can be compiled and executed. It will report nothing,
since no read-only variable is overwritten. But you can verify that
the variables are checked by setting ``PSYDATA_VERBOSE=2``:

```sh
$ PSYDATA_VERBOSE=2 ./dummy 
 PSyData: PreStart dummy r0
 PSyData: DeclareScalarChar: dummy r0: char_var
 PSyData: DeclareScalarLogical: dummy r0: logical_var
 PSyData: DeclareScalarChar: dummy r0: char_var
 PSyData: DeclareScalarLogical: dummy r0: logical_var
 PSyData: checked variable char_var
 PSyData: checked variable logical_var
 PSyData: PostEnd dummy r0
   3.00000000     F
```

If you copy the lines 68 and 69 from ``dummy.f90`` into ``psy.f90``,
the code will modify ``logical_var`` (by using out-of-bound array accesses.
Or you could just manually set ``logical_var = .true.``). If you then
compile again (using `make compile`, otherwise the original file would
get processed again, overwriting your changes), an error will be produced.

```sh
$ ./dummy 
 ------------- PSyData -------------------------
 Logical(kind=4) variable logical_var has been modified in dummy : r0
 Original value:  F
 New value:       T
 ------------- PSyData -------------------------
   3.00000000     T
```

Note that adding the assignment to ``logical_var`` as above to the original
``dummy.f90`` file would mean that ``logical_var`` is not a read-only variable
anymore, so no test and therefore no error will be produced for this variable.
The code commented out can also not be processed by PSyclone (missing support
for ``loc`` and ``sizeof``, which are non-standard Fortran extensions).

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
