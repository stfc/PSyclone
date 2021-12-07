# PSyclone PSyAD Example1: creating an adjoint kernel and test harness.

**Author:** A. R. Porter, STFC Daresbury Lab
**Modified by:** R. W. Ford, STFC Daresbury Lab

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

This example demonstrates the use of PSyAD to create the adjoint of a
simple kernel (contained in `testkern_mod.f90`).
It also demonstrates the creation of a test harness for the adjoint kernel.

PSyAD can be run in the directory containing this file by executing, e.g.

```sh
make
```

Alternatively, PSyAD may be run from the command line as:

```sh
psyad -t -otest test_harness.f90 -oad testkernadj_mod.f90 testkern_mod.f90
```

This will generate two new files, `testkernadj_mod.f90` and `test_harness.f90`.

The Makefile also supports the `compile` target which will build
the kernel, its adjoint and the test harness. The `run` target will execute
the test harness giving output something like:

```sh
Running PSyAD-generated test harness...
 Test of adjoint of 'testkern_code' passed: diff =    0.0000000000000000
...done.
```

Note, you may find that the test fails, but if so the diff should be
relatively small.

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2021, Science and Technology Facilities Council.
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
