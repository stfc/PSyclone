# PSyclone GOcean Example 4

**Authors:** A. R. Porter, STFC Daresbury Lab

The directory containing this file contains an example of the use of
PSyclone to transform kernels that access variables and routines
via module use statements.

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

PSyclone can be run in the directory containing this file by 
executing, e.g.:

```sh
psyclone -api "gocean1.0" alg.f90
```

This will produce 'vanilla' PSy code along with a re-written version of
the algorithm.

The example algorithm file (alg.f90) contains a single Invoke that
calls three kernels. Each of these illustrates a different aspect
of kernels accessing data/routines via module use statements:

1. `kern_use_var` - an example of a kernel that uses a variable
   declared in another module;

2. `kern_call_kern` - a kernel that calls another routine (kernel) defined
   in another module;

3. `kern_nested_use` - a kernel that calls another routine that itself
   uses data defined in another module.

These various forms of kernel only present a problem if a user wishes
to transform them e.g. for use in either an OpenACC or OpenCL
application. For instance, the following:

```sh
psyclone -api "gocean1.0" -s ./acc_transform.py alg.f90
```

causes PSyclone to raise a `TransformationError`. This is because the
`data_mod::gravity` variable read by `kern_use_var` is not available on
the remote device. This problem is the subject of [Issue #323](https://github.com/stfc/PSyclone/issues/323) which
proposes to extend PSyclone to convert any module data accessed by a
kernel into a kernel argument. In addition, although the kernels
named in the Invoke would be transformed, the other kernels that they
then call have not and this means that they won't be compiled for
execution on the OpenACC device. Support for recursive kernel
transformation is the subject of [Issue #342](https://github.com/stfc/PSyclone/issues/342).

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2019, Science and Technology Facilities Council.
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
