# PSyclone GOcean Example 4

**Authors:** A. R. Porter and S. Siso, STFC Daresbury Lab

The directory containing this file contains an example of the use of
PSyclone to transform kernels that access variables and routines
via module use statements.

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

PSyclone can be run in the directory containing this file by 
executing, e.g.:

```sh
psyclone -api "gocean1.0" alg_kern_with_use.f90
```

This will produce 'vanilla' PSy code along with a re-written version of
the algorithm.

The example algorithm files (alg_*.f90) each contain an Invoke that
calls a single kernel. Each of these illustrates a different aspect
of kernels accessing data/routines via module use statements:

1. `alg_kern_use_var` - an example of a kernel that uses a variable
   declared in another module;

2. `alg_kern_call_kern` - a kernel that calls another routine (kernel)
   defined in another module;

3. `alg_nested_use` - a kernel that calls another routine that itself
   uses data defined in another module.

These various forms of kernel only present a problem if a user wishes
to transform them e.g. for use in either an OpenACC or OpenCL
application. For instance, we can use PSyclone with a suitable
transformation script (employing the `KernelImportsToArguments` and
`GOOpenCLTrans` transformations) to generate OpenCL code for the first
example:

```sh
psyclone -api "gocean1.0" -s ./ocl_transform.py alg_kern_use_var.f90
```

The generated PSy- and Algorithm-layer code is written to stdout and a
transformed version of the kernel is written to
kern_use_var_kern_use_var_0.cl. This kernel has the `gravity` variable
(previously accessed from a module in the original Fortran) passed by
argument:

    __kernel void kern_use_var_code(
      __global double * restrict fld,
      double gravity
      ){

Similarly, we can generate an OpenACC version of the same example by
doing:

```sh
psyclone -api "gocean1.0" -s ./acc_transform.py alg_kern_use_var.f90
```

which write the OpenACC PSy- and Algorithm-layer code to stdout with
the used kernel inlined in the PSy-layer.

However, attempting to perform the same transformation for the
`alg_kern_call_kern` example will not work - although the kernels
named in the Invoke would be transformed, the other kernels that they
then call would not and this means that they would not be compiled for
execution on the OpenACC device.
Support for recursive kernel transformation is the subject of
[Issue #342](https://github.com/stfc/PSyclone/issues/342) and will
be demonstrated by:

```sh
psyclone -api "gocean1.0" -s ./acc_transform.py alg_kern_call_kern.f90
```

Currently this raises a TransformationError because PSyclone spots
that the kernel accesses the global symbol 'my_function'.

The third example:

```sh
psyclone -api "gocean1.0" -s ./acc_transform.py alg_nested_use.f90
```

also currently raises a TransformationError for the same reason.


Additionally, this example's Makefile has the necessary rules to compile
the OpenAcc version of `alg_kern_use_var.f90`. This is done with the
`compile-acc` target. Note that this requires an OpenACC compatible compiler
(currently the Makefile only supports pgf90 - the build system flags will need
to be updated to support other OpenACC compilers) and the infrastructure libraries
must be built with the same compiler. To make sure the infrastructure and the
generated files are compiled as described, we can do:

```sh
export F90=pgf90
make allclean
make compile-acc
```


## Licence

-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------
