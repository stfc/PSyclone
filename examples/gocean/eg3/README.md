# PSyclone GOcean Example 3

**Authors:** A. R. Porter and S. Siso, STFC Daresbury Lab

The directory containing this file contains an example of the use of
PSyclone to generate OpenCL code with the GOcean 1.0 API.

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

PSyclone can be run in the directory containing this file by 
executing, e.g.:

```sh
psyclone -api "gocean1.0" alg.f90
```

This will generate 'vanilla' PSy-layer code which is output to stdout.

In order to generate an OpenCL PSy layer instead, PSyclone must be
provided with a transformation script::

```sh
psyclone -api "gocean1.0" -s ./ocl_trans.py alg.f90
```

where `ocl_trans.py` simply applies the `psyclone.transformations.OCLTrans`
transformation to the Schedule of the Invoke. This will generate the OpenCL
driver layer to stdout and a 'kernel_name'.cl file for each of the kernels
referenced in alg.f90 translated to OpenCL.

Each OpenCL kernel needs to be compiled before buidling the driver layer.
For example, the steps to generate the code using the Intel OpenCL SDK
(https://software.intel.com/en-us/opencl-sdk) are::

```sh
psyclone -oalg psyalg.f90 -opsy psylayer.f90 -api "gocean1.0" \
    -s ./ocl_trans.py alg.f90

# Pre-build OpenCL kernels
ioc64 -cmd=build -device=cpu -input=kernels.cl -spirv64=kernels.spirv \
    -bo="-cl-std=CL1.2"
export PSYCLONE_KERNELS_FILE=kernels.spirv
```

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2018, Science and Technology Facilities Council
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
