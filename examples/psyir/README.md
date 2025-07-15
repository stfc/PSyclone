<!--
BSD 3-Clause License

Copyright (c) 2019-2025, Science and Technology Facilities Council.
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

Authors: R. W. Ford, A. R. Porter and S. Siso STFC Daresbury Lab
         M. Naylor, University of Cambridge, UK
-->

# PSyclone PSyIR Examples

This directory contains examples of how to create and/or modify
instances of PSyIR and how to use backends to transform them into
code.

All of these examples require PSyclone to be installed.

## Example 1:

Create an instance of PSyIR using many of the generic PSyIR nodes and
output the resultant tree as Fortran and C. Currently the C
backend does not support all of the node types so it only outputs a
subset of the tree. This example may be run by doing:

```sh
> python create.py
```

## Example 2:

Demonstrates how to create and manipulate structure types (a.k.a.
derived types in Fortran) within the PSyIR.
To run this example:

```sh
> python create_structure_types.py
```

## Example 3:

Demonstrates how to manipulate an existing PSyIR tree. This example
imports the PSyIR created in Example 1, applies some modifications
to it and then outputs the modified PSyIR as Fortran code. This example may
be run by doing:

```sh
> python modify.py
```

## Example 4:

Demonstrates acceleration of a simple routine for matrix transposition using
`LoopTilingTrans` and `OMPLoopTrans`. To run this example:

```sh
> cd transpose
> make
./trans
Passed 0.7202s
./trans_tiled
Passed 0.2420s
./trans_omp
Passed 0.2996s
./trans_omp_tiled
Passed 0.0445s
```

(Sample output from a 20-core Intel i9-12900H.)

## Example 5:

Demonstrates acceleration of a simple routine for matrix multiplication using
`LoopTilingTrans` and `OMPLoopTrans`. To run this example:

```sh
> cd matmul
> make
./matmul
Passed   3.213s
./matmul_tiled
Passed   3.291s
./matmul_omp
Passed   1.134s
./matmul_omp_tiled
Passed   0.306s
```

(Sample output from a 20-core Intel i9-12900H.)
