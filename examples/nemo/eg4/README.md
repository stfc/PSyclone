# PSyclone NEMO Example 4 - SIR

**Authors:** R. W. Ford, STFC Daresbury Lab

The SIR and its Dawn back-end come with three examples which create
SIR using its Python Interface.

The PSyclone SIR back end translates PSyIR to SIR and prints out the
required calls to the Dawn Python interface.

## PSyclone SIR generation

In order to test the SIR backend, the equivalent Fortran to the
examples provided in Dawn are available in this directory
(`copy_stencil.f90`, `hori_diff.f90` and `tridiagonal_solve.f90`). When run
through PSyclone they should produce the same (or equivalent) SIR as
is in the examples. This then validates that the translation works
correctly.

An additional example (`if_example.f90`) extracted from a tracer
advection benchmark (which itself was extracted from the NEMO code
base) tests the ability of PSyclone and Dawn to deal with IF
statements.

To test this, run:

```sh
> psyclone -s ./sir_trans.py -api nemo <filename> -opsy /dev/null
```

## Building Dawn

PSyclone has been tested with Dawn master commit hash
568375f8bf3bdb064d006d958323b1b8e31b726e on Monday 9th September 2019.

This version contains a bug which causes the `if_example.f90` example to
fail. This is fixed in the latest version of Dawn master.

To build Dawn with Python support:

```sh
> git clone https://github.com/MeteoSwiss-APN/dawn.git
> cd dawn/bundle
> mkdir build
> cd build
> cmake -DDAWN_BUNDLE_PYTHON=ON -DDAWN_PYTHON_EXAMPLES=ON ..
> make -j8 install
> export PYTHONPATH=<path>/dawn/bundle/install/python
```

## Running existing Dawn examples

```sh
> export PYTHONPATH=<path>/dawn/bundle/install/python
> cd dawn/bundle/install/examples/python
> python3 [copy_stencil.py, hori_diff.py, tridiagonal_solve.py]
> ls data/[copy_stencil.cpp, hori_diff.cpp, tridiagonal_solve.cpp]
```

## Running PSyclone-generated code in Dawn

1. Add the generated code inbetween the "PSyclone code start" and
   "PSyclone code end" comments in the supplied `dawn_script.py` file
   (in the same directory as this README).
2. Run: `cp <path>/dawn/bundle/install/examples/python/config.py .`
3. Run: `export PYTHONPATH=<path>/dawn/bundle/install/python`
4. Run the script with python3: `python3 dawn_script.py`
5. Cuda code will be output in `data/psyclone.cpp`

## Issues/limitations

1. Loop bounds are not analysed yet so it is not possible to add in
   offset and loop ordering for the vertical.
2. There are no checks that the loops conform to the NEMO lat.lon.levs
   convention.
3. Literals are assumed to be floating point numbers (as the PSyIR
   does not currently capture the type of a literal).
4. Fortran literals such as `0.0d0` are output directly in the
   generated cuda code.
5. The only unary operator currently supported is '-'.
6. The subject of the unary operator must be a literal.
7. Loops must be triply nested.
8. Loops must be perfectly nested (no computation between different
   loop levels).

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
