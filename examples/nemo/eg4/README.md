.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2021, Science and Technology Facilities Council.
.. All rights reserved.
..
.. Redistribution and use in source and binary forms, with or without
.. modification, are permitted provided that the following conditions are met:
..
.. * Redistributions of source code must retain the above copyright notice, this
..   list of conditions and the following disclaimer.
..
.. * Redistributions in binary form must reproduce the above copyright notice,
..   this list of conditions and the following disclaimer in the documentation
..   and/or other materials provided with the distribution.
..
.. * Neither the name of the copyright holder nor the names of its
..   contributors may be used to endorse or promote products derived from
..   this software without specific prior written permission.
..
.. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
.. "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
.. LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
.. FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
.. COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
.. INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
.. BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
.. LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
.. CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.. LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
.. ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
.. POSSIBILITY OF SUCH DAMAGE.
.. -----------------------------------------------------------------------------
.. Written by: R. W. Ford, STFC Daresbury Lab

# PSyclone NEMO Example 4 - SIR

The SIR and its Dawn back-end include three simple (Python Interface)
examples. In this directory these three examples are provided in
Fortran and PSyclone scripts are provided which translate the Fortran
code back into SIR (DAWN Python interface code), which can then be run
through the Dawn back end.

We are also working towards supporting the translation of the NEMO
Dwarf (being implemented in the ESCAPE2 project) from Fortran to SIR
(DAWN Python interface code). As the NEMO Dwarf is not yet available
we are currently concentrating on the tracer advection benchmark
(tra_adv) as the codes are very similar. There are three examples
associated with this which are described below.

## PSyclone SIR generation

In order to test the SIR backend, the equivalent Fortran to the
examples provided in Dawn are available in this directory
(`copy_stencil.f90`, `hori_diff.f90` and
`tridiagonal_solve.f90`). When run through PSyclone they should
produce the same (or equivalent) SIR as is in the examples. This then
validates that the translation works correctly.

Two additional examples (`if_example.f90` and `intrinsic_example.f90`)
both extracted from a tracer advection benchmark (which itself was
extracted from the NEMO code base and will form the basis of the NEMO
Dwarf benchmark) test the ability of PSyclone to a) translate if
statements to SIR and b) translate Fortran `min`, `abs` and `sign`
intrinsics to equivalent PSyIR code before translating to SIR (as SIR
does not support intrinsics).

A third additional example (`tra_adv_compute.F90`) is the
computational part of the tracer advection benchmark and this shows
how much of the code can currently be translated (with the parts that
can't be translated being modified or commented out).

To test the `copy_stencil.f90`, `hori_diff.f90`,
`tridiagonal_solve.f90` and `if_example.f90` examples run:

```sh
> psyclone -s ./sir_trans.py -api nemo <filename> -opsy /dev/null
```

To test the `intrinsic_example.f90` and `tra_adv_compute.F90` examples run:

```sh
> psyclone -s ./sir_trans_all.py -api nemo <filename> -opsy /dev/null
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
3. The only unary operator currently supported is '-'.
4. Loops must be triply nested.
