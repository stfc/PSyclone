# PSyclone Wrapper Library for Vernier

This is a wrapper library that maps the [PSyclone profiling API](
https://psyclone.readthedocs.io/en/latest/user_guide/profiling.html#profiling)
to [Vernier](https://github.com/MetOffice/Vernier).

## Dependencies

Vernier  must be installed.

This profiling library uses the [PSyData API](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html) to interface with
the application. The library is based on the [PSyData base class](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#psydata-base-class),
which is included in PSyclone as a Jinja template, ``psy_data_base.jinja``.
Full documentation on using this template is provided in the PSyclone
[Developer Guide](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#jinja).

The library uses the ``ProfileData`` type to store a Vernier handle for each
region.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

To compile the PSyclone wrapper library for Vernier, one of the following
two ``Makefile`` variables must be set to specify the path to the Vernier
installation:
VERNIER_ROOT ?= ./../../../../Vernier

- ``VERNIER_ROOT``, the path to the Vernier root directory in which
  Vernier is compiled and installed. It defaults to ``./../../../../Vernier``
  in the ``Makefile`` (i.e., it assumes Vernier is installed next to a PSyclone
  repository clone). This will set ``VERNIER_MODULES`` to
  ``.../Vernier/local/include``, so that the ``*.mod`` for Vernier
  can be found.

- ``VERNIER_MODULES``: Setting these environment
  variables explicitly will allow to flexibly point to an existing
  Vernier installation.

For instance, compiling the wrapper library with the default compiler
flags may look something like:

```shell
VERNIER_ROOT=<path_to_vernier> make
```

The compilation process will create the wrapper library ``libvernier_psy.a``.

### Linking the wrapper library

In order to use the wrapper with your application, the location of this
library must be provided as an ``include`` path (so that the module file
is found), and linked first with the wrapper library, ``vernier_psy``,
and then with the Vernier library:

```shell
$(F90) -c  ... -I <PATH-TO-PSYCLONE>/lib/profiling/vernier somefile.f90
$(F90) -o a.out ... -L <PATH-TO-PSYCLONE>/lib/profiling/vernier -lvernier_psy \
       -L <PATH-TO-VERNIER> -lvernier_f -lvernier_c -lvernier
```

**Note:**

- The ``<PATH-TO-PSYCLONE>`` differs depending on whether the wrapper
  library is compiled in a clone of PSyclone repository or in a PSyclone
  [installation](./../../README.md#installation).

## Output

An example output of the profiling report is below. Note that Vernier
writes the output into MPI-rank-specific output files, the output is not
added to stdout:

```
Profiling on 8 thread(s).

    #  % Time         Cumul         Self        Total     # of calls        Self   Total    Routine@
                                                                             (Size; Size/sec; Size/call; MinSize; MaxSize)
        (self)        (sec)        (sec)        (sec)                    ms/call   ms/call

    1  100.000        1.496        1.496        1.496              1    1496.066    1496.066    skeleton_constants_mod_psy:invoke_create_de_rham_matrices-compute_derham_matrices_code-r0@0
    2    0.425        1.502        0.006        0.006              5       1.271       1.271    lfric_xios_setup_mod_psy:invoke_1_nodal_coordinates_kernel_type-nodal_coordinates_code-r1@0
    3    0.368        1.508        0.006        0.006             10       0.550       0.550    skeleton_alg_mod_psy:invoke_compute_divergence-matrix_vector_code-r2@0
    4    0.318        1.513        0.005        0.005              1       4.754       4.754    lfric_xios_setup_mod_psy:invoke_0_nodal_xyz_coordinates_kernel_type-nodal_xyz_coordinates_code-r0@0
    5    0.195        1.516        0.003        0.003              3       0.971       0.971    lfric_xios_setup_mod_psy:invoke_2_nodal_coordinates_kernel_type-nodal_coordinates_code-r2@0

```

<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2024-2025, Science and Technology Facilities Council.
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

-------------------------------------------------------------------------------
Authors: J. Henrichs, Bureau of Meteorology,
-->
