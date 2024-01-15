# PSyclone Wrapper Library for TAU

This is a wrapper library that maps the [PSyclone profiling API](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling)
to the TAU API.

## Dependencies

TAU must be available at link time but is not actually required when compiling the
PSyData wrapper.

This profiling library uses the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html) to interface with
the application. The library is based on the [PSyData base class](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psydata-base-class),
which is included in PSyclone as a Jinja template, ``psy_data_base.jinja``.
Full documentation on using this template is provided in the PSyclone
[Developer Guide](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja).

The library uses the ``ProfileData`` type to store TAU's handle for each
region.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string. Do not set ``$F90`` to
``tau_f90.sh``, since then the PSydata wrapper would be instrumented as well
and you end up with additional entries in your profiling results.

The compiler used here should be the same that was used to compile TAU. You
can use ``tau_f90.sh -optVerbose`` to see which compiler will be invoked
by TAU's compiler wrapper.


The compilation process will create the wrapper library ``libtau_psy.a``.

### Linking the wrapper library

In order to use the wrapper with your application, at compile time you must
provide the location of the wrapper as an ``include`` path (so that the
module file is found). It is not required to use the ``tau_f90.sh``
compiler wrapper when compiling the application, e.g.:

```shell
$(F90) -c  ... -I <PATH-TO-PSYCLONE>/lib/profiling/tau somefile.f90
```

At link time, you need to link with the wrapper library ``libtau_psy.a``.
If you use the ``tau_f90.sh`` compiler wrapper, nothing else is required,
otherwise you need to add the required libraries from TAU at link time.

```shell
tau_f90.sh -o a.out ... -L <PATH-TO-PSYCLONE>/lib/profiling/tau -ltau_psy
```

**Note:**

- The ``<PATH-TO-PSYCLONE>`` differs depending on whether the wrapper
  library is compiled in a clone of the PSyclone repository or in a PSyclone
  [installation](./../../README.md#installation).


<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
