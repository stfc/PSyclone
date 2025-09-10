.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
.. Written by J. Henrichs, Bureau of Meteorology
.. Modified by I. Kavcic, Met Office

.. highlight:: python

.. _psy_data:

The PSyData Transformations
===========================

PSyclone provides transformations that will insert callbacks to
an external library at runtime. These callbacks allow third-party
libraries to access data structures at specified locations in the
code. The PSyclone :ref:`wrappers <libraries>` to external libraries
are provided in ``share/psyclone/lib`` in PSyclone :ref:`getting-going-install-loc`.
Some example use cases are:

Profiling:
  By inserting callbacks before and after a region of code,
  performance measurements can be added. PSyclone provides
  wrapper libraries for some common performance profiling tools,
  see :ref:`profiling` for details.

Kernel Data Extraction:
  PSyclone provides the ability to add callbacks that provide access
  to all input variables before, and output variables after a kernel
  invocation. This can be used to automatically create tests for
  a kernel, or to write a stand-alone driver that just calls one
  kernel, which can be used for performance tuning. Two example
  libraries that extract input and output data into either a Fortran
  binary or a NetCDF file are included with PSyclone (see
  :ref:`extraction_libraries`).

Access Verification:
  The callbacks can be used to make sure a field declared as read-only
  is not modified during a kernel call (either because of an incorrect
  declaration, or because memory is overwritten). The implementation
  included in PSyclone uses a simple 64-bit checksum to detect changes
  to a field (and scalar values). See :ref:`psydata_read_verification`
  for details.

Value Range Check:
  The callbacks can be used to make sure that all floating point input
  and output parameters of a kernel are within a user-specified range.
  Additionally, it will also verify that the values are not a ``NaN``
  (not-a-number) or infinite. See :ref:`psydata_value_range_check` for
  the full description.

In-situ Visualisation:
  By giving access to output fields of a kernel, an in-situ visualisation
  library can be used to plot fields while a (PSyclone-processed)
  application is running. There is no example library available at
  this stage, but the API has been designed with this application in mind.


The PSyData API should be general enough to allow these and other
applications to be developed and used.

PSyclone provides transformations that will insert callbacks to
the PSyData API, for example ``ProfileTrans``, ``GOceanExtractTrans``
and ``LFRicExtractTrans``. A user can develop additional transformations
and corresponding runtime libraries for additional functionality.
Refer to :ref:`psy_data` for full details about the PSyData API.

.. _psydata_read_verification:

Read-Only Verification
----------------------

The PSyData interface is being used to verify that read-only variables
in a kernel are not overwritten. The ``ReadOnlyVerifyTrans`` (in
``psyir.transformations.read_only_verify_trans``, or the
:ref_guide:`Transformation Reference Guide psyclone.psyir.transformations.html#classes`) uses the dependency
analysis to determine all read-only variables (i.e. arguments declared
to be read-only in metadata, most implicit arguments in LFRic, grid
properties in GOcean). A simple 64-bit checksum is then computed for all
these arguments before a kernel call, and compared with the checksum
after the kernel call. Any change in the checksum causes a message to
be printed at runtime, e.g.::

    --------------------------------------
    Double precision field b_fld has been modified in main : update
    Original checksum:   4611686018427387904
    New checksum:        4638355772470722560
    --------------------------------------

The transformation that adds read-only-verification to an application
can be applied for both the :ref:`LFRic <lfric-api>` and
:ref:`GOcean API <gocean-api>` - no API-specific transformations are required.
It can also be used to verify existing Fortran code.
Below is an example that searches for each loop in a PSyKAl invoke code (which
will always surround kernel calls) and applies the transformation to each one.
This code has been successfully used as a global transformation with the LFRic
Gravity Wave application (the executable is named ``gravity_wave``)

.. code-block:: fortran

    def trans(psyir):
        from psyclone.psyir.transformations import ReadOnlyVerifyTrans
        from psyclone.psyir.nodes import Loop
        read_only_verify = ReadOnlyVerifyTrans()

        for loop in psyir.walk(Loop):
            read_only_verify.apply(loop)

Besides the transformation, a library is required to do the actual
verification at runtime. There are three implementations of the
read-only-verification library included in PSyclone: one for LFRic,
one for GOcean, and one for generic Fortran code.
These libraries support the environment variable ``PSYDATA_VERBOSE``.
This can be used to control how much output is generated
by the read-only-verification library at runtime. If the
variable is not specified or has the value '0', warnings will only
be printed if checksums change. If it is set to '1', a message will be
printed before and after each kernel call that is checked. If the
variable is set to '2', it will additionally print the name of each
variable that is checked.


Read-Only Verification Library for LFRic
++++++++++++++++++++++++++++++++++++++++

This library is contained in ``lib/read_only/lfric`` and it must be compiled
before compiling any LFRic-based application that uses read-only verification.
Compiling this library requires access to the LFRic infrastructure library
(since it must implement a generic interface for e.g. the LFRic
:ref:`field <lfric-field>` class).

The ``Makefile`` uses the variable ``LFRIC_INF_DIR`` to point to the
location where LFRic's ``field_mod`` and ``integer_field_mod`` have been
compiled. It defaults to the path to location of the pared-down LFRic
infrastructure located in a clone of PSyclone repository,
``<PSYCLONEHOME>/external/lfric_infrastructure``,
but this will certainly need to be changed for any user (for instance with
PSyclone installation). The LFRic infrastructure library is not used in
linking the verification library. The application which uses the
read-only-verification library needs to link in the infrastructure
library anyway.

.. note::
    It is the responsibility of the user to make sure that the infrastructure
    files used during compilation of the read-only-verification library are
    also used when linking the application. Otherwise strange and
    non-reproducible crashes might happen.

Compilation of the library is done by invoking ``make`` and setting
the required variables:

.. code-block:: shell

    make LFRIC_INF_DIR=some_path F90=ifort F90FLAGS="--some-flag"

This will create a library called ``lib_read_only.a``.

An executable example for using the LFRic read-only-verification library is
included in ``tutorial/practicals/LFRic/building_code/4_psydata`` directory,
see `this link for more information
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals/LFRic/building_code/4_psydata>`_.



Read-Only-Verification Library for GOcean
+++++++++++++++++++++++++++++++++++++++++

This library is contained in the ``lib/read_only/dl_esm_inf`` directory and
it must be compiled before linking any GOcean-based application that uses
read-only verification. Compiling this library requires access to the
GOcean infrastructure library (since it must implement a generic interface
for e.g. the dl_esm_inf ``r2d_field`` class).

The ``Makefile`` uses the variable ``GOCEAN_INF_DIR`` to point to the
location where dl_esm_inf's ``field_mod`` has been compiled. It
defaults to the relative path to location of the dl_esm_inf version
included in PSyclone repository as a Git submodule,
``<PSYCLONEHOME>/external/dl_esm_inf/finite_difference/src``. It can be
changed to a user-specified location if required (for instance with the
PSyclone installation).

The dl_esm_inf library is not used in linking the verification library.
The application which uses the read-only-verification library needs to
link in the infrastructure library anyway.

.. note:
    It is the responsibility of the user to make sure that the infrastructure
    files used during compilation of the Read-Only-Verification library are
    also used when linking the application. Otherwise strange and
    non-reproducible crashes might happen.

Compilation of the library is done by invoking ``make`` and setting
the required variables:

.. code-block:: shell

    make GOCEAN_INF_DIR=some_path F90=ifort F90FLAGS="--some-flag"

This will create a library called ``lib_read_only.a``.
An executable example for using the GOcean read-only-verification
library is included in ``examples/gocean/eg5/readonly``, see
:ref:`gocean_example_readonly`.


Read-Only-Verification Library for Generic Fortran
++++++++++++++++++++++++++++++++++++++++++++++++++

This library is contained in the ``lib/read_only/generic`` directory and
it must be compiled before linking any existing Fortran code that uses
read-only verification.

Compilation of the library is done by invoking ``make`` and setting
the required variables:

.. code-block:: shell

    make F90=ifort F90FLAGS="--some-flag"

This will create a library called ``lib_read_only.a``.
An executable example for using the generic read-only-verification
library is included in ``examples/nemo/eg6/``.

.. _psydata_value_range_check:

Value Range Check
-----------------

This transformation can be used for both LFRic and GOcean APIs. It will
test all input and output parameters of a kernel to make sure they are
within a user-specified range. Additionally, it will also verify that floating
point values are not ``NaN`` or infinite.

At runtime, environment variables must be specified to indicate which variables
are within what expected range, and optionally also at which location.
The range is specified as a ``:`` separated tuple::

    1.1:3.3   A value between 1.1 and 3.3 (inclusive).
    :3.3      A value less than or equal to 3.3
    1.1:      A value greater than or equal to 1.1

The syntax for the environment variable is one of:

``PSYVERIFY__module__kernel__variable``
    The specified variable is tested when calling the specified kernel in the
    specified module.

``PSYVERIFY__module__variable``
    The specified variable name is tested in all kernel calls of the
    specified module that are instrumented with the ValueRangeCheckTrans
    transformation.

``PSYVERIFY__variable``
    The specified variable name is tested in any instrumented code region.

If the module name or kernel name contains a `-` (which can be inserted
by PSyclone, e.g. `invoke_compute-r1`), it needs to be replaced with an
underscore character in the environment variable (`_`)

An example taken from the LFric tutorial (note that values greater than
4000 are actually valid, the upper limit was just chosen to show
a few warnings raised by the value range checker)::

    PSYVERIFY__time_evolution__invoke_initialise_perturbation__perturbation_data=0.0:4000
    PSYVERIFY__time_evolution__perturbation_data=0.0:4000
    PSYVERIFY__perturbation_data=0.0:4000
    
.. warning:: Note that while the field variable is called `perturbation`, PSyclone will
             append `_data` when the LFRic domain is used, so the name becomes
             `perturbation_data`. You have to use
             this name in LFRic in order to trigger the value range check. To verify
             that the tests are done as expected, set the environment variable
             `PSYDATA_VERBOSE` to 1, which will print which data is taken from the
             environment variables:

             .. code-block:: bash

                 PSyData: checking 'time_evolution' region 'invoke_initialise_perturbation' :   0.0000000000000000       <= perturbation_data <=    4000.0000000000000


If values outside the specified range are found, appropriate warnings are printed,
but the program is not aborted::

    PSyData: Variable 'perturbation_data' has the value 4227.3587826606408 at index/indices 27051 in module 'time_evolution', region 'invoke_initialise_perturbation', which is not between '0.0000000000000000' and '4000.0000000000000'.


The library uses the function ``IEEE_IS_FINITE`` from the ieee_arithmetic module
for additionally verifying that values are not ``NAN`` or ``infinity``
for any floating point variable, even if no ``PSY_VERIFY...`` environment
variable is set for this variable. Integer numbers do not have a bit pattern
for 'infinity' or ``NaN``, so they will only be tested for valid range
if a corresponding environment variable is specified.

The runtime libraries for GOcean and LFRic are based on a jinja-template
contained in the directory ``<PSYCLONEHOME>/lib/value_range_check``.
The respective API-specific libraries map the internal field structures
to Fortran basic types and call the functions from the base class to
handle those.

The relevant libraries for the LFRic and GOcean APIs are contained in
the ``lib/value_range_check/lfric`` and ``lib/value_range_check/dl_esm_inf`` subdirectories,
respectively. For more information on how to build and link these libraries,
please refer to the relevant ``README.md`` files.

.. _integrating_psy_data_lfric:

Integrating PSyData Libraries into the LFRic Build Environment
--------------------------------------------------------------
The easiest way of integrating any PSyData-based library into the LFRic
build environment is:

- In the LFRic source tree create a new directory under ``infrastructure/source``,
  e.g. ``infrastructure/source/psydata``.
- Build the PSyData wrapper stand-alone in ``lib/extract/netcdf/lfric`` (which
  will use NetCDF as output format), ``lib/extract/binary/lfric`` (which
  uses standard Fortran binary output format), or ``lib/extract/ascii/lfric``
  (which uses ASCII as output format), by executing ``make``. The compiled
  files will actually not be used, but this step will create all source
  files (some of which are created by jinja). Do not copy
  the compiled files into your LFRic build tree, since these files might be
  compiled with an outdated version of the infrastructure files and be
  incompatible with files in a current LFRic version.
- Copy all processed source files (``extract_netcdf_base.f90``,
  ``kernel_data_netcdf.f90``, ``psy_data_base.f90``,
  ``read_kernel_data_mod.f90``) into ``infrastructure/source/psydata``
- Start the LFRic build process as normal. The LFRic build environment will
  copy the PSyData source files into the working directory and compile
  them.
- If the PSyData library needs additional include paths (e.g. when using an
  external profiling tool), add the required paths to ``$FFLAGS``.
- If additional libraries are required at link time, add the paths
  and libraries to ``$LDFLAGS``. Alternatively, when a compiler wrapper
  script is provided by a third-party tool (e.g. the profiling tool
  TAU provides a script ``tau_f90.sh``), either set the environment variable
  ``$FC``, or if this is only required at link time, the variable ``$LDMPI``
  to this compiler wrapper.

.. warning::
    Only one PSyData library can be integrated at a time. Otherwise there
    will be potentially several modules with the same name (e.g.
    ``psy_data_base``), resulting in errors at compile time.

.. note::
    With the new build system FAB this process might change.
