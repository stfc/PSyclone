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
.. Written by J. Henrichs, Bureau of Meteorology
.. Modified by I. Kavcic, Met Office

.. highlight:: python

.. _psy_data:

PSyData API
===========

PSyclone provides transformations that will insert callbacks to
an external library at runtime. These callbacks allow third-party
libraries to access data structures at specified locations in the
code. The PSyclone :ref:`wrappers <libraries>` to external libraries
are provided with the :ref:`PSyclone installation <getting-going-env-loc>`.
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
  kernel, which can be used for performance tuning. An example
  library that extracts input and output data into a NetCDF file
  is included with PSyclone (see :ref:`psyke_netcdf`).

Access Verification:
  The callbacks can be used to make sure a field declared as read-only
  is not modified during a kernel call (either because of an incorrect
  declaration, or because memory is overwritten). The implementation
  included in PSyclone uses a simple 64-bit checksum to detect changes
  to a field (and scalar values). See :ref:`psydata_read_verification`
  for details.

NAN Test:
  The callbacks can be used to make sure that all floating point input
  and output parameters of a kernel are not a ``NaN`` (not-a-number) or
  infinite. See :ref:`psydata_nan_test` for the full description.

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
Refer to :ref:`dev_guide:psy_data` for full details about the PSyData API.

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
can be applied for both the :ref:`LFRic <dynamo0.3-api>` and
:ref:`GOcean API <gocean1.0-api>` - no API-specific
transformations are required. Below is an example that searches for each
loop in an invoke (which will always surround kernel calls) and applies the
transformation to each one. This code has been successfully used as a
global transformation with the LFRic Gravity Wave miniapp (the executable
is named ``gravity_wave``)::

    def trans(psy):
        from psyclone.psyir.transformations import ReadOnlyVerifyTrans
        from psyclone.psyir.nodes import Loop
        read_only_verify = ReadOnlyVerifyTrans()

        for invoke in psy.invokes.invoke_list:
            schedule = invoke.schedule
            for node in schedule:
                if isinstance(node, Loop):
                    read_only_verify.apply(node)

        return psy

Besides the transformation, a library is required to do the actual
verification at runtime. There are two implementations of the
read-only-verification library included in PSyclone: one for LFRic,
and one for GOcean.
Both libraries support the environment variable ``PSYDATA_VERBOSE``.
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
``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``,
but this will certainly need to be changed for any user (for instance with
PSyclone installation). The LFRic infrastructure library is not used in
linking the verification library. The application which uses the
read-only-verification library needs to link in the infrastructure
library anyway.

.. note:
    It is the responsibility of the user to make sure that the infrastructure
    files used during compilation of the read-only-verification library are
    also used when linking the application. Otherwise strange and
    non-reproducible crashes might happen.

Compilation of the library is done by invoking ``make`` and setting
the required variables:

.. code-block:: shell

    make LFRIC_INF_DIR=some_path F90=ifort F90FLAGS="--some-flag"

This will create a library called ``lib_read_only.a``.

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

.. _psydata_nan_test:

NAN Test
--------

This transformation can be used for both LFRic and GOcean APIs. It will
test all input and output parameters of a kernel to make sure they are not
``NaN`` or infinite. If they are, an error message like the following
is printed, but the program is not aborted::

     PSyData: Variable a_fld has the invalid value Inf at index/indices 1 1 in module 'main' region 'update'.

Is uses the function ``IEEE_IS_FINITE`` from the ieee_arithmetic module
for this test. Note that only floating point numbers will be tested.
Integer numbers do not have a bit pattern for 'infinity' or ``NaN``.

The runtime libraries for GOcean and LFRic are based on a jinja-template
contained in the directory ``<PSYCLONEHOME>/lib/nan_test``.
The respective API-specific libraries map the internal field structures
to Fortran basic types and call the functions from the base class to
handle those.

The relevant libraries for the LFRic and GOcean APIs are contained in
the ``lib/nan_test/lfric`` and``lib/nan_test/dl_esm_inf`` subdirectories,
respectively. For more information on how to build and link these libraries,
please refer to the relevant ``README.md`` files.

An executable example for using the LFRic read-only-verification library is
included in ``tutorial/practicals/LFRic/building_code/4_psydata`` directory,
see `this link for more information
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals/LFRic/building_code/4_psydata>`_.
