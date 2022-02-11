.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2021, Science and Technology Facilities Council.
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
.. Written I. Kavcic, Met Office

.. _libraries:

Libraries
=========

PSyclone provides :ref:`PSyData-API-based <psy_data>` wrappers to
various external libraries. These wrapper libraries provide PSyclone
transformations that insert callbacks to an external library at runtime.
The callbacks then allow third-party libraries to access data structures
at specified locations in the code for different purposes, such as
profiling and extraction of argument values.

These wrapper libraries can be found under the ``lib`` directory in the Git
repository. If you have installed PSyclone using ``pip`` then the libraries
may be found in ``share/psyclone/lib`` under your Python (or PSyclone,
depending on the ``pip install`` options) installation (see
:ref:`here <getting-going-env-loc>` for possible locations).

.. note::  If working with wrapper libraries from a PSyclone installation,
           it is advisable to copy the entire ``lib`` directory to some
           convenient location before building and using them. The provided
           ``Makefile``\s support the options to specify paths to the
           libraries and their dependencies, see :ref:`compilation
           <libraries-compilation>` for more information.

Available libraries
-------------------

An overview of the currently available functionality is below. For details
of what each library does and how to build and use it please see the related
sections in the User Guide and the specific ``README.md`` files in the
associated directories.

Profiling
^^^^^^^^^

PSyclone provides wrapper libraries for some common performance profiling
tools, such as dl_timer and Dr Hook. More information can be found in
the :ref:`Profiling <profiling>` section.

Profiling libraries are located in the ``lib/profiling`` `directory
<https://github.com/stfc/PSyclone/tree/master/lib/profiling>`__.
For detailed instructions on how to build and use them please refer
to their specific ``README.md`` documentation.

Kernel Data Extraction
^^^^^^^^^^^^^^^^^^^^^^

These libraries enable PSyclone to add callbacks that provide access
to all input variables before, and output variables after a kernel
invocation. More information can be found in the
:ref:`PSy Kernel Extractor (PSyKE) <psyke>` section.

Example libraries that extract input and output data into a NetCDF file
for :ref:`LFRic (Dynamo0.3) <dynamo0.3-api>` and
:ref:`GOcean1.0 <gocean1.0-api>` APIs are included with PSyclone in the
``lib/extract/netcdf`` `directory
<https://github.com/stfc/PSyclone/tree/master/lib/extract/netcdf>`__.
For detailed instructions on how to build and use these libraries
please refer to their specific ``README.md`` documentation.

Access Verification
^^^^^^^^^^^^^^^^^^^

Read-only libraries check that a field declared as read-only is not
modified during a kernel call. More information can be found in the
:ref:`Read-Only Verification <psydata_read_verification>` section.

The libraries for :ref:`LFRic (Dynamo0.3) <dynamo0.3-api>` and
:ref:`GOcean1.0 <gocean1.0-api>` APIs are included with PSyclone in
the ``lib/read_only`` `directory
<https://github.com/stfc/PSyclone/tree/master/lib/read_only>`__.
For detailed instructions on how to build and use these libraries
please refer to their specific ``README.md`` documentation.

NAN Test
^^^^^^^^

These libraries test all input and output parameters of a kernel to
make sure they are not ``NaN`` or infinite.  More information can be
found in the :ref:`NAN Test <psydata_nan_test>` section.

The libraries for :ref:`LFRic (Dynamo0.3) <dynamo0.3-api>` and
:ref:`GOcean1.0 <gocean1.0-api>` APIs are included with PSyclone in
the ``lib/nan_test`` `directory
<https://github.com/stfc/PSyclone/tree/master/lib/nan_test>`__.
For detailed instructions on how to build and use these libraries
please refer to their specific ``README.md`` documentation.

.. _libraries-dependencies:

Dependencies
------------

Building and using the wrapper libraries requires that PSyclone be installed
on the host system, see section :ref:`Getting Going <getting-going>`. A
Fortran compiler (e.g. Gnu Fortran compiler, ``gfortran``, is free and easily
installed) and Gnu Make are also required.

The majority of wrapper libraries use `Jinja
<https://pypi.org/project/Jinja/>`_ templates to create PSyData-derived
classes (please refer to :ref:`dev_guide:psy_data` and :ref:`dev_guide:jinja`
for full details about the PSyData API).

Compilation of ``extract``, ``nan_test``, ``read_only`` and some of the
profiling wrapper libraries depends on infrastructure libraries relevant
to the API they are used for. :ref:`LFRic API <dynamo0.3-api>` uses the
LFRic infrastructure and :ref:`GOcean1.0 <gocean1.0-api>` uses the
dl_esm_inf library. The LFRic infrastructure can be obtained from the
LFRic `code repository <https://code.metoffice.gov.uk/trac/lfric/browser>`_,
however this requires access to the `Met Office Science Repository Service
(MOSRS) <https://code.metoffice.gov.uk/trac/home>`_. A useful contact for
LFRic-related questions (including access to MOSRS) is the `"lfric" mailing
list <mailto:lfric@cmpd1.metoffice.gov.uk>`_ which gathers the Met Office and
external LFRic developers and users. The dl_esm_inf library is freely
available and can be downloaded from `<https://github.com/stfc/dl_esm_inf>`_.

Some libraries require NetCDF for compilation. Installation of NetCDF is
described in details in the `hands-on practicals documentation
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals#user-content-netcdf-library-lfric-examples>`_.

Profiling wrapper libraries that depend on external tools (e.g. dl_timer)
require these tools be installed and configured beforehand.

.. _libraries-compilation:

Compilation
-----------

Each library is compiled with ``make`` using the provided ``Makefile`` that
has configurable options for compiler flags and locations of dependencies.

As in case of :ref:`examples <examples-compilation>`, ``F90`` and
``F90FLAGS`` specify the compiler and compilation flags to use. The default
value for ``F90`` is ``gfortran``.

Locations of the top-level ``lib`` directory and the required Jinja templates
are specified with the ``PSYDATA_LIB_DIR`` and ``LIB_TMPLT_DIR`` variables.
For testing purposes their default values are set to relative paths to the
respective directories in the PSyclone repository.

The locations of the infrastructure libraries for LFRic and GOcean1.0
applications can be configured with the variables ``LFRIC_INF_DIR`` and
``GOCEAN_INF_DIR``, respectively. Their default values are set to relative
paths to the locations of these libraries in the PSyclone repository. The
dl_esm_inf library is provided as a Git submodule of the PSyclone
project (see :ref:`dev_guide:dev-installation` in the Developers' Guide
for details on working with submodules) and a pared-down version of LFRic
infrastructure is also available in the PSyclone repository (please refer
to the ``README.md`` documentation of relevant wrapper libraries). However,
the infrastructure libraries are not available in a PSyclone installation
and they need to be downloaded separately, see :ref:`Dependencies
<libraries-dependencies>` for more information. In this case
``LFRIC_INF_DIR`` and ``GOCEAN_INF_DIR`` **must be set** to the exact paths
to where the respective infrastructure source can be found. For instance,

.. code-block:: shell

    GOCEAN_INF_DIR=$HOME/dl_esm_inf/finite_difference make

Profiling wrapper libraries that depend on external tools have specific
variables that configure paths to where these libraries are located in a
user environment.

For more information on how to build and configure a specific library
please refer to its ``README.md`` documentation.

Similar to compilation of the :ref:`examples <examples-compilation>`, the
compiled library can be removed by running ``make clean``. There is also
the ``allclean`` target that removes the compiled wrapper library as well
as the compiled infrastructure library that the wrapper may depend on.

The compilation of wrapper libraries was tested with the Gnu and Intel
Fortran compilers, see :ref:`here <supported-compilers>` for the full list.
Please let the PSyclone developers know if you have problems using a
compiler that has been tested or if you are working with a different compiler.
