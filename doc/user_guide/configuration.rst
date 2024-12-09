.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2024, Science and Technology Facilities Council
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
.. Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
.. Modified by: J. Henrichs, Bureau of Meteorology,
..              I. Kavcic, Met Office

.. _configuration:

Configuration
=============

PSyclone reads various run-time configuration options from
the ``psyclone.cfg`` file. As described in
:ref:`getting-going-configuration`, the default ``psyclone.cfg``
configuration file is installed in ``<python-base-prefix>/share/psyclone/``
during the installation process. The original version of this file
is in the ``PSyclone/config`` directory of the PSyclone
distribution.

At execution-time, the user can specify a custom configuration file to
be used. This can either be done with the ``--config`` command-line
option, or by specifying the (full path to the) configuration file
to use via the ``PSYCLONE_CONFIG`` environment variable. If the specified
configuration file is not found then PSyclone will fall back to
searching in a list of default locations.

The ordering of these
locations depends upon whether PSyclone is being run within a Python
virtual environment (such as ``venv``). If no virtual environment is
detected then the locations searched, in order, are:

1. ``${PWD}/.psyclone/``
2. ``${HOME}/.local/share/psyclone/``
3. ``<python-base-dir>/share/psyclone/``

where ``<python-base-dir>`` is the path stored in Python's ``sys.prefix``.

If a virtual environment is detected then it is assumed that the
``share`` directory will be a part of that environment. In order to
maintain isolation of distinct virtual environments this directory is
then checked *before* the user's home directory. i.e. the list of
locations searched is now:

1. ``${PWD}/.psyclone/``
2. ``<python-base-dir>/share/psyclone/``
3. ``${HOME}/.local/share/psyclone/``

Note that for developers a slightly different configuration handling
is implemented, see :ref:`dev_guide:dev_configuration` for details.

Options
-------

The configuration file is read by the Python ConfigParser class
(https://docs.python.org/3/library/configparser.html) and must be
formatted accordingly. It currently consists of a ``DEFAULT``
section e.g.:
::

    [DEFAULT]
    DISTRIBUTED_MEMORY = true
    REPRODUCIBLE_REDUCTIONS = false
    REPROD_PAD_SIZE = 8
    PSYIR_ROOT_NAME = psyir_tmp
    VALID_PSY_DATA_PREFIXES = profile, extract

and an optional API specific section, for example for the
``lfric`` section:
::

   [lfric]
   access_mapping = gh_read: read, gh_write: write, gh_readwrite: readwrite,
                    gh_inc: inc, gh_readinc: readinc, gh_sum: sum
   COMPUTE_ANNEXED_DOFS = false
   supported_fortran_datatypes = real, integer, logical
   default_kind = real: r_def, integer: i_def, logical: l_def
   precision_map = i_def: 4,
                   l_def: 1,
                   r_def: 8,
                   r_double: 8,
                   r_ncdf: 8,
                   r_quad: 16,
                   r_second: 8,
                   r_single: 4,
                   r_solver: 4,
                   r_tran: 8,
                   r_bl: 8,
                   r_phys: 8,
                   r_um: 8
   RUN_TIME_CHECKS = false
   NUM_ANY_SPACE = 10
   NUM_ANY_DISCONTINUOUS_SPACE = 10

or for ``gocean``:
::

   [gocean]
   access_mapping = go_read:read, go_write:write, go_readwrite:readwrite
   grid-properties = go_grid_xstop: {0}%%grid%%subdomain%%internal%%xstop: scalar,
                  go_grid_ystop: {0}%%grid%%subdomain%%internal%%ystop: scalar,
                  go_grid_data: {0}%%data: array,
                  ...

The meaning of the various entries is described in the following sub-sections.

Note that ConfigParser supports various forms of boolean entry
including "true/false", "yes/no" and "1/0". See
https://docs.python.org/3/library/configparser.html#supported-datatypes
for more details.

.. _config-default-section:

``DEFAULT`` Section
^^^^^^^^^^^^^^^^^^^

This section contains entries that are, in principle, applicable to all APIs
supported by PSyclone.

.. tabularcolumns:: |l|L|l|

======================= ======================================================= ===========
Entry                   Description                                             Type
======================= ======================================================= ===========
DISTRIBUTED_MEMORY      Whether or not to generate code for distributed-memory  bool
                        parallelism by default.  Note that this is currently
                        only supported for the LFRic (Dynamo 0.3) API.
REPRODUCIBLE_REDUCTIONS Whether or not to generate code for reproducible OpenMP bool
                        reductions (see :ref:`openmp-reductions`) by default.
REPROD_PAD_SIZE         If generating code for reproducible OpenMP reductions,  int
                        this setting controls the amount of padding used
                        between elements of the array in which each thread
                        accumulates its local reduction. (This prevents false
                        sharing of cache lines by different threads.)
PSYIR_ROOT_NAME         The root for generated PSyIR symbol names if one is not str
                        supplied when creating a symbol. Defaults to
                        "psyir_tmp".
VALID_PSY_DATA_PREFIXES Which class prefixes are permitted in any               list of str
                        PSyData-related transformations. See :ref:`psy_data`
                        for details.
BACKEND_CHECKS_ENABLED  Optional (defaults to True). Whether or not the PSyIR   bool
                        backend should validate the tree that it is passed.
                        Can be overridden by the ``--backend`` command-line
                        flag (see :ref:`backend-options`).
======================= ======================================================= ===========

Common Sections
^^^^^^^^^^^^^^^

The following entries must be defined for each API in order for PSyclone to
work as expected:

.. tabularcolumns:: |l|L|

======================= =======================================================
Entry                   Description
======================= =======================================================
access_mapping          This field defines the strings that are used by a
                        particular API to indicate write, read, ... access. Its
                        value is a comma separated list of access-string:access
                        pairs, e.g.:

                        ``gh_read: read, gh_write: write, gh_readwrite: readwrite,
                        gh_inc: inc, gh_readinc: gh_sum: sum``

                        At this stage these 6 types are defined for
                        read, write, read+write, increment,
                        read+increment and summation access by
                        PSyclone. Sum is a form of reduction. The
                        GOcean API does not support increment or sum,
                        so it only defines three mappings for read,
                        write, and readwrite.
======================= =======================================================


``lfric`` Section
^^^^^^^^^^^^^^^^^^^^^

This section contains configuration options that are only applicable when
using the LFRic (Dynamo 0.3) API.

.. tabularcolumns:: |l|L|

=========================== ===================================================
Entry                       Description
=========================== ===================================================
COMPUTE_ANNEXED_DOFS        Whether or not to perform redundant computation
                            over annexed dofs in order to reduce the number of
                            halo exchanges, see :ref:`lfric-annexed_dofs`.

supported_fortran_datatypes Captures the supported Fortran data types of LFRic
                            arguments, see :ref:`lfric-datatype-kind`.

default_kind                Captures the default kinds (precisions) for the
                            supported Fortran data types in LFRic, see
                            :ref:`lfric-datatype-kind`.

precision_map               Captures the value of the actual precisions in
                            bytes, see :ref:`lfric-precision-map`
                            
RUN_TIME_CHECKS             Specifies whether to generate run-time validation
                            checks, see :ref:`lfric-run-time-checks`.

NUM_ANY_SPACE               Sets the number of ``ANY_SPACE`` function spaces
                            in LFRic, see :ref:`lfric-num-any-spaces`.

NUM_ANY_DISCONTINUOUS_SPACE Sets the number of ``ANY_DISCONTINUOUS_SPACE``
                            function spaces in LFRic, see
                            :ref:`lfric-num-any-spaces`.
=========================== ===================================================

``gocean`` Section
^^^^^^^^^^^^^^^^^^^^^
This section contains configuration options that are only applicable when
using the Gocean 1.0 API.

.. tabularcolumns:: |l|L|

======================= =======================================================
Entry                   Description
======================= =======================================================
iteration-spaces        This contains definitions of additional iteration spaces
                        used by PSyclone. A detailed description can be found
                        in the :ref:`gocean-configuration-iteration-spaces`
                        section of the GOcean1.0 chapter.

grid-properties         This key contains definitions to access various grid
                        properties. A detailed description can be found
                        in the :ref:`gocean-configuration-grid-properties`
                        section of the GOcean1.0 chapter.
======================= =======================================================
