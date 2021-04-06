.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2021, Science and Technology Facilities Council
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
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab
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
be used. This can either be done with the ``--config`` command line
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
    DEFAULTAPI = dynamo0.3
    DEFAULTSTUBAPI = dynamo0.3
    DISTRIBUTED_MEMORY = true
    REPRODUCIBLE_REDUCTIONS = false
    REPROD_PAD_SIZE = 8
    PSYIR_ROOT_NAME = psyir_tmp
    VALID_PSY_DATA_PREFIXES = profile, extract

and an optional API specific section, for example for the
``dynamo0.3`` section:
::

   [dynamo0.3]
   access_mapping = gh_read: read, gh_write: write, gh_readwrite: readwrite,
                    gh_inc: inc, gh_sum: sum
   COMPUTE_ANNEXED_DOFS = false
   supported_fortran_datatypes = real, integer, logical
   default_kind = real: r_def, integer: i_def, logical: l_def
   RUN_TIME_CHECKS = false
   NUM_ANY_SPACE = 10
   NUM_ANY_DISCONTINUOUS_SPACE = 10

or for ``gocean1.0``:
::

   [gocean1.0]
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

``DEFAULT`` Section
^^^^^^^^^^^^^^^^^^^

This section contains entries that are, in principle, applicable to all APIs
supported by PSyclone.

.. tabularcolumns:: |l|L|

======================= =======================================================
Entry                   Description
======================= =======================================================
DEFAULTAPI              The API that PSyclone assumes an Algorithm/Kernel
                        conforms to if no API is specified. Must be one of the
                        APIs supported by PSyclone ("dynamo0.1", "dynamo0.3",
                        "gocean0.1", "gocean1.0" and "nemo"). If there is no
                        API specified and there is only one API-specific
                        section in the config file loaded, this API will be
                        used. This value can be overwritten by the command
                        line option '-api'. If there is no API entry in the
                        config file, and '-api' is not specified on the 
                        command line, "dynamo0.3" is used as default.
DEFAULTSTUBAPI          The API that the kernel-stub generator assumes by
                        default. Must be one of the stub-APIs supported by
                        PSyclone ("dynamo0.3" only at this stage).
DISTRIBUTED_MEMORY      Whether or not to generate code for distributed-memory
                        parallelism by default.  Note that this is currently
                        only supported for the LFRic (Dynamo 0.3) API.
REPRODUCIBLE_REDUCTIONS Whether or not to generate code for reproducible OpenMP
                        reductions (see :ref:`openmp-reductions`) by default.
REPROD_PAD_SIZE         If generating code for reproducible OpenMP reductions,
                        this setting controls the amount of padding used
                        between elements of the array in which each thread
                        accumulates its local reduction. (This prevents false
                        sharing of cache lines by different threads.)
PSYIR_ROOT_NAME         The root for generated PSyIR symbol names if one is not
                        supplied when creating a symbol. Defaults to
                        "psyir_tmp".
VALID_PSY_DATA_PREFIXES Which class prefixes are permitted in any
                        PSyData-related transformations. See :ref:`psy_data`
                        for details.
======================= =======================================================

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
                        gh_inc: inc, gh_sum: sum``

                        At this stage these 5 types are defined for read, write,
                        read+write, increment and summation access by PSyclone.
                        Sum is a form of reduction.
                        The GOcean APIs do not support increment or sum, so
                        they only define three mappings for read, write, and 
                        readwrite.
======================= =======================================================


``dynamo0.3`` Section
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

RUN_TIME_CHECKS             Specifies whether to generate run-time validation
                            checks, see :ref:`lfric-run-time-checks`.

NUM_ANY_SPACE               Sets the number of ``ANY_SPACE`` function spaces
                            in LFRic, see :ref:`lfric-num-any-spaces`.

NUM_ANY_DISCONTINUOUS_SPACE Sets the number of ``ANY_DISCONTINUOUS_SPACE``
                            function spaces in LFRic, see
                            :ref:`lfric-num-any-spaces`.
=========================== ===================================================

``gocean1.0`` Section
^^^^^^^^^^^^^^^^^^^^^
This section contains configuration options that are only applicable when
using the Gocean 1.0 API.

.. tabularcolumns:: |l|L|

======================= =======================================================
Entry                   Description
======================= =======================================================
iteration-spaces        This contains definitions of additional iteration spaces
                        used by PSyclone. A detailed description can be found
                        in the :ref:`gocean1.0-configuration-iteration-spaces`
                        section of the GOcean1.0 chapter.

grid-properties         This key contains definitions to access various grid
                        properties. A detailed description can be found
                        in the :ref:`gocean1.0-configuration-grid-properties`
                        section of the GOcean1.0 chapter.
======================= =======================================================

``NEMO`` Section
^^^^^^^^^^^^^^^^^^^^^
This section contains configuration options that are only applicable when
using the NEMO API.

.. tabularcolumns:: |l|L|

======================= =======================================================
Entry                   Description
======================= =======================================================
mapping-TYPE            This declares a mapping for a certain loop level,
                        specified as TYPE. Each value must have three key:value
                        pairs. A value can be empty if it is not required or
                        not known, but the key must still be specified. 
                        The required keys are:

                        ``var``: the variable name that indicates
                        the loop level,

                        ``start``: the first loop iteration, and

                        ``stop``: the last loop iteration.

                        Each loop detected by the NEMO API will be given one of
                        the TYPE values specified in the configuration file.
                        See the example below for more details.

index-order             Specifies the order in which loops are created when
                        converting an implicit loop to an explicit loop.
                        All values in this comma-separated list must have a
                        corresponding ``mapping-TYPE`` value defined.
======================= =======================================================

Below we show an example of the NEMO section of a PSyclone configuration file.
Note how the values in ``index-order`` have corresponding mapping entries, e.g.
``mapping-lon``, ``mapping-lat`` etc.::

    mapping-lon = var: ji, start: 1, stop: jpi
    mapping-lat = var: jj, start: 1, stop: jpj
    mapping-levels = var: jk, start: 1, stop: jpk
    mapping-tracers = var: jt, start: 1, stop:
    mapping-unknown = var: , start: 1, stop:

    index-order = lon, lat, levels, tracers

If a NEMO loop then uses ``Do jj=...``, PSyclone will give this loop the type
'lat', because the loop uses the variable name specified in the configuration file
for a loop of type 'lat'.
The loop type can be accessed using ``loop.loop_type``, i.e. in this example
it will be ``loop.loop_type == 'lat'``.

The entry ``mapping-unknown`` has an empty value for the key 'var'. This means
that the type 'unknown'  will be used for any loop that can not be mapped
using any of the other variable names in the configuration file.
