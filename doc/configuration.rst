.. _configuration:

Configuration
=============

PSyclone reads various run-time configuration options from
the ``psyclone.cfg`` file. As described in
:ref:`getting-going-configuration`, the default ``psyclone.cfg``
configuration file is installed in ``<python-base-prefix>/share/psyclone/``
during the installation process. (The original version of this file
may be found in the ``PSyclone/config`` directory of the PSyclone
distribution.)

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


Options
-------

The configuration file is read by the Python ConfigParser class
(https://docs.python.org/3/library/configparser.html) and must be
formatted accordingly. It currently consists of a ``DEFAULT``
section e.g.:
::

    [DEFAULT]
    API = dynamo0.3
    DEFAULTSTUBAPI = dynamo0.3
    DISTRIBUTED_MEMORY = true
    REPRODUCIBLE_REDUCTIONS = false
    REPROD_PAD_SIZE = 8

and a an optional API specific section, for example for 
``dynamo0.3`` section:
::

   [dynamo0.3]
   COMPUTE_ANNEXED_DOFS = false

or for ``gocean1.0``:
::

   [gocean1.0]
   iteration-spaces=offset_sw:ct:internal_we_halo:1:2:3:4
                    offset_sw:ct:internal_ns_halo:1:{stop}:1:{stop}+1

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
Entry         		Description
======================= =======================================================
API                     The API that PSyclone assumes an Algorithm/Kernl
                        conforms to if no API is specified. Must be one of the
                        APIs supported by PSyclone (gunghoproto, dynamo0.1,
                        dynamo0.3, gocean0.1, gocean1.0). If there is no
                        API specified and there is only one API-specific
                        section in the config file loaded, this API will be
                        used. This value can be overwritten by the command
                        line option '-api'. If there is no API entry in the
                        config file, and '-api' is not specified on the 
                        command line, dynamo0.3 is used as default.
DEFAULTSTUBAPI          The API that the kernel-stub generator assumes by
                        default. Must be one of the stub-APIs supported by
                        PSyclone (dynamo0.3 only at this stage).
DISTRIBUTED_MEMORY      Whether or not to generate code for distributed-memory
                        parallelism by default.  Note that this is currently
                        only supported for the dynamo0.3 API.
REPRODUCIBLE_REDUCTIONS Whether or not to generate code for reproducible OpenMP
                        reductions (see :ref:`openmp-reductions`) by default.
REPROD_PAD_SIZE         If generating code for reproducible OpenMP reductions,
                        this setting controls the amount of padding used
                        between elements of the array in which each thread
                        accumulates its local reduction. (This prevents false
                        sharing of cache lines by different threads.)
======================= =======================================================

``dynamo0.3`` Section
^^^^^^^^^^^^^^^^^^^^^

This section contains configuration options that are only applicable when
using the Dynamo 0.3 API.

.. tabularcolumns:: |l|L|

=======================	=======================================================
Entry             		Description
=======================	=======================================================
COMPUTE_ANNEXED_DOFS    Whether or not to perform redundant computation over
                        annexed dofs in order to reduce the number of halo
                        exchanges. See :ref:`annexed_dofs` in the Developers'
                        guide.
======================= =======================================================

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
                        in the :ref:`gocean1.0-configuration` section of the
                        GOcean1.0 chapter.
======================= =======================================================
