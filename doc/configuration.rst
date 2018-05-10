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

At execution-time, PSyclone searches for the configuration file in a
number of locations. The ordering of these
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

If it is desired to use a configuration file in a non-standard
location then the search mechanism may be overriden by specifying the
(full path to the) configuration file to use via the
``PSYCLONE_CONFIG`` environment variable. If the specified
configuration file is not found then PSyclone will fall back to
searching the previously listed locations.

Options
-------

The configuration file is read by the Python ConfigParser class
(https://docs.python.org/3/library/configparser.html) and must be
formatted accordingly. It currently consists of a required, DEFAULT
section e.g.:
::

    [DEFAULT]
    SUPPORTEDAPIS = gunghoproto, dynamo0.1, dynamo0.3, gocean0.1, gocean1.0
    DEFAULTAPI = dynamo0.3
    SUPPORTEDSTUBAPIS = dynamo0.3
    DEFAULTSTUBAPI = dynamo0.3
    DISTRIBUTED_MEMORY = true
    REPRODUCIBLE_REDUCTIONS = false
    REPROD_PAD_SIZE = 8

The meaning of each of these entries is described in the following sub-sections.

SUPPORTEDAPIS
^^^^^^^^^^^^^

A comma-separated list of the names of the various APIs that PSyclone supports.

DEFAULTAPI
^^^^^^^^^^

The API that PSyclone assumes an Algorithm/Kernl conforms to if no API
is specified. Must be one of the SUPPORTEDAPIS.

SUPPORTEDSTUBAPIS
^^^^^^^^^^^^^^^^^

Comma-separated list of the APIs that the kernel-stub generator
(see :ref:`stub-generation`) supports.

DEFAULTSTUBAPI
^^^^^^^^^^^^^^

The API that the kernel-stub generator assumes by default.

DISTRIBUTED_MEMORY
^^^^^^^^^^^^^^^^^^

Whether or not to generate code for distributed-memory parallelism by
default.  Note that this is currently only supported for the dynamo0.3
API.

REPRODUCIBLE_REDUCTIONS
^^^^^^^^^^^^^^^^^^^^^^^

Whether or not to generate code for reproducible OpenMP reductions
(see :ref:`openmp-reductions`) by default.

REPROD_PAD_SIZE
^^^^^^^^^^^^^^^

If generating code for reproducible OpenMP reductions, this setting
controls the amount of padding used between elements of the array
in which each thread accumulates its local reduction. (This prevents
false sharing of cache lines by different threads.)
