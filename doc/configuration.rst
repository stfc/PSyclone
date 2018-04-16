.. _configuration:

Configuration
=============

At execution time, PSyclone reads various configuration options from the
``psyclone.cfg`` file. It searches for this file in the following locations,
in order:

1. ``${PWD}/.psyclone/``
2. ``${HOME}/.psyclone/``
3. ``/etc/``

i.e. PSyclone may be configured on a per-project, per-user or system-wide
basis.

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
