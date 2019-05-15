.. _distributed_memory:

Distributed Memory
==================

PSyclone supports the generation of code for distributed memory
machines. When this option is switched on, PSyclone takes on
responsibility for both performance and correctness, as described
below.

Correctness
-----------

PSyclone is responsible for adding appropriate distributed memory
communication calls to the PSy layer to ensure that the distributed
memory code runs correctly. For example, a stencil operation will
require halo exchanges between the different processes.

The burden of correctly placing distributed memory communication calls
has traditionally been born by the user. However, PSyclone is able to
determine the placing of these within the PSy-layer, thereby freeing
the user from this responsibility. Thus, the Algorithm and Kernel code
remain the same, irrespective of whether the target architecture does
or does not require a distributed memory solution.

Performance
-----------

PSyclone adds **HaloExchange** and **GlobalSum** objects to the
generated PSyIR **InvokeSchedule** at the required locations.
The halo-exchange and global-sum objects
are exposed here for the purposes of optimisation. For example the
halo-exchange and/or global-sum objects may be moved in the schedule
(via appropriate transformations) to enable overlap of computation
with communication.

.. note:: When these optimisations are implemented, add a reference to
   the :ref:`transformations` Section.

A halo exchange is required with distributed memory when a processor
requires data from its halo and the halo information is out of
date. One example is where a field is written to and then read using a
stencil access. Halo exchanges have performance implications so should
only be used where necessary.

A global sum is required with distributed memory when a scalar is
written to. Global sums can have performance implications so should
only be used where necessary. Global sums currently only occur in
certain Built-in kernels. The description of Built-ins indicates when
this is the case.


Implementation
--------------

Within the contents of an ``invoke()`` call, PSyclone is able to
statically determine which communication calls are required and where
they should be placed. However, PSyclone has no information on what
happens outside ``invoke()`` calls and thus is unable to statically
determine whether communication is required between these calls. The
solution we use is to add run-time flags in the PSy layer to keep
track of whether data has been written to and read from. These flags
are then used to determine whether communication calls are required upon
entry to an ``invoke()``.

Control
-------

Support for distributed memory can be switched on or off with the
default being on. The default can be changed permanently by modifying
the ``DISTRIBUTED_MEMORY`` variable in the ``psyclone.cfg`` configuration
file to ``false`` (see :ref:`configuration`).

Distributed memory can be switched on or off from the ``psyclone``
script using the ``-dm``/``--dist_mem`` or ``-nodm``/``--no_dist_mem``
flags, respectively.

For interactive access, the distributed memory option can be changed
interactively from the ``PSyFactory`` class by setting the optional
``distributed_memory`` flag; for example: ::

    psy = PSyFactory(api=api, distributed_memory=False)

Similarly the distributed memory option can be changed interactively
from the ``generate`` function by setting the optional
``distributed_memory`` flag, e.g.:
::

    psy, alg = generate("file.f90", distributed_memory=False).

Status
------

Distributed memory support is currently limited to the ``dynamo0.3``
API.  The remaining APIs ignore the distributed memory flag and
continue to produce code without any distributed memory functionality,
irrespective of its value.
