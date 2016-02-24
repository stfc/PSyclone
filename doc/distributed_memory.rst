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

PSyclone adds **HaloExchange** objects to the generated PSy
**Schedule** (see the :ref:`psy-layer-schedule` Section) at the
required locations. The halo-exchange objects are exposed here for the
purposes of optimisation. For example the halo-exchange objects may be
moved in the schedule (via appropriate transformations) to enable
overlap of computation with communication.

.. note:: When these optimisations are implemented, add a reference to
   the :ref:`transformations` Section.

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
the ``DISTRIBUTED_MEMORY`` variable in the ``config.py`` file to
``False``. Alternatively, the distributed memory option can be changed
interactively from the ``PSyFactory`` routine by setting the optional
``distributed_memory`` flag; for example: ::

    psy = PSyFactory(api=api, distributed_memory=False)

At this time there is no way to switch distributed memory support off
from the ``generator`` script.

Status
------

Distributed memory support is currently limited to the ``dynamo0.3``
API.  The remaining API's ignore the distributed memory flag and
continue to produce code without any distributed memory functionality,
irrespective of its value.
