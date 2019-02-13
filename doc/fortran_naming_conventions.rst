.. _fortran_naming:

Fortran Naming Conventions
--------------------------

There is a convention in the kernel code for the Dynamo0.3 and
GOcean1.0 APIs that if the name of the operation being performed is
``<name>`` then a kernel file is ``<name>_mod.[fF90]``, the name of
the module inside the kernel file is ``<name>_mod``, the name of the
kernel metadata in the module is ``<name>_type`` and the name of the
kernel subroutine in the module is ``<name>_code``.

PSyclone itself does not rely on this convention apart from in the
stub generator (see the :ref:`stub-generation` Section) where the name
of the metadata to be parsed is determined from the module name.
