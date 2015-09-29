Algorithm layer
===============

In the PSyKAl separation of concerns, the Algorithm layer specifies
the algorithm that the scientist would like to run (in terms of calls
to kernel and infrastructure routines) and logically operates on full
fields. Algorithm code in the algorithm layer is not allowed to
include any parallelisation calls or directives and passes datatypes
specified by the particular API.

API
---

The Algorithm layer is forbidden from calling the Kernel layer
directly. In PSyclone, if the programmer would like to call a Kernel
routine from the algorithm layer they must use the ``invoke`` call
(which is common to all API's). The ``invoke`` call is not necessary
(and indeed will not work) if the PSy layer is written manually.

In an ``invoke`` call, the algorithm layer developer adds ``call invoke()``
to their code and within the content of the ``invoke`` call they add a
reference to the required Kernel and the data to pass to it. For example,
::

    ...
    call invoke(integrate_one_kernel(arg1,arg2))
    ...

The algorithm layer can consist of an arbitrary number of files
containing fortran code, any of which may contain as many ``invoke()``
calls as is required. PSyclone is applied to an individual algorithm
layer file and must therefore be run multiple times if multiple files
containing ``invoke()`` calls exist in the algorithm layer.

The algorithm developer is also able to reference more than one Kernel
within an invoke. In fact this feature is encouraged for performance
reasons. **As a general guideline the developer should aim to use as
few invokes as possible with as many Kernel references within them as
is possible**. The reason for this is that it allows for greater
freedom for optimisation in the PSy layer as PSy layer optimisations
are limited to the contents of individual invoke calls - PSyclone
currently does not attempt to optimise the PSy layer over multiple
invoke calls.

As well as generating the PSy layer code, PSyclone modifies the
Algorithm layer code, replacing ``invoke`` calls with calls to the
generated PSy layer so that the algorithm code is compilable and
linkable to the PSy layer and adding in the appropriate ``use``
statement. For example, the above ``integrate_one_kernel`` invoke is
translated into something like the following:
::

  ...
  use psy, only : invoke_0_integrate_one_kernel
  ...
  call invoke_0_integrate_one_kernel(arg1,arg2)
  ...

You may have noticed from other examples in this guide that an
algorithm specification in an invoke call references the metadata
``type`` in an invoke call, not the ``code`` directly; this is by
design.

For example, in the invoke call below, ``integrate_one_kernel`` is
used.
::

  ...
  call invoke(integrate_one_kernel(arg1,arg2))
  ...

``integrate_one_kernel`` is the name of the metadata type in the module, not
the name of the subroutine in the Kernel ...
::

  module integrate_one_module
    ...
    type, extends(kernel_type) :: integrate_one_kernel
      ...
    end type
    ...
  contains
    ...
    subroutine integrate_one_code(...)
    ...
    end subroutine integrate_one_code
    ...
  end module integrate_one_module
