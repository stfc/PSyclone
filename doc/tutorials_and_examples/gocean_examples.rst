GOcean Examples 
===============

Example 1: Loop transformations
-------------------------------

Examples of applying various transformations (loop fusion, OpenMP,
OpenMP Taskloop, OpenACC, OpenCL) to the semi-PSyKAl'd version of
the Shallow benchmark. ("semi" because not all kernels are called
from within invoke()'s.) Also includes an example of generating a
DAG from an InvokeSchedule.

Example 2: OpenACC
------------------

This is a simple but complete example of using PSyclone to enable an
application to run on a GPU by adding OpenACC directives. A ``Makefile``
is included which will use PSyclone to generate the PSy code and
transformed kernels and then compile the application. This compilation
requires that the `dl_esm_inf library <https://github.com/stfc/dl_esm_inf>`_
be installed/available - it is provided as a Git submodule of the PSyclone
project (see :ref:`dev-installation` in the Developers' Guide
for details on working with submodules).

The supplied ``Makefile`` also provides a second, ``profile`` target which
performs the same OpenACC transformations but then encloses the whole
of the resulting PSy layer in a profiling region. By linking this with
the PSyclone NVTX profiling wrapper (and the NVTX library itself), the
resulting application can be profiled using NVIDIA's `nsys` tool.

Example 3: OpenCL
-----------------

Example of the use of PSyclone to generate an OpenCL driver version of
the PSy layer and OpenCL kernels. The ``Makefile`` in this example provides
a target (`make compile-ocl`) to compile the generated OpenCL code. This
requires an OpenCL implementation installed in the system. Read the README
provided in the example folder for more details about how to compile and
execute the generated OpenCL code.

Example 4: Kernels containing use statements
--------------------------------------------

Transforming kernels for use with either OpenACC or OpenCL requires
that we handle those that access data and/or routines via module
``use`` statements. This example shows the various forms for which
support is being implemented. Although there is support for converting
global-data accesses into kernel arguments, PSyclone does not yet support
nested ``use`` of modules (i.e. data accessed via a module that in turn
imports that symbol from another module) and kernels that call other
kernels (Issue #342).

.. _gocean_example_psydata:

Example 5: PSyData
------------------
This directory contains all examples that use the
:ref:`PSyData API<psy_Data>`. At this stage there are three
runnable examples:

Example 5.1: Kernel data extraction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example shows the use of kernel data extraction in PSyclone.
It instruments each of the two invokes in the example program
with the PSyData-based kernel extraction code. Detailed compilation
instructions are in the ``README.md`` file, including how to switch
from using the stand-alone extraction library to the NetCDF-based one
(see :ref:`extraction_libraries` for details).

The ``Makefile`` in this example will create the binary that extracts
the data at run time, as well as two driver programs that can read in
the extracted data, call the kernel, and compare the results. These
driver programs are independent of the dl_esm_inf infrastructure library.
These drivers can only read the corresponding file format, i.e. a NetCDF
driver program cannot read in extraction data that is based on Fortran IO
and vice versa.

Example 5.2: Profiling
~~~~~~~~~~~~~~~~~~~~~~
This example shows how to use the profiling support in PSyclone.
It instruments two invoke statements and can link in with any
of the following profiling wrapper libraries: template,
simple_timer, dl_timer, TAU, Vernier, and DrHook (see
:ref:`profiling_third_party_tools`). The ``README.md``
file contains detailed instructions on how to build the
different executables. By default (i.e. just using ``make``
without additional parameters) it links in with the
template profiling library included in PSyclone. This library just
prints out the name of the module and region before and after each
invoke is executed. This example can actually be executed to
test the behaviour of the various profiling wrappers, and is
also useful if you want to develop your own wrapper libraries.

.. _gocean_example_readonly:

Example 5.3: Read-only-verification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example shows the use of read-only-verification with PSyclone.
It instruments each of the two invokes in the example program
with the PSyData-based read-only-verification code.
It uses the dl_esm_inf-specific read-only-verification library
(``lib/read_only/dl_esm_inf/``).

.. note:: The ``update_field_mod`` subroutine contains some very
    buggy and non-standard code to change the value of some
    read-only variables and fields, even though the variables
    are all declared with
    ``intent(in)``. It uses the addresses of variables and
    then out-of-bound writes to a writeable array to
    actually overwrite the read-only variables. Using
    array bounds checking at runtime will be triggered by these
    out-of-bound writes.

The ``Makefile`` in this example will link with the compiled
read-only-verification library. You can execute the created
binary and it will print two warnings about modified
read-only variables:

.. code-block:: none

    --------------------------------------
    Double precision field b_fld has been modified in main : update
    Original checksum:   4611686018427387904
    New checksum:        4638355772470722560
    --------------------------------------
    --------------------------------------
    Double precision variable z has been modified in main : update
    Original value:    1.0000000000000000     
    New value:         123.00000000000000     
    --------------------------------------

.. _gocean_example_value_range_check:

Example 5.4: Value Range Check
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example shows the use of valid-number verification with PSyclone.
It instruments each of the two invokes in the example program
with the PSyData-based Value-Range-Check code.
It uses the dl_esm_inf-specific value range check library
(``lib/value_range_check/dl_esm_inf/``).

.. note:: The ``update_field_mod`` subroutine contains code
    that will trigger a division by 0 to create NaNs. If
    the compiler happens to add code that handles floating point
    exceptions , this will take effect before the value testing
    is done by the PSyData-based verification code.

The ``Makefile`` in this example will link with the compiled
value_range_check library. You can then execute the binary
and enable the value range check by setting environments
(see :ref:`value range check<psydata_value_range_check>` for
details).

.. code-block:: shell

    PSYVERIFY__main__init__b_fld=2:3 ./value_range_check
    ...
    PSyData: Variable b_fld has the value 0.0000000000000000 at index/indices 6 1 in module 'main', region 'init', which is not between '2.0000000000000000' and '3.0000000000000000'.
    ...
    PSyData: Variable a_fld has the invalid value 'Inf' at index/indices 1 1 in module 'main', region 'update'.

As indicated in :ref:`value range check<psydata_value_range_check>`, you can
also check a variable in all kernels of a module, or in any instrumented
code region (since the example has only one module, both settings below
will create the same warnings):

.. code-block:: shell

    PSYVERIFY__main__b_fld=2:3 ./value_range_check
    PSYVERIFY__b_fld=2:3 ./value_range_check
    ...
    PSyData: Variable b_fld has the value 0.0000000000000000 at index/indices 6 1 in module 'main', region 'init', which is not between '2.0000000000000000' and '3.0000000000000000'.
    ...
    PSyData: Variable b_fld has the value 0.0000000000000000 at index/indices 6 1 in module 'main', region 'update', which is not between '2.0000000000000000' and '3.0000000000000000'.

Notice that now a warning is created for both kernels: ``init`` and ``update``.

Support for checking arbitrary Fortran code is tracked as issue #2741.


Example 6: PSy-layer Code Creation using PSyIR
----------------------------------------------
This example informs the development of the code generation of PSy-layer
code using the PSyIR language backends.
