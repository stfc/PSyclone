Introduction to the examples
============================

Various examples of the use of PSyclone are provided under the
``examples`` directory in the Git repository. If you have installed
PSyclone using ``pip`` then the examples may be found in
``share/psyclone/examples`` in psyclone  :ref:`getting-going-install-loc`.

Running any of these examples requires that PSyclone be installed on
the host system, see Section :ref:`Getting Going <getting-going>`.
This section is intended to provide an overview of the various examples
so that a user can find one that is appropriate to them. For details of
what each example does and how to run each example please see the
``README.md`` files in the associated directories.

.. TODO #2627
    Alternatively, some of the examples have associated Jupyter notebooks
    that may be launched with Binder on `MyBinder <https://mybinder.org/>`_.
    This is most easily done by following the links from the top-level
    `README <https://github.com/stfc/PSyclone#try-it-on-binder>`_.

For the purposes of correctness checking, the whole suite of examples
may be executed using Gnu ``make`` (this functionality is used by GitHub
Actions alongside the test suite). The default target is ``transform`` which
just performs the PSyclone code transformation steps for each
example. For those examples that support it, the ``compile`` target
also requests that the generated code be compiled. The ``notebook``
target checks the various Jupyter notebooks using ``nbconvert``.

.. note:: As outlined in the :ref:`Run <getting-going-run>` section, if
          working with the examples from a PSyclone installation, it is
          advisable to copy the whole ``examples`` directory to some
          convenient location before running them. If you have copied the
          ``examples`` directory but still wish to use ``make`` then you
          will also have to set the ``PSYCLONE_CONFIG`` environment variable
          to the full path to the PSyclone configuration file, e.g.
          ``PSYCLONE_CONFIG=/some/path/psyclone.cfg make``.

.. _examples-compilation:

Compilation
-----------

Some of the examples support compilation (and some even execution of
a compiled binary). Please consult the ``README.md`` to check which ones
can be compiled and executed.

As mentioned above, by default each example will execute the
``transform`` target, which performs the PSyclone code transformation
steps. In order to compile the sources, use the target ``compile``:

.. code-block:: bash

    make compile

which will first perform the transformation steps before compiling
any created Fortan source files. If the example also supports running
a compiled and linked binary, use the target:

.. code-block:: bash

    make run

This will first trigger compilation using the ``compile`` target, and
then execute the program with any parameters that might be required
(check the corresponding ``README.md`` document for details).

All ``Makefile``\s support the variables ``F90`` and ``F90FLAGS`` to specify
the compiler and compilation flags to use. By default, the Gnu Fortran
compiler (``gfortran``) is used, and the compilation flags will be set
to debugging. If you want to change the compiler or flags, just define
these as environment variables:

.. code-block:: bash

    F90=ifort F90FLAGS="-g -check bounds" make compile

To clean all compiled files (and potential output files from a run),
use:

.. code-block:: bash

    make clean

This will clean up in the ``examples`` directory. If you want to change compilers
or compiler flags, you should run ``make allclean``, see the section
about :ref:`examples_dependencies` for details.

.. _supported-compilers:

Supported Compilers
-------------------

All examples have been tested with the following compilers.
Please let the developers know if you have problems using a compiler
that has been tested or if you are working with a different compiler
so it can be recorded in this table.

.. tabularcolumns:: |l|L|

======================= =======================================================
Compiler                Version
======================= =======================================================
Gnu Fortran             9.3
Intel Fortran           17, 21
NVIDIA Fortran          23.5
======================= =======================================================

.. _examples_dependencies:

Dependencies
------------

Any required library that is included in PSyclone (typically
the infrastructure libraries for the APIs, or :ref:`PSyData wrapper
libraries <libraries>`) will automatically be compiled with the same
compiler and compilation flags as the examples.

.. note:: Once a dependent library is compiled, changing the
          compilation flags will not trigger a recompilation
          of this library. For example, if an example is first compiled
          with debug options, and later the same or a different
          example is compiled with optimisations, the dependent library
          will not automatically be recompiled!

All ``Makefile``\s support an ``allclean`` target, which will not only
clean the current directory, but also all libraries the current
example depends on.

.. important:: Using ``make allclean`` is especially important if
               the compiler is changed. Typically, one compiler cannot
               read module information from a different compiler, and
               then compilation will fail.

NetCDF
++++++

Some examples require NetCDF for compilation. Installation of NetCDF
is described in detail in
`the hands-on practicals documentation
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals#user-content-netcdf-library-lfric-examples>`_.
