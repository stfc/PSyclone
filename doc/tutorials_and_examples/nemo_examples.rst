NEMO Examples
=============

These examples may all be found in the ``examples/nemo`` directory.

Example 1: OpenMP parallelisation of tra_adv
--------------------------------------------

Demonstrates the use of PSyclone to parallelise loops in a NEMO
tracer-advection benchmark using OpenMP for CPUs and for GPUs.

Example 2: OpenMP parallelisation of traldf_iso
-----------------------------------------------

Demonstrates the use of PSyclone to parallelise loops in some NEMO
tracer-diffusion code using OpenMP for CPUs and for GPUs.

Example 3: OpenACC parallelisation of tra_adv
---------------------------------------------

Demonstrates the introduction of simple OpenACC parallelisation (using the
``data`` and ``kernels`` directives) for a NEMO tracer-advection benchmark.

.. _nemo-eg4-sir:

Example 4: Transforming Fortran code to the SIR
-----------------------------------------------

Demonstrates that simple Fortran code can be transformed to the Stencil
Intermediate Representation (SIR). The SIR is the front-end language to DAWN
(https://github.com/MeteoSwiss-APN/dawn), a tool which generates
optimised cuda, or gridtools code. Thus various simple Fortran
examples and the computational part of the tracer-advection benchmark
can be transformed to optimised cuda and/or gridtools code by using
PSyclone and then DAWN.

Example 5: Kernel Data Extraction
---------------------------------

This example shows the use of kernel data extraction in PSyclone for
generic Fortran code. It instruments each kernel in the NEMO tracer-advection
benchmark with the PSyData-based kernel extraction code. Detailed
compilation instructions are in the ``README.md`` file, including how
to switch from using the stand-alone extraction library to the NetCDF-based
one (see :ref:`extraction_libraries` for details).

Example 6: Read-only Verification
---------------------------------

This example shows the use of read-only verification with PSyclone for
generic Fortran code. It instruments each kernel in a small Fortran
program with the PSyData-based read-only verification code. Detailed
compilation instructions are in the ``README.md`` file.


Scripts
~~~~~~~

This contains examples of two different scripts that aid the use of PSyclone
with the full NEMO model. The first, `process_nemo.py` is a simple wrapper
script that allows a user to control which source files are transformed, which
only have profiling instrumentation added and which are ignored altogether.
The second, `kernels_trans.py` is a PSyclone transformation script which
adds the largest possible OpenACC Kernels regions to the code being processed.

For more details see the ``examples/nemo/README.md`` file.

Note that these scripts are here to support the ongoing development of PSyclone
to transform the NEMO source. They are *not* intended as 'turn-key' solutions
but as a starting point.
