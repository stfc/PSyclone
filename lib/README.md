# Libraries for use with PSyclone

This directory contains a base class as a Jinja template that can be used to simplify
the creation of PSyData-based wrapper libraries.

## PSyData base class

The file ``psy_data_base.jinja`` contains a Jinja template that can be used
by PSyData-based wrapper libraries. Full documentation on using this template
is provided in the
[developer's guide](https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja).
The script ``process.py`` is used by the derived classes to process this
template. There is a simple Makefile for compilation tests, but any PSyData
wrapper library (in any of the subdirectories here) will process this
template and compile it in its own directory. This allows each library
to exactly specify which data types are required. No library should
rely on a file compiled in this directory.


## `extract` directory

Contains code for extracting kernel data - i.e. all input and output parameters of
a kernel invocation.

## `nan_test`

Contains PSyData libraries for checking that input and output parameters of kernels
are valid numbers (i.e. not NaN or infinity).

## `profiling` directory

Contains PSyData wrapper libraries for various profiling libraries and stand-alone timing
libraries.

## `read_only` directory

Contains PSyData wrapper libraries for verifying at run time that parameters declared
as read-only in the PSyclone metadata are indeed not changed in a subroutine.

