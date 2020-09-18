# Libraries for use with PSyclone

This directory contains a base class as a Jinja template that can be used to simplify
the creation of PSyData-based libraries and various PSyData wrapper libraries.

## PSyData base class

The file ``psy_data_base.jinja`` contains a Jinja template that can be used
by PSyData-based wrapper libraries. Full documentation on using this template
is provided in the
[link developer's guide](https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja).
The script ``process.py`` is used by the derived classes to process this
template. There is a simple Makefile for compilation tests, but any PSyData
wrapper library (in any of the subdirectories here) will process this
template and compile it in their own directory (to allow for the required
types to be supported), they do not link with a compiled version from this
directory.


## `extract` directory

Contains code for extracting kernel data - i.e. all input and output of
a kernel invocation.

## `profiling` directory

Contains PSyData wrapper libraries for various profiling libraries and stand-alone timing
libraries.

## `read_only` directory

Contains PSyData wrapper libraries for verifying at run time that parameters declared
as read-only in the PSyclone meta data are indeed not changed in a subroutine.
