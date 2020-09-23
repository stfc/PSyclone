# ReadOnly Verification Libraries

This directory contains files related to read-only verification, i.e. checks at
runtime that a read-only parameter of a subroutine is indeed not changed in a kernel.
There is a base class as a Jinja template that can be used to simplify
the creation of API-specific wrapper libraries.

## ReadOnly base class

The file `read_only_base.jinja` contains a Jinja template that is used
by the dl_esm_inf- and LFRic-specific wrapper libraries. It implements
the required PSyData API calls for Fortran base types (scalar and arrays).
Full documentation to the Jinja implementation of a PSyData base class is in the
[developer's guide](https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja).
The script `process.py` is used by the derived classes to process this template.
There is a simple Makefile contained here for compilation tests, but any API-specific
implementation (in any of the subdirectories here) will process this
template and compile it in their own directory (to allow for the required
data types to be supported), they do not link with a compiled version from this
directory.


## `dl_esm_inf` directory

Contains the read-only PSyData wrapper library for the dl_esm_inf API.

## `lfric` directory

Contains the read-only PSyData wrapper library for the LFRic API.
