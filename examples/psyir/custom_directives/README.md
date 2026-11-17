# Use custom directives

This directory contains an example of how write PSyclone scripts to transform code
by taking advantage of information provided by the use of custom Fortran code directives.

The Fortran code contains a custom directive, ``!$my_dir no_par`` which is
used by the PSyclone script to skip attempting to parallelise a specific loop.

The example can be compiled using the Makefile, but this is a simple operation:
``psyclone --keep-directive -s identify_custom_directives.py directive_filtering.F90``.
The ``--keep-directives`` option is essential to enabling the directive of interest
to appear in the tree.
