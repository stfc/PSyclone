This directory contains the tangent-linear kernels for which the Met
Office require adjoint versions and Makefile's to generate the adjoint
versions of these kernels using `psyad`.

The `Makefile` in this directory creates adjoint kernels from the
tangent-linear kernels in the `tangent_linear` and
`tangent_linear_tweaked` directories and places the generated adjoint
kernels in either the `adjoint_partial` or `adjoint` directories.

The reason for having two directories containing the tangent-linear
kernels is that some required manual modification before `psyad` is
able to process them. These kernels are copied from the
`tangent_linear` directory, manually modified and placed in the
`tangent_linear_tweaked` directory.

Similarly, there are two directories containing the adjoint kernels
because some require manual modification after 'psyad' has processed
them. These kernels are copied from the `adjoint_partial` directory,
manually modified and placed in the `adjoint` directory. At the
present time all generated adjoint kernels need some manual
modification so none are placed directly into the `adjoint` directory
by `psyad`.

The original tangent-linear kernels (stored in the `tangent_linear`
directory) are copied from the LFRic repository with no changes and
are stored here purely for convenience. These kernels are taken from
the branch
https://code.metoffice.gov.uk/svn/lfric/LFRic/branches/dev/christinejohnson/r36316_tl_example
(last changed date: Wednesday the 6th of July 2022), as that is what
the Met Office are also working from at this time (January 2023).
