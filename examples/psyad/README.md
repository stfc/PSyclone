# PSyAD Examples

This directory contains various examples of the use of PSyAD to
transform tangent linear code to its adjoint. See the READMEs in the
individual example directories for further details.

## Example 1

Simple, generic tangent linear example which is translated to its
adjoint form. A test harness is also generated which may be compiled
and executed to validate the adjoint.

## Example 2

An LFRic example using the kernel that computes the tangent-linear of
the hydrostatic balance term. A test harness for this kernel can also
be generated. However, it must be incorporated into an LFRic mini-app
in order to be compiled and executed.

## lfric

Creates and/or stores adjoint versions of all of the LFRic
tangent-linear kernels requested by the Met Office. At the current
time a test harness is not created for these kernels so they are not
automatically validated. As some of the tangent-linear kernels require
tweaks before `psyad` is able to translate them, this example stores
some manually modified tangent-linear kernels as well as the original
unmodified kernels. Further, as the adjoint kernels automatically
created by `psyad` require tweaks before they are valid adjoint
kernels, this example also stores manually modified adjoint kernels.
