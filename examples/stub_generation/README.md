# PSyclone Kernel Stub Generation

PSyclone provides a utility, `psyclone-kern`, that is able to
take Kernel metadata and generate an appropriate Fortran subroutine
with the correct arguments and datatypes. This is useful for people
developing new Kernels or modifying existing ones to make sure their
implementation has the correct number and type of arguments.

This directory gives an example of the use of `psyclone-kern` for
Kernel stub generation. This particular example defines a Kernel with
four fields, 3 of which have a stencil access.

To run this example type:

```sh
psyclone-kern -gen stub ./testkern_stencil_multi_mod.f90
```

More Kernel examples can be found in the following directory, most of
which start wth the name `testkern`:

[../../src/psyclone/tests/test_files/dynamo0p3](../../src/psyclone/tests/test_files/dynamo0p3)

For example:

```sh
psyclone-kern -gen stub ../../src/psyclone/tests/test_files/dynamo0p3/testkern_qr.F90
```
