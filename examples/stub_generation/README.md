# PSyclone Stub Generation

PSyclone provides a utility to generate a Kernel's argument list and
its datatypes from the Kernel metadata. This is useful for people
developing new Kernels or modifying existing ones to make sure their
implementation has the correct number and type of arguments.

This directory gives an example of the use of the Kernel stub
generator. This particular example defines a Kernel with four fields,
3 of which have a stencil access.

To run this example type:

```sh
python ../../src/psyclone/gen_kernel_stub.py testkern_stencil_multi_mod.f90
```

More Kernel examples can be found in the following directory, most of
which start wth the name `testkern`:

[../../src/psyclone/tests/test_files/dynamo0p3](../../src/psyclone/tests/test_files/dynamo0p3)

For example:

```sh
python ../../src/psyclone/gen_kernel_stub.py ../../src/psyclone/tests/test_files/dynamo0p3/testkern_qr.F90
```
