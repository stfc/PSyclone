# PSyclone PSyAD Example1: creating an adjoint kernel and test harness.

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

This example demonstrates the use of PSyAD to create the adjoint of a
simple kernel (contained in `testkern_mod.f90`).
It also demonstrates the creation of a test harness for the adjoint kernel.

PSyAD can be run in the directory containing this file by executing, e.g.

```sh
make
```

Alternatively, PSyAD may be run from the command line as:

```sh
psyad -t -otest test_harness.f90 -oad testkernadj_mod.f90 testkern_mod.f90
```

This will generate two new files, `testkernadj_mod.f90` and `test_harness.f90`.

The Makefile also supports the `compile` target which will build
the kernel, its adjoint and the test harness. The `run` target will execute
the test harness giving output something like:

```sh
Running PSyAD-generated test harness...
 Test of adjoint of 'testkern_code' passed: diff =    0.0000000000000000
...done.
```

Note, you may find that the test fails, but if so the diff should be
relatively small.
