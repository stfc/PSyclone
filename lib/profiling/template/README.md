# PSyclone Wrapper Library Template

This is a simple example to help writing your own
PSyclone wrapper libraries.

## Compilation

```sh
gfortran -c dummy_lib.f90
```

The application needs to provide the template directory as module or include
path, and link with `dummy_lib.o`.
