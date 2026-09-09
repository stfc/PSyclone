This directory is where generated adjoint kernels are placed when they
need further manual tweaks to make them work. As they are generated,
it is not necessary to keep them in the repository. To create them,
move up one directory and make use of the Makefile i.e. `cd ..;make`.

The tweaked versions of these kernels are stored in the `../adjoint`
directory. The tweaks can be easily seen using `diff`. For example,
after creating the adjoint code (`cd ..;make`), run `diff
adj_matrix_vector_kernel_mod.F90
../adjoint/adj_matrix_vector_kernel_mod.F90`
