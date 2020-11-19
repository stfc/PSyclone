# Example 3: Time-evolution of a field on a planar mesh

Accompanying materials:

* `Makefile` to build the code;
* `example3_driver.f90` - LFRic-like main program that sets up
  the model run, runs the main timestep loop by calling routines
  from `example3_alg_mod.x90` and outputs results by calling
  routines from `diagnostic_alg_mod.x90` (does not need to be modified);
* `example3_alg_mod.x90` - an example of LFRic algorithm that
  calls kernels to initialise fields and runs one model timestep (the
  `invoke` calls need to be completed);
* `diagnostic_alg_mod.x90` - calls kernels to map fields from one space
  to another for diagnostic output and calls I/O routine (does not need
  to be modified);
* `init_perturbation_kernel_mod.f90` - kernel that initialises a field
  on `W3` function space to analytically prescribed function (needs to
  be completed).

-rw-r--r--. 1 ikavcic users   1132 Nov 10 14:23 configuration.nml
drwxr-xr-x. 2 ikavcic users   4096 Nov 10 14:23 gungho_lib
-rw-r--r--. 1 ikavcic users   6938 Nov 10 14:23 init_perturbation_kernel_mod.f90
-rw-r--r--. 1 ikavcic users 967360 Nov 10 14:23 mesh_planar100x100-1000x1000.nc
-rw-r--r--. 1 ikavcic users   5180 Nov 10 14:23 nodal_coordinates_kernel_mod.F90
-rw-r--r--. 1 ikavcic users   5014 Nov 10 14:23 plot_xy_slices_ex3.py
