# Example 3: Time-evolution of a field on a planar mesh

### Step 1

Complete the supplied `init_perturbation_kernel_mod.f90` that initialises
a perturbation field on `W3` function space to the analytical function

*p(x,y,z) = A(z)\*exp(-((x - x<sub>c</sub>)/hw<sub>x</sub>)<sup>2</sup>
- ((y - y<sub>c</sub>)/hw<sub>y</sub>)<sup>2</sup>*

where *A(z) = max(p<sub>0</sub> - z, 0)/s<sub>p</sub>*.

The symbols are as follows:

* *x*, *y*, *z* - model coordinates on `Wchi` function space
 (calculated by the supplied LFRic routine `assign_coordinate_field_mod.F90`
 in the `gungho_lib` directory;
* *p(x,y,z)* - a bell-shaped perturbation field;
* *p<sub>0</sub>* - maximum height of the perturbation (not higher than
  the model domain top);
* *s<sub>p</sub>* - scaling factor for the amplitude of perturbation;
* *x<sub>c</sub>* and *y<sub>c</sub>* - *x* and *y* coordinate of
  the centre of the perturbation field (within the domain limits);
* *hw<sub>x</sub>* and *hw<sub>y</sub>* - half-widths of the
  perturbation signal in *x* and *y* direction.

These parameters can be set in the `perturbation_bell` namelist located
in the configuration file `configuration.nml`: `perturbation_height`
(*p<sub>0</sub>*), `perturbation_scale` (*s<sub>p</sub>*), `x_centre`
(*x<sub>c</sub>*), `y_centre` (*y<sub>c</sub>*), `half_width_x`
(*hw<sub>x</sub>*) and `half_width_y` (*hw<sub>y</sub>*).

### Step 2

After completing the kernel, complete the initialisation of `perturbation`
field in  the `example3_alg_init` subroutine:

* Use built-ins to set `perturbation` to 0;
* Call kernel `init_perturbation_kernel_mod.f90` to set the perturbation
  field to the prescribed analytical function.

Also, check the minimum and maximum values of coordinate and perturbation
fields using field's `log_minmax` function (use algorithms from previous
examples as reference, e.g. [`example1_alg_mod.x90`](
../example1/example1_alg_mod.x90).

### Step 3

Use the completed `init_perturbation_kernel_mod.f90` as a template to
create a kernel called `prop_perturbation_kernel_mod.f90` that propagates
the perturbation signal in x and y direction with time by replacing
*(x - x<sub>c</sub>)* with *(x - x<sub>c</sub> - u\*t<sub>tot</sub>)*
and *(y - y<sub>c</sub>)* with *(y - y<sub>c</sub> - v\*t<sub>tot</sub>)*.

Here *u* and *v* are constant-valued velocity components in *x* and
*y* directions, respectively, and can be set in the `perturbation_bell`
namelist as `u_vel` and `v_vel`.

*t<sub>tot</sub>* is the total time that is calculated as *dt* multiplied
by the current timestep passed from the `example3_driver`. The timestep size
in seconds, `dt` and the start and end of timestepping loop, `timestep_start`
and `timestep_end` can be set in the `timestepping` namelist in the
configuration file.

*Tips:* What additional information needs to be passed to the kernel from
the `example3_alg_step` subroutine for time propagation of the field? How
does this reflect on the metadata?

[**Link to solutions**](solutions) (run `make` in the directory
to build and check results).

## Accompanying materials:

* `example3_alg_mod.x90` - an example of LFRic algorithm that
  calls kernels to initialise fields and runs one model timestep (the
  `invoke` calls need to be completed);
* `Makefile` to build the code;
* `example3_driver.f90` - LFRic-like main program that sets up
  the model run, runs the main timestep loop by calling routines
  from `example3_alg_mod.x90` and outputs results by calling
  routines from `diagnostic_alg_mod.x90` (does not need to be modified);

* `diagnostic_alg_mod.x90` - calls kernels to map fields from one space
  to another for diagnostic output and calls I/O routine (does not need
  to be modified);
* `init_perturbation_kernel_mod.f90` - kernel that initialises a field
  on `W3` function space to analytically prescribed function (needs to
  be completed).

* `configuration.nml`
* `gungho_lib`
* `init_perturbation_kernel_mod.f90`
* `mesh_planar100x100-1000x1000.nc`
* `nodal_coordinates_kernel_mod.F90`
* `plot_xy_slices_ex3.py`

### Driver structure

`example3_driver.f90` follows the order of setting up LFRic object
stack outlined in [this full NetCDF LFRic example](
../../../examples/lfric/full_example_netcdf/README.md):
**global 2D mesh** read from NetCDF file -> **partition** ->
**local 3D mesh** -> **function space** -> **field**. In LFRic
all this information is read from configuration namelists by utilising
the generated Fortran files for processing namelists. This process
is mimicked here with the supplied namelist file `configuration.nml`
and `<namelist_name>_config_mod.f90` files in `gungho_lib` directory
to process the namelist inputs.

This example also shows two other functions that the driver layer in
LFRic performs: diagnostic output (or checkpoint/restart) and the main
model time loop. In this case `diagnostic_alg_mod.x90` prepares fields
for output, however the diagnostic routines do not need to be algorithms
if no kernel or built-in calls are required to process the fields. What
is very similar to LFRic code is the main timestepping loop

```fortran
  do tstep = timestep_start, timestep_end
    call example3_alg_step(perturbation, chi, tstep)
  end do
```

that calls an algorithm to calculate the "model state" for one timestep.

### Algorithm structure

As said in the [*LFRic algorithm*](
../background/LFRic_algorithm.md) section, it is usual for an
algorithm to have multiple subroutines, `<base_name>_init`
(initialisation of the required algorithm data), `<base_name>_step`
(calculations of model state in one timestep) and `<base_name>_final`
(usually clean-up of memory related to objects used in algorithms).

`example3_alg_mod.x90` follows illustrates this functionality with
subroutines `example3_alg_init` that initialises the perturbation field
and `example3_alg_step` that calculates the field propagation for one
timestep.

Algorithms can also perform auxiliary functions such as processing fields
for output which is illustrated in `diagnostic_alg_mod.x90`. Here the
subroutine `diagnostic_alg_init` maps the model coordinate fields `chi(3)`
from their designated function space to the function space of the main
model field, perturbation. This is required for the `diagnostic_alg_write`
subroutine that outputs the "model state" consisting of coordinates and
the diagnostic field.
