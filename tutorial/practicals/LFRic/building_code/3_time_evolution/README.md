# Tutorial 3: Time-evolution of a field on a planar mesh

In the [first tutorial](../1_simple_kernels) we learned how to create and
use kernels for mathematical operations on field data. In the
[second tutorial](../2_built_ins) we learned how to use PSyclone built-ins
for simple linear algebra operations that update fields.

In this tutorial we will build on this knowledge by using kernels for
more complicated (and realistic) mathematical operations and built-ins
for e.g. field initialisation and simple operations. This is the usual
way of utilising kernels and built-ins in LFRic.

Specifically, we will use kernels for two goals: initialisation of
a "perturbation signal" field to the prescribed analytical function
and the time evolution of that field.

The time evolution means that we need to have a **timestepping loop**.
In LFRic the timestepping is handled by the [driver layer](
../background/LFRic_structure.md) and this tutorial follows this
practice. The tutorial also demonstrates the use of setting up the
model configuration parameters via namelists and calling I/O
diagnostics routines to output a "model state" that can be plotted.
We use namelist parameters in all three model layers,
[driver](../background/LFRic_structure.md#driver-layer),
[algorithm](../background/LFRic_structure.md#algorithm-layer) and
[kernels](../background/LFRic_structure.md#kernel-layer).

As in the previous examples, we will only be **completing and creating
algorithm and kernel code**. The use of other supplied materials will
be explained in the specific steps of the tutorial.

The working directory for this part of the tutorial is
`<PSYCLONEHOME>/tutorial/practicals/LFRic/building_code/3_time_evolution`
where `<PSYCLONEHOME>` is the full path to the local PSyclone repository.

## Supporting source and scripts

We will use the following modules to modify, create and call kernels
and built-ins in this tutorial:

* [`init_perturbation_kernel_mod.f90`](init_perturbation_kernel_mod.f90),
  a kernel that initialises a perturbation field on the `W3` function space
  to the analytically-prescribed bell-shaped function (the kernel loop body
  that initialises a field needs to be completed);

* [`time_evolution_alg_mod.x90`](time_evolution_alg_mod.x90), an example
  of an LFRic-like algorithm that calls kernels to initialise and propagate
  the perturbation field and runs one model timestep (the `invoke` calls
  and simple field checks via the `log_minmax` field class procedure
  need to be completed).

Specific information on how to complete and use the above kernel and
algorithm code is given in the specific steps in this tutorial. The
[*Algorithm structure*](#algorithm-structure) section below outlines
the role of the algorithm layer in this tutorial.

Other supporting modules and libraries that we will use and that require
no modifications are:

* [`time_evolution_driver.f90`](time_evolution_driver.f90), an LFRic-like
  main program that sets up the model run, runs the main timestep loop by
  calling routines from the `time_evolution_alg_mod.x90` subroutines and
  outputs results calling an I/O routine from the
  `write_diagnostics_mod.f90` module;

* [`write_diagnostics_mod.f90`](write_diagnostics_mod.f90) calls a diagnostic
  output routine to write the "model state" consisting of the coordinate and
  perturbation fields to a file;

* [`gungho_lib`](gungho_lib), a collection of the LFRic infrastructure and
  science-like libraries for assigning coordinate fields, reading namelists
  and outputting results.

Utilities to build and run the code and read the input parameters are:

* [`Makefile`](Makefile) script that builds the executable program
 `time_evolution` (does not need to be modified);

* [`configuration.nml`](configuration.nml), a namelist file that sets
  parameters required to run the model (e.g. mesh file name, domain top,
  perturbation parameters). The namelists **`extrusion_uniform`**,
  **`perturbation_bell`** and **`timestepping`** can be modified to explore
  different model height and vertical resolution, perturbation field
  behaviour and timestepping options;

* `mesh_planar100x100-1000x1000.nc`, an input 2D global planar mesh in the
  NetCDF-based UGRID format to create the model domain (3D partitioned
  mesh) from (does not need to be modified). The horizontal limits of
  the mesh are written down in the *`domain_size`* namelist (must not be
  modified) in the `configuration.nml` file. Viewing the file requires
  [`ncdump` utility](
  https://www.unidata.ucar.edu/software/netcdf/docs/netcdf_utilities_guide.html#ncdump_guide).

* [`plot_xy_slices_ex3.py`](plot_xy_slices_ex3.py), a Python plotting script
  for model outputs. It takes output of a `model_state_tstep_*.txt` file name
  (where `*` stands for an integer-valued string denoting timestep index)
  and a string of comma-separated model levels in the range of
  `[0, number_of_layers]`. E.g.

  ```python
  python plot_xy_slices_ex3.py model_state_tstep_10.txt '0,2,4'
  ```
  will return plots of model output at the timestep 10 and the listed
  levels. To plot one level just supply one number.

### Driver structure

`time_evolution_driver.f90` follows the order of setting up LFRic object
stack outlined in [this full NetCDF LFRic example](
../../../../../examples/lfric/full_example_netcdf/README.md):
**global 2D mesh** read from NetCDF file -> **partition** ->
**local 3D mesh** -> **function space** -> **field**. In LFRic
all this information is read from configuration namelists by utilising
the generated Fortran files for processing namelists. This process
is mimicked here with the supplied namelist file `configuration.nml`
and `<namelist_name>_config_mod.f90` files in `gungho_lib` directory
that process the namelist inputs.

The Fortran calls to create LFRic infrastructure objects are pretty
much the same as in the [simple kernels](
../1_simple_kernels/README.md#driver-structure) and
[built-ins](
../2_built_ins/README.md#driver-structure) tutorials. The two exceptions are
outlined below.

1. Reading a global 2D mesh from a NetCDF file
   ```fortran
   global_mesh = global_mesh_type(filename, prime_mesh_name)
   global_mesh_ptr => global_mesh
   ```
   where the `filename` of the input mesh file as well as the `prime_mesh_name`
   (mesh data object name) are read from the configuration namelist file
   [`configuration.nml`](configuration.nml) and

2. Creating a field through copying the properties (but not the data) of
   another field on a same function space instead of using a pointer to a
   function space object. This is another useful procedure from the LFRic
   field class and here it is utilised to create the coordinate fields
   `chi(3)` from the `perturbation` field as:
   ```fortran
   do i = 1, size(chi)
     write(cind, '(I5)') i  ! Convert integer index to string
     call perturbation%copy_field_properties( &
                       chi(i), name="chi_"//trim(adjustl(cind)))
   end do
   ```

---
**NOTE**

The coordinate fields `chi(3)` in this tutorial are an example of a
[field vector object](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#field-vector)
which is essentially a bundle of fields. The metadata representation
of a field vector will be shown in the [Step 1](#step-1) below where
we update the relevant kernel.

---

As said above, this driver also shows two other functions that the driver
layer in LFRic performs: diagnostic output (or checkpoint/restart) and the
main model timestepping loop. The main timestepping loop is very similar
to the LFRic code

```fortran
  do tstep = timestep_start, timestep_end
    call time_evolution_alg_step(perturbation, chi, tstep)
  end do
```

where these loops call algorithm subroutines that calculate the
"model state" for one timestep.

### Algorithm structure

As said in the LFRic [*Algorithm layer section*](
../background/LFRic_structure.md#algorithm-layer) section, it is usual for
an algorithm to have multiple subroutines, `<base_name>_init`
(initialisation of the required algorithm data), `<base_name>_step`
(calculations of model state in one timestep) and `<base_name>_final`
(usually clean-up of memory assigned to the objects used in algorithms).

[`time_evolution_alg_mod.x90`](`time_evolution_alg_mod.x90`) illustrates
this functionality with the subroutines

* `time_evolution_alg_init` that initialises the perturbation field and
* `time_evolution_alg_step` that calculates the field propagation for one
  timestep.

As in the previous two tutorials, this algorithm also calls the LFRic field
class `log_minmax` procedure for quick checks of the the minimum and maximum
values of the fields after calling the kernels that update them. One of the
tasks in this tutorial is to create these calls.

## Tutorial exercise

### Step 1: Complete the `init_perturbation_kernel_mod.f90` kernel

Navigate to the working directory for this part of the tutorial and open
the supplied kernel stub [`init_perturbation_kernel_mod.f90`](
init_perturbation_kernel_mod.f90) in an editor. Let us first take a look
at the metadata for this kernel

```fortran
    type(arg_type), dimension(2) :: meta_args = (/ &
         arg_type(GH_FIELD,   GH_READWRITE, W3),   &
         arg_type(GH_FIELD*3, GH_READ,      W3)    &
         /)
```

The first `arg_type` descriptor is for the `perturbation` field on the
`W3` function space that is updated (`GH_READWRITE` access for discontinuous
function spaces). The second `arg_type` descriptor is for the `chi(3)`
coordinate [field vector](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#field-vector)
(see the [note above](#driver-layer)) on the same space as the `perturbation`
field.

The coordinate fields are used to initialise the `perturbation` field to the
space-varying analytical function

*p(x,y,z) = A(z)\*exp\{-\[(x - x<sub>c</sub>)/hw<sub>x</sub>\]<sup>2</sup>
\- \[(y - y<sub>c</sub>)/hw<sub>y</sub>\]<sup>2</sup>*\}

where *A(z) = max(p<sub>0</sub> - z, 0)/s<sub>p</sub>* is the
height-dependent amplitude of the perturbation.

The meaning of the symbols is as follows:

* *x*, *y*, *z* - model coordinates `chi(3)` on the `W3` function space
 (calculated by the supplied LFRic routine `assign_coordinate_field_mod.F90`
 in the `gungho_lib` directory);
* *p(x,y,z)* - a bell-shaped perturbation field on the `W3` function space;
* *p<sub>0</sub>* - maximum height of the perturbation (not higher than
  the model domain top);
* *s<sub>p</sub>* - scaling factor for the amplitude of perturbation;
* *x<sub>c</sub>* and *y<sub>c</sub>* - *x* and *y* coordinates of
  the centre of the perturbation field (within the domain limits);
* *hw<sub>x</sub>* and *hw<sub>y</sub>* - half-widths of the
  perturbation signal in *x* and *y* direction.

These parameters can be set in the `perturbation_bell` namelist located
in the configuration file [`configuration.nml`](configuration.nml):
`perturbation_height` (*p<sub>0</sub>*), `perturbation_scale`
(*s<sub>p</sub>*), `x_centre` (*x<sub>c</sub>*), `y_centre`
(*y<sub>c</sub>*), `half_width_x` (*hw<sub>x</sub>*) and
`half_width_y` (*hw<sub>y</sub>*).

The parameters are read and processed by the
`perturbation_bell_config_mod.f90` configuration module in the `gungho_lib`
directory and are accessed by the kernel through the relevant `use`
statement:

```fortran
  use perturbation_bell_config_mod, &
                         only: half_width_x, half_width_y, &
                               perturbation_scale,         &
                               perturbation_height,        &
                               x_centre, y_centre
```

The task in this step is to complete the kernel loop body to the
above-defined analytical expression. To help with the process there is
a rough code representation of this expression in the kernel module header

```fortran
!   perturbation = ampl(z)*exp( -((x - x_centre)/half_width_x)**2 &
!                               -((y - y_centre)/half_width_y)**2 )
!   where ampl(z) = max(perturbation_height - z, 0)/perturbation_scale
```

and also local kernel variables `x(3)`, `xt`, `yt`, `ampl` for the DoF-wise
access to the coordinate fields data and storing the intermediate calculation
results for the transformed `x` and `y` coordinates and the height-varying
perturbation amplitude. For instance, the expression
*(x - x<sub>c</sub> - u\*t<sub>tot</sub>)* can be written in the loop body as

```fortran
xt = ( x(1) - x_centre )/half_width_x
```

and then use to calculate the value of the `perturbation` field for each
DoF in the loop (`perturbation( map_w3(df) + k ) = ...`).

You should now have the completed `init_perturbation_kernel_mod.f90`
kernel. To check that everything is correct, look into the completed
kernel in the [Solutions directory](solutions).

### Step 2: Initialise the perturbation field

After completing `init_perturbation_kernel_mod.f90`, complete the
initialisation of `perturbation` field in the `time_evolution_alg_init`
subroutine of [`time_evolution_alg_mod.x90`](time_evolution_alg_mod.x90):

* Use built-ins to set the perturbation field to 0;
* Call kernel `init_perturbation_kernel_mod.f90` to initialise the
  perturbation field to the prescribed analytical function.

Open the supplied algorithm source,
[`time_evolution_alg_mod.x90`](time_evolution_alg_mod.x90), in an
editor and look for the comment that marks the place to complete
the `invoke` call in the `time_evolution_alg_init` subroutine,
`! TO COMPLETE (in the same invoke):`. Create the appropriate `invoke`
call to the `setval_c` built-in and `init_perturbation_kernel_type`.

After that check the minimum and maximum values of the perturbation
field by using the `log_minmax` function (use algorithms from
previous examples as a reference, e.g. [`simple_kernels_alg_mod.x90`](
../1_simple_kernels/part2/simple_kernels_alg_mod.x90).

### Step 3: Create the `prop_perturbation_kernel_mod.f90` kernel

In this step we will use the completed `init_perturbation_kernel_mod.f90`
as a template to create a kernel called `prop_perturbation_kernel_mod.f90`
that propagates the perturbation signal in the *x* and *y* direction with
time by replacing

- *(x - x<sub>c</sub>)* with *(x - x<sub>c</sub> - u\*t<sub>tot</sub>)* and
- *(y - y<sub>c</sub>)* with *(y - y<sub>c</sub> - v\*t<sub>tot</sub>)*.

Here *u* and *v* are constant-valued velocity components in *x* and
*y* directions, respectively, and can be set in the `perturbation_bell`
namelist as `u_vel` and `v_vel`.

*t<sub>tot</sub>* is the total time that is calculated as the timestep
size in seconds, *dt*, multiplied by the current timestep passed from the
[`time_evolution_driver.f90`](time_evolution_driver.f90). The timestep
size, `dt`, and the start and end of timestepping loop, `timestep_start`
and `timestep_end`, can be set in the `timestepping` namelist in the
[configuration file](configuration.nml).

First copy the `init_perturbation_kernel_mod.f90` into the new kernel
`prop_perturbation_kernel_mod.f90` and then open the new kernel in an
editor to change the code unit names accordingly (e.g. `init_perturbation_`
becomes `prop_perturbation_`).

Now we need to make sure that the kernel uses the


*Tips:*

* What additional information needs to be passed to the kernel from
  the `time_evolution_alg_step` subroutine for time propagation of the
  field? How does this reflect on the metadata?
* What additional information from the namelists needs to be used in
  the kernel for time propagation?

### Step 4

After completing `prop_perturbation_kernel_mod.f90`, complete the
time propagation of the `perturbation` field in the
`time_evolution_alg_step` subroutine of [`time_evolution_alg_mod.x90`](
time_evolution_alg_mod.x90):

* Calculate total propagating `t_tot` from `timestep` and `dt`;
* Call kernel `prop_perturbation_kernel_mod.f90` to propagate the
  perturbation in `x` and `y` directions with each timestep.

Also, check the minimum and maximum values of the perturbation
field using field's `log_minmax` function (use algorithms from previous
examples as reference, e.g. [`simple_kernels_alg_mod.x90`](
../1_simple_kernels/simple_kernels_alg_mod.x90).

### [Solutions](solutions)

To check for the correct results, navigate to the `solutions` directory
and run `make` to build the `time_evolution` executable.
