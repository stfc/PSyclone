# Tutorial 2: Use PSyclone built-ins

In the [first tutorial](../1_simple_kernels) we used [kernels](
../1_simple_kernels/LFRic_kernel_structure.md) for simple mathematical
linear algebra operations, such as setting field to a value and adding fields.
PSyclone provides [built-ins support](
https://psyclone.readthedocs.io/en/stable/built_ins.html) for such simple
operations.

In this tutorial we will learn how to utilise the
[LFRic (Dynamo 0.3) API built-ins](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins)
support in PSyclone instead of using kernels. A quick introduction to
built-ins is also given in the [section below](#quick-intro-to-built-ins).

We will modify the supplied algorithm [`builtins_alg_mod.x90`](
builtins_alg_mod.x90) to

* Create fields on a specified function space,
* Call the required built-ins,
* Print out the min/max values of the modified fields.

The working directory for this part of the tutorial is
```
<PSYCLONEHOME>/tutorial/practicals/LFRic/building_code/2_built_ins
```
where `<PSYCLONEHOME>` is the full path to the local PSyclone repository.

## Supporting source and scripts

We will use the following module to call built-ins in this tutorial:

* [`builtins_alg_mod.x90`](builtins_alg_mod.x90), an example of an LFRic
  [algorithm](../background/LFRic_structure.md#algorithm-layer) that sets
  up fields on `W0` and `W3` function spaces and performs simple mathematical
  operations via a group of `invoke` calls to PSyclone LFRic API built-ins
  (`invoke` call needs to be completed).

As in the [simple kernels tutorial](
../1_simple_kernels/README.md#algorithm-structure),
[`builtins_alg_mod.x90`](builtins_alg_mod.x90) creates *function space*
and *field* objects with the input mesh and finite-element order
information from the driver layer. The names of fields on `W0` and `W3`
function spaces and the general set-up is identical to the
[`simple_kernels_alg_mod.x90`](
../1_simple_kernels/part2/simple_kernels_alg_mod.x90) from the
[Part 2 of Tutorial 1](../1_simple_kernels/part2).

We will also use the following utilities to build and run the code:

* [`builtins_driver.f90`](builtins_driver.f90), an example of an LFRic-like
  [main program (driver)](../background/LFRic_structure.md#driver-layer)
  that creates the required LFRic objects and calls the algorithm code in
  `builtins_alg_mod.x90`.

* `Makefile` script that builds the executable program called `builtins`.

These utilities do not need to be modified. The `builtins_driver.f90` sets
up the LFRic object stack as outlined in the
[overview of the LFRic driver layer](
../background/LFRic_structure.md#driver-layer) and its structure is
identical to the driver structure of the
[simple kernels tutorial](
../1_simple_kernels/README.md#driver-structure), so please look there
for the explanation of the relevant Fortran calls.

## Tutorial exercise

The provided [`builtins_alg_mod.x90`](builtins_alg_mod.x90) source
defines output and two input fields on function spaces `W0` and `W3`,
respectively. It outlines the creation of fields on the `W0` function
space and prints out their `min` and `max` values. In summary, the
tasks in this example are:

* Create the output and input fields on the `W3` space using the `W0`
  fields as a template;

* Use built-ins to perform various mathematical operations on the fields
  (listed in the *Step 1* below).

### Step 1: Create fields on the `W3` function space

Open the supplied algorithm source, `builtins_alg_mod.x90`, in an editor and
look for this comment, `TO COMPLETE: Create fields on W3 function space`.

We will use the LFRic field class `initialise` procedure to create the output
field `field_out_w3` and two input fields, `field1_in_w3` and `field2_in_w3`,
on the `W3` function space.

Look at the `W0` fields creation code above the comment as a template, e.g.

```fortran
    ! Create fields on W0 function space
    call field_out_w0%initialise( vector_space = fs_w0_ptr, &
                                  name = "field_out_w0" )
```

and change the function space pointer and the field name accordingly. We need
to do this for the input `W3` fields, too.

### Step 2: Create an `invoke` that calls built-ins

After completing the creation of the `W3` fields we will create a single
`invoke` that performs the above specified mathematical operations. We will
place the code below the comment line
`! TO COMPLETE (in the same invoke) - Use built-ins to`.

This step consists of applying the following mathematical operations:

1. Initialise output `field_out_w0` to `0`,
2. Initialise input `field1_in_w0` to `1`,
3. Initialise input `field2_in_w0` to `2`,
4. Calculate `field_out_w0 = field1_in_w0 + field2_in_w0`,
5. Initialise output `field_out_w3` to `0`,
6. Initialise input `field1_in_w3` to `-1`,
7. Calculate `field2_in_w3 = 2*field1_in_w3`,
8. Calculate `field_out_w3 = field1_in_w3 - 0.5*field2_in_w3`.

The following built-ins need to be used for the operations above:

* [`setval_c` built-in](
  https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#setval-c) for
  initialising (setting) fields to a scalar value (e.g. `field_out_w0 = 0`);

* [`X_plus_Y` built-in](
  https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#x-plus-y)
  for adding two fields (`field_out_w0 = field1_in_w0 + field2_in_w0`);

* [`a_times_X` built-in](
  https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#a-times-x) for
  multiplying a field by a scalar value (`field2_in_w3 = 2*field1_in_w3`);

* [`X_minus_bY` built-in](
  https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#x-minus-by) for
  subtracting scaled fields (`field_out_w3 = field1_in_w3 - 0.5*field2_in_w3`).

For more information on how the built-ins are named, look into the
[*Naming scheme* section](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#naming-scheme)
of the [PSyclone LFRic API built-ins documentation](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins).

The calls to some of the listed built-ins are outlined below:

```fortran
    call invoke( setval_c(field_out_w0, 0.0_r_def),                  &
                 setval_c(field_out_w0, 1.0_r_def),                  &
                 ...
                 X_plus_Y(field_out_w0, field1_in_w0, field2_in_w0), &
                 setval_c(field_out_w3, 0.0_r_def),                  &
```

As in the [Part 2](../1_simple_kernels/part2/README.md) of the simple
kernel tutorial (see the *Step 3* there), we are passing literal scalar
values, e.g. `0.0_r_def`, as arguments to the built-in calls (in LFRic we
specify the Fortran `kind` of a literal value, e.g. `r_def` for the
`real`-valued literals in these calls).

---
**NOTE**

Sometimes PSyclone will generate invalid PSy-layer code if the user
makes a mistake in the ordering of the kernel arguments. This happens
because there is currently no validation of the arguments supplied in
the Algorithm layer. This support will be implemented in the future.

---

Copy the `invoke` stub above to the algorithm source and complete it. As
this is a long `invoke`, it is advisable to add one built-in call at a time
and check that the code builds with PSyclone in between. E.g. start with:

```fortran
    call invoke( setval_c(field_out_w0, 0.0_r_def) )
```

and then process the modified algorithm by calling `make test` as outlined
in the *Step 3* below. Then add the next built-in:

```fortran
    call invoke( setval_c(field_out_w0, 0.0_r_def), &
                 setval_c(field_out_w0, 1.0_r_def) )
```

and repeat the process (and so on for the subsequent built-in calls).

---
**NOTE**

There is one major difference from the algorithm code in the
first tutorial, [`simple_kernels_alg_mod.x90`](
../1_simple_kernels/part2/simple_kernels_alg_mod.x90), and this algorithm,
and that is the lack of the algorithm-level `use` statements for the
built-in classes. The built-in kernel classes are defined in
PSyclone (see the [*Quick intro* section](#quick-intro-to-built-ins)
below).

---

### Step 2A (optional): Use a name for the `invoke` call

As each `invoke` call generates a PSy-layer subroutine (see the
*Step 3* below for the algorithm code processing), it can be
easier to specify a [label (name) for an `invoke`](
https://psyclone.readthedocs.io/en/stable/algorithm_layer.html#named-invokes)
(and hence the subroutine) call to make it easier to search for in the
generated code. To do this here modify the completed `invoke` by adding a
name in the first line of the `invoke` call, e.g.

```fortran
    call invoke( name = "builtins_on_w0_and_w3_fields",              &
                 setval_c(field_out_w0, 0.0_r_def),                  &
                 ...
                 X_plus_Y(field_out_w0, field1_in_w0, field2_in_w0), &
                 ... )
```

The completed algorithm can be found in the [`solutions` directory](solutions).

### Step 3: Use PSyclone to generate algorithm and PSy-layer source

We will first check that the built-in calls in the algorithm code are
correct by running `make test` to process the algorithm source with
PSyclone. As explained in the *Step 2* above, it is best doing this as
the `invoke` is extended before the final check in this step. If the code
is correct, this will result in the generated algorithm source file
`builtins_alg_mod.f90` and the generated PSy-layer source file
`builtins_alg_mod_psy.f90`.

Looking at the generated algorithm file `builtins_alg_mod.f90` we can
see that the original `invoke` call to built-ins has become a call
to a single subroutine with all of the field arguments that were passed
to the built-ins:

```fortran
    CALL invoke_0(field_out_w0, field1_in_w0, field2_in_w0, field_out_w3, &
         field1_in_w3, field2_in_w3)
```

(if the `invoke` call was named in the *Step 2A* above, the subroutine
name would be `invoke_builtins_on_w0_and_w3_fields`).

The generated `invoke` subroutine code is located in the generated PSy-layer
file, `builtins_alg_mod_psy.f90`. We will look at the generated code for
just the `X_plus_Y` built-in below.

As for a call to a user-supplied kernel, there are, amongst other things,
generated declarations and calls to the `field_out_w0`, `field1_in_w0`
and `field2_in_w0` proxies

```fortran
      TYPE(field_proxy_type) field_out_w0_proxy, field1_in_w0_proxy, ...
      ...
      !
      ! Initialise field and/or operator proxies
      !
      field_out_w0_proxy = field_out_w0%get_proxy()
      field1_in_w0_proxy = field1_in_w0%get_proxy()
      field2_in_w0_proxy = field2_in_w0%get_proxy()
```

and generated declarations and dereferencing assignments to access
the proxy data

```fortran
      ...
      INTEGER(KIND=i_def) undf_aspc1_field_out_w0, undf_aspc1_field1_in_w0, ...
      ...
      !
      ! Initialise number of DoFs for aspc1_field_out_w0
      !
      undf_aspc1_field_out_w0 = field_out_w0_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for aspc1_field1_in_w0
      !
      undf_aspc1_field1_in_w0 = field1_in_w0_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for aspc1_field2_in_w0
      !
      undf_aspc1_field2_in_w0 = field2_in_w0_proxy%vspace%get_undf()
```

---
**NOTE**

Since built-ins work on any space, PSyclone cannot assume that a field
passed to one built-in is on the same space as that passed to a
different built-in. Some of this information could be deduced by static
analysis but we have yet to implement that.

---

Unlike the PSy-layer kernel code calls, the generated code for the
built-in calls is not provided by the user. Instead it is specified by
PSyclone to be a direct representation of the mathematical operation
that a built-in performs (PSyclone could also generate calls to e.g.
an optimised linear-algebra library where appropriate).
The generated code for the specific mathematical operation here
(`field_out_w0 = field1_in_w0 + field2_in_w0`) looks something like:

```fortran
      !
      ! Call our kernels
      !
      ...
      DO df=1,undf_aspc1_field_out_w0
        field_out_w0_proxy%data(df) = field1_in_w0_proxy%data(df) + &
                                      field2_in_w0_proxy%data(df)
      END DO
```

The above built-in code call also illustrates how the `operates_on = DOF`
metadata in the definition of `X_plus_Y_code` translates to a
**loop over DoFs** (`df` loop counter) in the PSy layer.

*Note:*  `aspc1` in the `undf_*` name comes from the generic function
space metadata `ANY_SPACE_1` used to define built-ins in PSyclone (see
the [quick intro to built-ins below](#quick-intro-to-built-ins) below).

### Step 4: Build and run the code

We will now run `make` to create the executable `builtins` using the
provided [`builtins_driver.f90`](builtins_driver.f90) and the LFRic
infrastructure [code support](
../README.md#lfric-code-support). If the build is successful we can
run the executable

```shell
./builtins
```

The program prints out several log messages about setting up the model
and calling the algorithm subroutine `builtins_alg`. As outlined
[above](#algorithm-structure), the algorithm checks the minimum
and maximum values of all fields after calling the built-ins to update
them. The correct values for the output fields depend on the values you
set the fields to (for instance, `field_w0_out` should return the
sum of the respective input fields on the `W0` function space).

*Note:* The generated source and the compiled objects and libraries
can be removed by running `make clean`.

## Quick intro to built-ins

Similar to the user-defined [LFRic kernels](
../1_simple_kernels/LFRic_kernel_structure.md),
the built-in kernels also have metadata but their metadata are defined in
the PSyclone [LFRic (Dynamo 0.3) API](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html).

Below is an example of metadata for the [built-in `X_plus_Y`](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#x-plus-y)
that adds two fields and stores the result in a third:

```fortran
  type, public, extends(kernel_type) :: X_plus_Y
     private
     type(arg_type) :: meta_args(3) = (/                              &
          arg_type(GH_FIELD, GH_REAL, GH_WRITE, ANY_SPACE_1),         &
          arg_type(GH_FIELD, GH_REAL, GH_READ,  ANY_SPACE_1),         &
          arg_type(GH_FIELD, GH_REAL, GH_READ,  ANY_SPACE_1)          &
          /)
     integer :: operates_on = DOF
   contains
     procedure, nopass :: X_plus_Y_code
  end type X_plus_Y
```

The metadata is very similar to a user-defined LFRic kernel metadata
with one major difference: built-ins are called from the [PSy-layer](
../background/LFRic_structure.md#psy-layer) from within loops over
the degrees of freedom (*DoFs*) of the field arguments. Hence, the
`operates_on` metadata for this type of kernel has the `DOF` value. All
fields passed to a built-in call **must be on the same function space**.

Built-ins need to work with fields on any function space, hence using the
`ANY_SPACE_1` identifier for the generic function space (see the
[*Supported Function Spaces* section](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#dynamo0-3-function-space)
for more information).
