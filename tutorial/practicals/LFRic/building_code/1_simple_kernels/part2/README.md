# Tutorial 1, Part 2: Update fields on a generic function space

In this part of the [first tutorial](../README.md), we will use the
[stub-generation](https://psyclone.readthedocs.io/en/stable/psyclone_kern.html)
functionality of the PSyclone kernel tool to
create the argument list and declarations for a generic kernel that
assigns a value to a field on any function space. For this we will use
the supplied kernel stub file [`setval_field_any_kernel_mod.f90`](
setval_field_any_kernel_mod.f90) and the provided documentation on the
[LFRic kernel structure](../LFRic_kernel_structure.md). After that we will
create another generic kernel that adds two fields on any function space and
stores the result in a third field on the same generic space.

We will then modify the supplied algorithm
[`simple_kernels_alg_mod.x90`](simple_kernels_alg_mod.x90) to call
these kernels.

The working directory for this part of the tutorial is
```
<PSYCLONEHOME>/tutorial/practicals/LFRic/building_code/1_simple_kernels/part2
```
where `<PSYCLONEHOME>` is the full path to the local PSyclone repository.

## Step 1: Complete the `setval_field_any_kernel_mod.f90` kernel

Navigate to the working directory for this part of the tutorial and open
the supplied kernel stub file [`setval_field_any_kernel_mod.f90`](
setval_field_any_kernel_mod.f90) in an editor. The `arg_type` [metadata](
../LFRic_kernel_structure.md#kernel-metadata) of this kernel stub

```fortran
    type(arg_type), dimension(2) :: meta_args = (/          &
         arg_type(GH_FIELD,  GH_REAL, GH_INC, ANY_SPACE_1), &
         arg_type(GH_SCALAR, GH_REAL, GH_READ)              &
         /)
```

describes two arguments: a `real`-valued field (`GH_FIELD`, `GH_REAL`)
with the degrees of freedom (DoFs) on a generic function space
(`ANY_SPACE_1`) and a `real`-valued scalar (`GH_SCALAR`, `GH_REAL`). The
field is to be updated (`GH_INC`) while the scalar is read only (`GH_READ`).

---
**NOTE**

* The access mode for the updated field in this kernel, `GH_INC`, is
the same as the access for an updated field on a continuous function
space `W0` in [`setval_field_w0_kernel_mod.f90`](
../part1/setval_field_w0_kernel_mod.f90) (see [here](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes)
for more information of valid access modes depending on the function
space that an argument is defined on). The generic function spaces are
treated as continuous (the "worst case scenario") except in the case of
a generic discontinuous function space `ANY_DISCONTINUOUS_SPACE_n`
(see [here](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#supported-function-spaces)
for information on the supported function spaces).

* Unlike the specific function space identifier `W0` that is located
in the LFRic infrastructure module `fs_continuity_mod`, the metadata
identifiers for the generic function spaces are located in
the LFRic infrastructure module `argument_mod`, so the statement
`use argument_mod, only: ...` is modified accordingly.

---

The kernel `setval_field_any_code()` subroutine body is empty and needs
to be populated. As in [Part 1](../part1), we will first create the code
for the argument list and declarations by running the PSyclone kernel
tool:

```shell
psyclone-kern setval_field_any_kernel_mod.f90
```

and then replace the empty kernel subroutine body with the generated
one that contains the argument list and the generated declarations.
Again, the generated `USE constants_mod, ONLY: r_def, i_def` statement
needs to be removed as the externally defined parameters are already
listed in the `use` statements at the kernel module level.

---
**NOTE**

The argument list and declarations are generated in one line per
statement each, so in this case of long argument names they may overrun
the free-form Fortran line-length limit of 132 characters and PSyclone
will complain when building the code (see e.g. [here](
https://psyclone.readthedocs.io/en/stable/line_length.html)). To wrap
long lines just use the `-l output` flag when running the kernel tool, e.g.

```shell
psyclone-kern -l output setval_field_any_kernel_mod.f90
```

---

The order of the kernel arguments should be the same as in 
[`setval_field_w0_kernel_mod.f90`](
../part1/setval_field_w0_kernel_mod.f90), as the only
major change is the function space name. The argument names now have
the extension`_aspc1_field_1` instead of `_w0`. The `_aspc1_field_1`
appendix comes from the `ANY_SPACE_1` metadata identifier as described
above (condensed to `aspc1`) and the "write-field" name, `field_1`.
This additional field identifier is added because a generic `ANY_SPACE_1`
function space of an updated field in one kernel does not have to be
the same as `ANY_SPACE_1` in another kernel.

You will now need to complete the implementation of the kernel so
that it sets elements of the supplied field to the specified scalar
value. This implementation will also be a
[nested `k`-first loop](../LFRic_kernel_structure.md#loops) as in
the completed `setval_field_w0_code` subroutine from [Part 1](../part1),
but the DoF-map and the field names will be different:

```fortran
do k = 0, nlayers-1
    do df = 1, ndf_aspc1_field_1
      field_1_aspc1_field_1( map_aspc1_field_1(df) + k ) = rscalar_2
  end do
end do
```

You should now have the completed `setval_field_any_kernel_mod.f90`
kernel. To check that everything is correct, look into the completed
kernel in the [`solutions` directory](solutions).

## Step 2: Create the `add_fields_any_kernel_mod.f90` kernel

We will now use the completed `setval_field_any_kernel_mod.f90` as a
template to create a generic kernel that adds two fields on any
function space and stores the result in a third field on the same
space.

Copy the created `setval_field_any_kernel_mod.f90` kernel into the new
`add_fields_any_kernel_mod.f90` and open the new kernel in an editor.

We will first change the new kernel code unit names from
`setval_field_any_` to `add_fields_any_`. After that we need to
modify the `arg_type` [metadata](
../LFRic_kernel_structure.md#kernel-metadata) to make sure that
we have one updated field and two read-only fields as the kernel
arguments:

```fortran
    type(arg_type), dimension(3) :: meta_args = (/          &
         arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_1), &
         arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_1), &
         arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_1)  &
         /)
```

**Note** that all fields use the same generic function space
identifier.

The argument list has changed significantly so we need to run the
kernel tool to update the argument list and declarations.

Replace the kernel subroutine header and the declarations with the
ones produced by the kernel tool. The generated header is:

```fortran
    SUBROUTINE add_fields_any_code(nlayers, field_1_aspc1_field_1, &
                        field_2_aspc1_field_1, field_3_aspc1_field_1, &
                        ndf_aspc1_field_1, undf_aspc1_field_1, &
                        map_aspc1_field_1)
```



You will now need to update the implementation of the kernel loop so
that it updates the elements of `field_1` to the sum of elements of
the read-only fields `field_2_aspc1_field_1` and `field_3_aspc1_field_1`
to e.g.

```fortran
do k = 0, nlayers-1
  do df = 1, ndf_aspc1_field_1
    field_1_aspc1_field_1( map_aspc1_field_1(df) + k ) = &
          field_2_aspc1_field_1( map_aspc1_field_1(df) + k ) + &
          field_3_aspc1_field_1( map_aspc1_field_1(df) + k )
  end do
end do
```

The completed kernel can also be found in the
[`solutions` directory](solutions).

## Step 3: Call kernels from the supplied algorithm

The structure and the role of the algorithm
[`simple_kernels_alg_mod.x90`](simple_kernels_alg_mod.x90) is very similar
to the [Part 1 algorithm](../part1/simple_kernels_alg_mod.x90) described
in the [*Algorithm structure* section](../README.md#algorithm-structure)
of this tutorial. In this step we will focus on using PSyclone `invoke`s
to call the created kernels.

Open the supplied algorithm source, `simple_kernels_alg_mod.x90`, in an
editor and look for the comment that marks the place to complete
the `invoke` calls, `! TO COMPLETE: Set each ...`

We will now create the `invoke` call to the above generic kernels to

1. Initialise the output field `field_w0_out` to `0`;

2. Initialise the input fields `field1_w0_in` and `field2_w0_in` to
   different constant `real` scalar values. Here we use `-2.0` and
   `6.0`, respectively, but you can use other values if you wish to;

3. Add `field1_w0_in` and `field2_w0_in` and return the output
  `field_w0_out`.

Note that for the `setval_field_any_kernel_type` calls we can pass
literal scalar values as the kernel arguments, e.g.

```fortran
    call invoke( setval_field_any_kernel_type(field_w0_out,  0.0_r_def), &
                 setval_field_any_kernel_type(field1_w0_in, -2.0_r_def), &
                 ...
```

It is mandatory in LFRic to specify the Fortran `kind` of a literal value.
In this case it is `r_def` for the `real`-valued literals that are used to
initialise the fields as the field data are of kind `r_def`.

The completed `invoke` call for the `W0` fields would be something like

```fortran
    call invoke( setval_field_any_kernel_type(field_w0_out, 0.0_r_def), &
                 setval_field_any_kernel_type(field1_w0_in, ...), &
                 setval_field_any_kernel_type(field2_w0_in, ...), &
                 add_fields_any_kernel_type(field_w0_out, field1_w0_in, field2_w0_in) )
```

We will then create another `invoke` that repeats the calls for the `W3`
fields, but with the different values of `real` scalar literals for
initialising the fields `field1_w3_in` and `field2_w3_in`. Here we use `55.5`
and `44.5`, respectively, but you can use other values if you wish to.

**Note** that, unlike in the [Part 1](../part1) of this tutorial, there is no
need for the different versions of kernels depending on the function space of the
kernel arguments.

The completed algorithm can be found in the
[`solutions` directory](solutions).

## Step 4: Build and run the code

We will now run `make` to create the executable `simple_kernels_part2`
using the provided [`simple_kernels_driver.f90`](simple_kernels_driver.f90)
and the LFRic infrastructure [code support](
../README.md#lfric-code-support). If the build is successful we can
run the executable

```shell
./simple_kernels_part2
```

The program prints out several log messages about setting up the model
and calling the algorithm subroutine `simple_kernels_alg`. As outlined
in the [Part 1](../part1/README.md), the algorithm checks the minimum
and maximum values of all fields after calling the kernels that update
them. The correct values for the output fields `field_w0_out` and
`field_w3_out` should be equal to the sum of the respective input field
values.

*Note:* The generated source and the compiled objects and libraries
can be removed by running `make clean`.
