# Tutorial 1, Part 2: Update fields on a general function space

In this part of the [first tutorial](../README.md), we will repurpose
the completed `setval_field_w0_kernel_mod.f90` from the
[Part 1](../part1/README.md) to assign a value to a field on any
function space. After that we will create another general-purpose
kernel that adds two fields on any function space and stores the
result in a third field on the same space.

We will then modify the supplied algorithm
[`simple_kernels_alg_mod.x90`](simple_kernels_alg_mod.x90) to call
these kernels.

The working directory for this part of the tutorial is
`<PSYCLONEHOME>/tutorial/practicals/LFRic/building_code/1_simple_kernels/part2`
where `<PSYCLONEHOME>` is the full path to the local PSyclone repository.

## Task 1: Repurpose the `setval_field_w0_kernel_mod.f90` kernel

Navigate to the working directory for this part of the tutorial and copy
the completed kernel [`setval_field_w0_kernel_mod.f90`] source from the
[Part 1](../part1) directory as

```shell
cp ../part1/setval_field_w0_kernel_mod.f90 .
```

and then rename the kernel to `setval_field_any_kernel_mod.f90` as

```shell
mv setval_field_w0_kernel_mod.f90 setval_field_any_kernel_mod.f90
```

Open the renamed kernel file in an editor and modify the `arg_type`
field [metadata](../LFRic_kernel_structure.md#metadata) to accept a
field on any function space as

```shell
    type(arg_type), dimension(2) :: meta_args = (/ &
         arg_type(GH_FIELD, GH_INC, ANY_SPACE_1),  &
         arg_type(GH_REAL,  GH_READ)               &
         /)
```

*Note* that the access mode for the updated field in this kernel,
`GH_INC`, is the same as the access for an updated field on a
continuous function space `W0` (see [here](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes)
for more information of valid access modes depending on the function
space that the argument is defined on). This is according to the rule that
the general function spaces need to be treated as continuous (the "worst
case scenario").

Unlike the specific function space identifier `W0` that is located
in the `fs_continuity_mod` LFRic infrastructure module, the metadata
identifiers for the general-purpose function spaces are located in
the `argument_mod` LFRic infrastructure module. We will need to remove
the `use fs_continuity_mod, only: W0` statement and add the generic
`ANY_SPACE_1` metadata identifier to the `use argument_mod, only: ...`
statement.

After that we need to change the kernel code unit names accordingly,
e.g. `setval_field_w0_` becomes `setval_field_any_`, and then we
will then run the kernel stub generator on the renamed kernel

```shell
genkernelstub setval_field_any_kernel_mod.f90
```

to check and update the kernel argument names and order if required.

The order of the kernel arguments should be the same, as the only
major change is the function space name. As for the argument names,
they should change extensions from `_w0` to `_aspc1`. Here `aspc1` in
the name of field data and other kernel arguments comes from the
generic `ANY_SPACE_1` metadata identifier. You can use the editor
to change the extensions accordingly.

You should now have the completed `setval_field_any_kernel_mod.f90`
kernel. To check that everything is correct, look into the completed
kernel in the [Solutions of Part 2 directory](../solutions/part2).

## Task 2: Create the `add_fields_any_kernel_mod.f90` kernel

We will now use the completed `setval_field_any_kernel_mod.f90` as a
template for a general-purpose kernel that adds two fields on any
function space and stored the result in a third field on the same
space.

Copy the created `setval_field_any_kernel_mod.f90` kernel into the new
`add_fields_any_kernel_mod.f90` and open the new kernel in an editor.

We will first change the new kernel code unit names from
`setval_field_any_` to `add_fields_any_`. After that we need to
modify the `arg_type` [metadata](
../LFRic_kernel_structure.md#metadata) to make sure that we
have one updated field and two read-only fields as the kernel
arguments:

```fortran
    type(arg_type), dimension(3) :: meta_args = (/ &
         arg_type(GH_FIELD, GH_INC,  ANY_SPACE_1), &
         arg_type(GH_FIELD, GH_READ, ANY_SPACE_1), &
         arg_type(GH_FIELD, GH_READ, ANY_SPACE_1)  &
         /)
```

*Note* that all fields use the same general-purpose function space
identifier.

The argument list has changed significantly so we need to run the
kernel stub generator to update the argument list and declarations.

Replace the kernel subroutine header and the declarations with the
ones produced by the kernel stub generator. The generated header is
something like

```fortran
    SUBROUTINE add_fields_any_code(nlayers, field_1_aspc1_field_1, &
                        field_2_aspc1_field_1, field_3_aspc1_field_1, &
                        ndf_aspc1_field_1, undf_aspc1_field_1, &
                        map_aspc1_field_1)
```

The `_aspc1_field_1` appendix comes from the `ANY_SPACE_1` metadata
identifier as described above and the "write-field" name, `field_1`.
This additional field identifier is added because the `ANY_SPACE_1`
function space of an updated field in one kernel not having to be the
same as in another kernel.

You will now need to update the implementation of the kernel loop so
that it updates the elements of `field_1` to the sum of elements of
the read-only fields `field_2` and `field_2` to something like

```fortran
do k = 0, nlayers-1
  do df = 1, ndf_w0
    field_1_aspc1_field_1( map_aspc1_field_1(df) + k ) = &
          field_2_aspc1_field_1( map_aspc1_field_1(df) + k ) + &
          field_3_aspc1_field_1( map_aspc1_field_1(df) + k )
  end do
end do
```

The completed kernel can also be found in the
[Solutions of Part 2 directory](../solutions/part2) (note shorter
argument names after removing of the `_field_1` appendix).

## Task 3: Call kernels from the supplied algorithm

The structure and the role of the algorithm
[`simple_kernels_alg_mod.x90`](simple_kernels_alg_mod.x90) is very similar
to the [Part 1 algorithm](../part1/simple_kernels_alg_mod.x90) described
in the [*Algorithm structure* section](../README.md#algorithm-structure)
of this tutorial. In this task we will focus on using PSyclone `invoke`s
to call the created kernels.

Open the supplied algorithm source, `simple_kernels_alg_mod.x90`, in an
editor and look for the commented-out code that marks the place to complete
the `invoke` calls, `! TO COMPLETE: Set each ...`.

We will now create the `invoke` call to the above general-purpose kernels to
1. Initialise the output field `field_w0_out` to `0`,
2. Initialise the input fields `field1_w0_in` and `field2_w0_in` to
   different constant scalar values and
3. Add `field1_w0_in` and `field2_w0_in` and return the output
  `field_w0_out`.

Note that for the `setval_field_any_kernel_type` calls we can pass
literal scalar values as the kernel arguments, e.g.

```fortran
    call invoke( setval_field_any_kernel_type(field_w0_out,  0.0_r_def), &
                 setval_field_any_kernel_type(field1_w0_in, -2.0_r_def), &
                 ...
```

The completed `invoke` call for the `W0` fields would be something like

```fortran
    call invoke( setval_field_any_kernel_type(field_w0_out, 0.0_r_def), &
                 setval_field_any_kernel_type(field1_w0_in, ...), &
                 setval_field_any_kernel_type(field2_w0_in, ...), &
                 add_fields_any_kernel_type(field_w0_out, field1_w0_in, field2_w0_in) )
```

We will then create another `invoke` that repeats the calls for the `W3`
fields, but with the different values of scalar literals for setting the
`field1_w3_in` and `field2_w3_in`. Unlike in the [Part 1](../part1) of
this tutorial, there is no need for the different versions of kernels
depending on the function space of the kernel arguments.

The completed algorithm can be found in the [Solutions of Part 2 directory](
../solutions/part2).

## Task 4: Build and run the code

The PSyclone code generation is illustrated in the [Part 1](
../part1/README.md) of this tutorial, so in this tutorial we go directly
to building and running the code. 

We will copy the `simple_kernels_driver.f90` file to this working directory

```shell
cp ../simple_kernels_driver.f90 .
```

and then run `make` to create the executable `simple_kernels_part2` using
the provided LFRic infrastructure [code support](
../../README.md#lfric-code-support] (note that the PSyclone code generation
is integrated into the full build process). If the build is successful we can
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
