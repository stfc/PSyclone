# Tutorial 1, Part 1: Update fields on a specific function space

In this part of the [first tutorial](../README.md), we will use the
[stub-generation](
https://psyclone.readthedocs.io/en/stable/psyclone_kern.html) functionality
of the PSyclone kernel tool to create the argument list and declarations
for two kernels, one that assigns a value to a field on a continuous
`W0` function space and another on a discontinuous `W3` function space.
For this we will use the supplied kernel stub file
[`setval_field_w0_kernel_mod.f90`](setval_field_w0_kernel_mod.f90) and
the provided documentation on the
[LFRic kernel structure](../LFRic_kernel_structure.md).

We will then modify the supplied algorithm
[`simple_kernels_alg_mod.x90`](simple_kernels_alg_mod.x90) to call
these kernels.

The working directory for this part of the tutorial is
```
<PSYCLONEHOME>/tutorial/practicals/LFRic/building_code/1_simple_kernels/part1
```
where `<PSYCLONEHOME>` is the full path to the local PSyclone repository.

## Step 1: Complete the `setval_field_w0_kernel_mod.f90` kernel

Navigate to the working directory for this part of the tutorial and open
the supplied kernel stub file [`setval_field_w0_kernel_mod.f90`](
setval_field_w0_kernel_mod.f90) in an editor. The `arg_type` [metadata](
../LFRic_kernel_structure.md#kernel-metadata) of this kernel stub

```fortran
    type(arg_type), dimension(2) :: meta_args = (/ &
         arg_type(GH_FIELD,  GH_REAL, GH_INC, W0), &
         arg_type(GH_SCALAR, GH_REAL, GH_READ)     &
         /)
```

describes two arguments: a `real`-valued field (`GH_FIELD`, `GH_REAL`) with
the degrees of freedom (DoFs) on the `W0` function space and a `real`-valued
scalar (`GH_SCALAR`, `GH_REAL`). The field is to be updated (`GH_INC`)
while the scalar is read only (`GH_READ`).

The kernel `setval_field_w0_code()` subroutine body is empty and needs
to be populated. We will first create the code for the argument list and
declarations by using the PSyclone kernel tool to generate a stub:

```shell
psyclone-kern setval_field_w0_kernel_mod.f90
```

Look into the [*Argument list and declarations*](
../LFRic_kernel_structure.md#argument-list-and-declarations) section
for the output of the command for this stub and the meaning of the
generated kernel arguments.

Replace the empty kernel subroutine body with the generated one that
contains the argument list and the generated declarations.
The generated `USE constants_mod, ONLY: r_def, i_def` statement can
be removed as the externally defined parameters are already listed
in the `use` statements at the kernel module level.

You will now need to complete the implementation of the kernel so
that it sets elements of the supplied field to the specified scalar
value. For this you can refer to the [*Loops* section](
../LFRic_kernel_structure.md#loops) of the kernel documentation in
this tutorial for guidance. The section uses this kernel as an
example so you can simply copy the code:

```fortran
do k = 0, nlayers-1
  do df = 1, ndf_w0
    field_1_w0( map_w0(df) + k ) = rscalar_2
  end do
end do
```

into the subroutine body after the declarations. This is a typical
loop structure for LFRic kernels, for more details see the
[*Loops* section](../LFRic_kernel_structure.md#loops).

You should now have the completed `setval_field_w0_kernel_mod.f90`
kernel. To check that everything is correct, look into the completed
kernel in the [`solutions` directory](solutions).

## Step 2: Create the `setval_field_w3_kernel_mod.f90` kernel

We will now use the completed `setval_field_w0_kernel_mod.f90` as a
template to create a kernel that assigns a value to a field on the
`W3` function space. Copy the existing kernel into the new
`setval_field_w3_kernel_mod.f90` kernel and change the function
space in the kernel metadata, argument names and code unit names
accordingly (e.g. `map_w0` becomes `map_w3`, `setval_field_w0_`
becomes `setval_field_w3_`).

The new kernel updates a field on a discontinuous function space
`W3` so you will need to correct the access metadata for the updated
field from `GH_INC` to `GH_READWRITE` (see [here](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes)
for more information of valid access modes depending on the function
space that a field argument is defined on).

Now run the PSyclone kernel tool on the new kernel to check that the
generated argument list and all declarations are correct:

```shell
psyclone-kern setval_field_w3_kernel_mod.f90
```

and update the kernel if required. The completed kernel can also be found
in the [`solutions` directory](solutions).

## Step 3: Call kernels from the supplied algorithm

The structure and the role of the algorithm
[`simple_kernels_alg_mod.x90`](simple_kernels_alg_mod.x90) is described in
the [*Algorithm structure* section](../README.md#algorithm-structure) of
this tutorial. In this step we will focus on using PSyclone `invoke`s to
call the created kernels.

Open the supplied algorithm source, `simple_kernels_alg_mod.x90`, in an
editor and look for the comment that marks the place to complete
the `invoke` calls, `! TO COMPLETE: Set each field ...`

We will now create the `invoke` calls to the two created kernels. The
LFRic algorithm calls *kernel objects* that are defined as derived types,
as explained in the [*LFRic kernel structure*](../LFRic_kernel_structure.md)
section. For instance, the `invoke` call to the first kernel here would be

```fortran
    call invoke( setval_field_w0_kernel_type(field_w0, scalar_w0) )
```

The above call illustrates how the LFRic [algorithm layer](
../../background/LFRic_structure.md#algorithm-layer) operates on
field objects (`field_w0` without knowing anything about the data in
those objects.

Each kernel can be called by a separate `invoke`, however **it is recommended
to group as many kernel calls within a single `invoke` as possible** when
writing algorithm code to enable PSyclone to be effective in performing
optimisations. This is what we will do in this example, so the completed
`invoke` call should look like

```fortran
    call invoke( setval_field_w0_kernel_type(field_w0, scalar_w0), &
                 setval_field_w3_kernel_type(field_w3, scalar_w3) )
```

The completed algorithm can be found in the
[`solutions` directory](solutions).

## Step 4: Use PSyclone to generate algorithm and PSy-layer source

We will first check that the kernels and the algorithm are correctly created
and updated by running `make transform` to process the algorithm source with
PSyclone. If the code is correct, this will result in the generated algorithm
source file `simple_kernels_alg_mod.f90` and the generated PSy-layer source
file `simple_kernels_alg_mod_psy.f90`.

Looking at the generated algorithm file `simple_kernels_alg_mod.f90` we can
see that the original `invoke` call to the two kernel types has become a call
to a single subroutine with all field arguments passed to the kernel calls:

```fortran
    CALL invoke_0(field_w0, scalar_w0, field_w3, scalar_w3)
```

The name of the generated `invoke` subroutine in this case is just the
`invoke` keyword with the appended numerical index. If there was a separate
`invoke` call for each kernel, the subroutine name would also contain the
name of the called kernel (see [PSyclone API](
https://psyclone.readthedocs.io/en/stable/algorithm_layer.html#api)
documentation for more information).

The generated `invoke_0` subroutine code is located in the generated PSy-layer
file, `simple_kernels_alg_mod_psy.f90`.

There are generated declarations and calls to get the `field_w0` and
`field_w3` proxies as outlined in the [*Algorithm layer* section](
../../background/LFRic_structure.md#algorithm-layer):

```fortran
      TYPE(field_proxy_type) field_w0_proxy, field_w3_proxy
      ...
      !
      ! Initialise field and/or operator proxies
      !
      field_w0_proxy = field_w0%get_proxy()
      field_w3_proxy = field_w3%get_proxy()
```

as well as generated declarations and dereferencing assignments to access
the proxy data required for the full kernel call argument list (see the
[*Kernel subroutine* section](
../LFRic_kernel_structure.md#kernel-subroutine)):

```fortran
      INTEGER(KIND=i_def) nlayers
      ...
      INTEGER(KIND=i_def), pointer :: map_w0(:,:) => null(), map_w3(:,:) => null()
      INTEGER(KIND=i_def) ndf_w0, undf_w0, ndf_w3, undf_w3
      ...
      !
      ! Initialise number of layers
      !
      nlayers = field_w0_proxy%vspace%get_nlayers()
      !
      ! Look-up dofmaps for each function space
      !
      map_w0 => field_w0_proxy%vspace%get_whole_dofmap()
      map_w3 => field_w3_proxy%vspace%get_whole_dofmap()
      !
      ! Initialise number of DoFs for w0
      !
      ndf_w0 = field_w0_proxy%vspace%get_ndf()
      undf_w0 = field_w0_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for w3
      !
      ndf_w3 = field_w3_proxy%vspace%get_ndf()
      undf_w3 = field_w3_proxy%vspace%get_undf()
```

Finally, there are generated calls to the two kernel subroutines,
`setval_field_w0_code` and `setval_field_w0_code`, with the complete
argument lists for each kernel:

```fortran
      !
      ! Call our kernels
      !
      DO cell=1,field_w0_proxy%vspace%get_ncell()
        !
        CALL setval_field_w0_code(nlayers, field_w0_proxy%data, &
             scalar_w0, ndf_w0, undf_w0, map_w0(:,cell))
      END DO
      DO cell=1,field_w3_proxy%vspace%get_ncell()
        !
        CALL setval_field_w3_code(nlayers, field_w3_proxy%data, &
             scalar_w3, ndf_w3, undf_w3, map_w3(:,cell))
      END DO
```

with the same argument order as generated by using `psyclone-kern`.

The above code also illustrates how the `operates_on = CELL_COLUMN`
kernel metadata translates to **loops over cells** (`cell` loop counter)
in the PSy layer.

---
**NOTE**

LFRic coding standards mandate lower-case for all source including
Fortran keywords and this holds for all code located in the
[LFRic repository](../../background/LFRic_intro.md). The PSyclone-generated
Fortran code capitalises keywords for better readability (not kept in
the LFRic repository).

---

## Step 5: Build and run the code

We will now run `make` to create the executable `simple_kernels_part1`
using the provided [`simple_kernels_driver.f90`](simple_kernels_driver.f90)
and the LFRic infrastructure [code support](
../README.md#lfric-code-support). If the build is successful we can
run the executable

```shell
./simple_kernels_part1
```

The program prints out several log messages about setting up the model
and calling the algorithm subroutine `simple_kernels_alg`. As outlined in the
[*Algorithm structure* section](../README.md#algorithm-structure), the
algorithm checks the minimum and maximum values of the fields after
calling the kernels that update them. The correct values for the `field_w0`
and the `field_w3` should be the values of `scalar_w0` and `scalar_w3`
set in the subroutine `simple_kernels_alg`.

*Note:* The generated source and the compiled objects and libraries
can be removed by running `make clean`.
