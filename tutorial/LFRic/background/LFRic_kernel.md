# LFRic kernel

[*Kernel layer in LFRic in PSyclone documentation*](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#kernel)

## What kernels do

LFRic kernels perform mathematical operations on [a subset of data
points](
https://psyclone.readthedocs.io/en/stable/kernel_layer.html#kernel-layer)
in LFRic data objects (fields, operators and scalars) passed
from the [algorithm](LFRic_algorithm.md) through the
[PSy](LFRic_PSy.md) layer.

Scalar values are passed to a kernel from the
[algorithm layer](LFRic_algorithm.md) as they are. In case of fields
and operators, however, LFRic kernels operate on a subset of degrees
of freedom (DoFs) of these objects. Such a subset occupies a portion
of the computational domain (e.g. an entire mesh or a partition of a
mesh that the field's DoFs are placed on). The subsections below
illustrate the LFRic kernel structure and how the relevant information
is passed to a kernel.

## Kernel structure

In this section we use the
[`../example1/setval_field_w0_kernel_mod.f90`](
../example1/setval_field_w0_kernel_mod.f90)
kernel stub to illustrate the structure of LFRic kernels (the
[completed example](../example1/solutions/part1/setval_field_w0_kernel_mod.f90)
is provided in the relevant [solutions](../example1/solutions/part1)).

```fortran
module setval_field_w0_kernel_mod

  use argument_mod,      only: arg_type,          &
                               GH_FIELD, GH_REAL, &
                               GH_INC, GH_READ, CELLS
  use fs_continuity_mod, only: W0
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! Public types
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: setval_field_w0_kernel_type
    private
    type(arg_type), dimension(2) :: meta_args = (/ &
         arg_type(GH_FIELD, GH_INC, W0),           &
         arg_type(GH_REAL,  GH_READ)               &
         /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: code => setval_field_w0_code
  end type setval_field_w0_kernel_type

  !-----------------------------------------------------------------------------
  ! Contained functions/subroutines
  !-----------------------------------------------------------------------------
  public setval_field_w0_code

  contains

  subroutine setval_field_w0_code()

  end subroutine setval_field_w0_code

end module setval_field_w0_kernel_mod
```

As can be seen from the code above, the *LFRic kernels are objects, defined
as derived types* that inherit from the abstract base type `kernel_type`

```fortran
type, public, extends(kernel_type) :: setval_field_w0_kernel_type
```

and this base type must be accessible to a kernel through a `use` statement

```fortran
use kernel_mod,        only: kernel_type
```

Looking at the `setval_field_w0_kernel_type` above, there are two main
parts of an LFRic kernel:
* Metadata that tells PSyclone how to operate on a kernel;
* Subroutine with the argument list, declarations and executable code.

All of this is stored in a module. The naming convention for the module,
the kernel type and the executable subroutine is as follows:

* Module name: `<base_name>_kernel_mod`;
* Kernel type name: `<base_name>_kernel_type`;
* Subroutine name: `<base_name>_code`.

Here the `<base_name>` is `setval_field_w0`.

### Kernel metadata

The PSyclone metadata are stored as the *private* data within the definition
of an individual kernel type. They come in two main forms: derived types
(e.g. `arg_type` in the above example) and `integer` arguments (single-valued
such as `iterates_over` above or arrays). Here we briefly explain the contents
of `arg_type` and `iterates_over` metadata. For more information please refer
to the [*Metadata* section](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#metadata)
of the LFRic (Dynamo 0.3) API [user documentation.](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html)

The `arg_type` in this example describes the two (hence `dimension(2)`)
arguments that this kernel operates on:
* A field argument (`GH_FIELD`) with the degrees of freedom (DoFs) on `W0`
  function space that is being updated in this kernel (`GH_INC` access for
  updating fields on continuous function spaces);
* A `real`-valued scalar (`GH_REAL`) whose value is read (`GH_READ`) to
  update the field with.

The `GH_` prefix comes from "GungHo", the name of the dynamical core used in
the LFRic model. Please refer to the documentation for more information on
[supported accesses](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes)
and [function spaces](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#supported-function-spaces)
for the LFRic data objects.

The `iterates_over` metadata specifies how LFRic kernels are called
from the [PSy layer](LFRic_PSy.md). A user-defined kernel in LFRic
is called from a PSy-layer loop over each cell in the horizontal domain,
hence the metadata identifier for this way of looping - `CELLS`. This means
that a kernel operates on a one-cell-wide vertical column of cells.

---
**NOTE**

* LFRic field data are currently `real`-valued, however there is ongoing
  work to allow the field data of other intrinsic types, such as `integer`.
  This will be reflected in the metadata, with `GH_REAL` and `GH_INTEGER`
  marking the data type and `GH_SCALAR` denoting the scalar arguments. The
  above `arg_type` notation will change to

  ```fortran
      type(arg_type), dimension(2) :: meta_args = (/ &
           arg_type(GH_FIELD,  GH_REAL, GH_INC, W0), &
           arg_type(GH_SCALAR, GH_REAL, GH_READ)     &
           /)
  ```

* The kernel metadata for the iteration spaces are also changing to be
  clearer about the subset of domain the kernel operates on rather than
  the PSy-layer looping. In the next PSyclone release `iterates_over = CELLS`
  will become `operates_on = CELL_COLUMN`.
---

### `use` statements and encapsulation

Besides inheritance from the abstract `kernel_type`, this kernel requires
LFRic identifiers for the PSyclone metadata and Fortran kind of the argument
data. This information is stored in LFRic infrastructure modules.

* `use argument_mod,      only: arg_type, ...` looks for the LFRic
  identifiers of PSyclone metadata for LFRic objects (e.g. fields);
* `use fs_continuity_mod, only: ...` looks for the `integer` identifier of
  the required function space (here `W0`);
* `use constants_mod,     only: ...` looks for the definitions of Fortran
  kind (precision) of data in LFRic objects (here `r_def` for `real` and
  `i_def` for `integer` arguments in LFRic).

The stub example above also illustrates the main "encapsulation rule" in
LFRic: everything is `private` unless it needs to be accessed outside the
object. As a rule, object data are `private` as illustrated in the
`setval_field_w0_kernel_type` definition above. Object procedures can be
`private` (if used only by the parent object) or `public`ly available
(e.g. `subroutine setval_field_w0_code` is called in the generated PSy layer).

### Kernel subroutine

#### Argument list and declarations

Running PSyclone
[kernel stub generator](https://psyclone.readthedocs.io/en/stable/stub_gen.html)
on `setval_field_w0_kernel_mod.f90` produces the argument list and
declarations for the `setval_field_w0_code` enclosed in a module:

```fortran
   MODULE setval_field_w0_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE setval_field_w0_code(nlayers, field_1_w0, rscalar_2, ndf_w0, undf_w0, map_w0)
      USE constants_mod, ONLY: r_def, i_def
      IMPLICIT NONE
      INTEGER(KIND=i_def), intent(in) :: nlayers
      INTEGER(KIND=i_def), intent(in) :: ndf_w0
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0
      INTEGER(KIND=i_def), intent(in) :: undf_w0
      REAL(KIND=r_def), intent(in) :: rscalar_2
      REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: field_1_w0
    END SUBROUTINE setval_field_w0_code
  END MODULE setval_field_w0_mod
```

The generated subroutine body with the argument list and declarations can
be used to complete the kernel stub above (note that the generated
`USE constants_mod` statement is redundant as per LFRic convention it is
already declared at the module level).

`rscalar_2` denotes the read-only scalar argument passed from the
[LFRic algorithm layer](LFRic_algorithm.md) as is. As said
[above](#what-kernels-do), kernels operate on a subset of a field
object and the generated code reflects the required information for a
kernel to read from and update a field:
* `nlayers` is the number of vertical layers in the mesh that the
  field's DoFs are placed on (or the number of cells in a vertical
  column that the kernel operates on);
* `field_1_w0` is the data stored in an LFRic field object;
* `ndf_w0` is the number of all DoFs (owned and annexed) for the field
  argument defined on `W0` function space;
* `undf_w0` is the number of owned DoFs for the field argument defined
  on `W0` function space;
* `map_w0` is the DoF-map for the cell at the base of the column for
  the `W0` function space that this field is defined on (the map
  contains local IDs of DoFs in a partition).

#### Loops

Having defined the kernel metadata, subroutine arguments and
declarations, we can write the executable code to actually update one
cell-column of a field in this example. In LFRic kernels this is done by
an outer `k`-loop over layers that updates each cell in a column and an
inner loop over DoFs that updates the DoFs in a cell.

```fortran
do k = 0, nlayers-1
  do df = 1, ndf_w0
    field_1_w0( map_w0(df) + k ) = rscalar_2
  end do
end do
```
