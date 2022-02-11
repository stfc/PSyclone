# LFRic kernel structure

In this part of the tutorial we learn about the structure of an LFRic
kernel and how to create or modify a kernel.

---
**NOTE**

For the high-level overview of the LFRic kernel layer role and
relevant links see the [Kernel layer](
../background/LFRic_structure.md#kernel-layer) section in the
[LFRic code structure document](../background/LFRic_structure.md).

---

We use the supplied [`setval_field_w0_kernel_mod.f90`](
part1/setval_field_w0_kernel_mod.f90) kernel stub to illustrate the structure
of an LFRic kernel:

```fortran
module setval_field_w0_kernel_mod

  use argument_mod,      only: arg_type,          &
                               GH_FIELD, GH_REAL, &
                               GH_SCALAR,         &
                               GH_INC, GH_READ,   &
                               CELL_COLUMN
  use fs_continuity_mod, only: W0
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! The type declaration for the kernel. Contains the metadata needed by
  ! the PSy layer.
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: setval_field_w0_kernel_type
    private
    type(arg_type), dimension(2) :: meta_args = (/ &
         arg_type(GH_FIELD,  GH_REAL, GH_INC, W0), &
         arg_type(GH_SCALAR, GH_REAL, GH_READ)     &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: code => setval_field_w0_code
  end type setval_field_w0_kernel_type

  public setval_field_w0_code

  contains

  subroutine setval_field_w0_code()

  end subroutine setval_field_w0_code

end module setval_field_w0_kernel_mod
```

As can be seen from the code above, the *LFRic kernels are classes, defined
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
* Metadata that tells PSyclone how to generate code to call a kernel;
* Subroutine with the argument list, declarations and executable code.

All of this is stored in a module. The naming convention for the module,
the kernel type and the executable subroutine is as follows:

* Module name: `<base_name>_kernel_mod`;
* Kernel type name: `<base_name>_kernel_type`;
* Subroutine name: `<base_name>_code`.

Here the `<base_name>` is `setval_field_w0`.

## Kernel metadata

The PSyclone metadata is stored as the *private* data within the definition
of an individual kernel type. They come in two main forms: derived types
(e.g. `arg_type` in the above example) and `integer` arguments (single-valued
such as `operates_on` above or arrays). Here we briefly explain the contents
of the `arg_type` and `operates_on` metadata. For more information please refer
to the [*Metadata* section](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#metadata)
of the LFRic (Dynamo 0.3) API [user documentation](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html).

The `arg_type` in this example describes the two (hence `dimension(2)`)
arguments that this kernel operates on:
* A `real`-valued field argument (`GH_FIELD`, `GH_REAL`) with the degrees of
  freedom (DoFs) on the `W0` function space that is being updated in this kernel
  (`GH_INC` access for updating fields on continuous function spaces);
* A `real`-valued scalar (`GH_SCALAR`, `GH_REAL`) that is read
  (`GH_READ`) and used to update the field.

The `GH_` prefix comes from "GungHo", the name of the dynamical core used in
the LFRic model. Please refer to the documentation for more information on
[supported accesses](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes)
and [function spaces](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#supported-function-spaces)
for the LFRic data objects.

The `operates_on` metadata specifies how LFRic kernels are called
from the [LFRic PSy layer](../background/LFRic_structure.md#psy-layer).
A user-defined kernel in LFRic is called from a PSy-layer loop over each
cell in the horizontal domain. This means that a kernel operates on a single,
vertical column of cells, hence the metadata identifier for this way
of looping, `CELL_COLUMN`.

## `use` statements and encapsulation

Besides inheritance from the abstract `kernel_type`, this kernel requires
LFRic identifiers for the PSyclone metadata and Fortran kind of the argument
data. This information is stored in LFRic infrastructure modules.

* `use argument_mod,      only: arg_type, ...` looks for the LFRic
  identifiers of PSyclone metadata for LFRic objects (e.g. fields);
* `use fs_continuity_mod, only: ...` looks for the enumerator identifier
  (implemented as `integer`) of the required function space (here `W0`);
* `use constants_mod,     only: ...` looks for the definitions of Fortran
  kind (precision) of data in LFRic objects (here `r_def` for `real` and
  `i_def` for `integer` arguments in LFRic).

The stub example above also illustrates the main "encapsulation rule" in
LFRic: everything is `private` unless it needs to be accessed outside the
object. As a rule, object data are `private` as illustrated in the
`setval_field_w0_kernel_type` definition above. Object procedures can be
`private` (if used only by the object) or `public`ly available (e.g.
`subroutine setval_field_w0_code` is called in the generated PSy layer).

## Kernel subroutine

### Argument list and declarations

Running the PSyclone [kernel tool](https://psyclone.readthedocs.io/en/stable/psyclone_kern.html) 
on `setval_field_w0_kernel_mod.f90` produces the argument list and
declarations for the `setval_field_w0_code`, enclosed in a module:

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

The generated subroutine body with the argument list and declarations
can be used to complete the kernel stub above (note that the generated
`USE constants_mod` statement is already declared at the module level
in the LFRic kernel source so it does not need to be copied from the
generated stub code).

`rscalar_2` denotes the read-only scalar argument passed from the
[LFRic algorithm layer](../background/LFRic_structure.md#algorithm-layer)
as is. The field argument, however, is more complicated - as said in the
overview of the [LFRic kernel layer](
../background/LFRic_structure.md#kernel-layer), kernels operate on a subset
of a field object and the generated code reflects the required information
for a kernel to read from and update a field:

* `nlayers` is the number of vertical layers in the mesh that the
  field's DoFs are placed on (or the number of cells in a vertical
  column that the kernel operates on);
* `field_1_w0` is the data stored in the LFRic field object passed as an
  argument to this kernel;
* `ndf_w0` is the number of all DoFs (owned and annexed) for the field
  argument defined on the `W0` function space;
* `undf_w0` is the number of owned DoFs for the field argument defined
  on the `W0` function space;
* `map_w0` is the DoF-map for the cell at the base of the column for
  the `W0` function space that this field is defined on. Since the mesh
  used in LFRic is unstructured in the horizontal, the location of the
  DoFs (in the data array) for a given cell must be looked up. The map
  contains local IDs of DoFs in a partition.

---
**NOTE**

The order of kernel arguments roughly corresponds to the order of
[kernel metadata descriptors](#kernel-metadata). The convention when
ordering arguments is to put arguments that are written to first.
The full PSyclone LFRic API specification of the argument ordering for
the [general LFRic user-defined kernels](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#rules-for-general-purpose-kernels)
is quite complex, hence making the [PSyclone kernel tool](
https://psyclone.readthedocs.io/en/stable/psyclone_kern.html) very useful
for development.

LFRic kernels can update more than one field and/or operator arguments.
However, scalar arguments in user-defined LFRic kernels must be
[read-only](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes).

---

### Loops

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

---
**NOTE**

Looping over `k` in the kernel allows computation to be done cell by cell
and as such is a more natural way of writing the numerics of finite-element
method for scientists. From the computational point of view it is an
optimisation which assumes that the field data is `k`-first. In the future
this looping may be moved up to the [PSy layer](
../background/LFRic_structure.md#psy-layer) PSy layer to
better handle e.g. `i`-first fields.

---
