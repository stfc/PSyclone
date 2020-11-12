# Example 2: Use PSyclone built-ins

The [example of simple kernels](../1_simple_kernels) in this tutorial
shows how to build LFRic kernels and use them for simple mathematical
operations, such as setting field to a value and adding fields.

LFRic (Dynamo 0.3) API provides
[built-ins support](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins)
for such mathematical operations. This example replaces calls to
user-defined [LFRic kernels](../background/LFRic_kernel.md) with PSyclone
built-ins in the [algorithm layer](../background/LFRic_algorithm.md).

The provided `builtins_alg_mod.x90` defines output and two input
fields on function spaces `W0` and `W3`, respectively. It outlines
the creation of fields on the `W0` function space and printing out
their `min` and `max` values. The tasks in this example are:
* Create and print values of output and input fields on `W3` space
  using `W0` code as a template;
* Use built-ins to perform the following mathematical operations:
  1. Initialise output fields (`field_out_w0` and `field_out_w3`) to `0`,
  2. Initialise `field1_in_w0` to `1` and `field2_in_w0` to `2`,
  3. Calculate `field_out_w0 = field1_out_w0 + field2_out_w0`,
  4. Initialise `field1_in_w3` to `-1`,
  5. Calculate `field2_in_w3 = 2*field1_in_w3`,
  6. Calculate `field_out_w3 = field1_in_w3 - 0.5*field2_in_w3`.

The list and more information on the appropriate built-ins to use
in this example can be found in the [LFRic built-ins documentation.](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins)

[***Link to solutions***](solutions) (run `make` in the directory
to build and check results).

## Supporting materials

The following modules need to be modified:

* `builtins_alg_mod.x90` - an example of LFRic algorithm that sets up
  fields on `W0` and `W3` function spaces and performs simple mathematical
  operations via a group of `invoke` calls to PSyclone LFRic API built-ins
  (`invoke` call needs to be completed).

Utilities to build and run the code are (do not need to be modified):

* `Makefile` - builds the executable program `builtins` (does not
  need to be modified). Run `make` to build the completed example and
  `./builtins` to run it;
* `builtins_driver.f90` - an example of LFRic-like main program that
  creates the required LFRic objects and calls the algorithm code in
  `builtins_alg_mod.x90`.

### Driver and algorithm structure

As in the [simple kernels example](
../1_simple_kernels/README.md#driver-and-algorithm-structure),
`builtins_driver.f90` sets up  **global 2D mesh**, **partition** and
**local 3D mesh** whilst the creation of **function space** and
**field** object is left to the `builtins_alg_mod.x90` with the
help mesh and finite element order information.

Unlike in the [simple kernels example algorithm](
../1_simple_kernels/simple_kernels_alg_mod.x90), this algorithm does
not need to call kernels to operate on field objects.

## Quick intro to built-ins

Similar to the user-defined [LFRic kernels](../background/LFRic_kernel.md),
the built-in kernels also have metadata but their metadata are defined in
the PSyclone [LFRic (Dynamo 0.3) API.](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html)

Below is an example of metadata for the [built-in `X_plus_Y`](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#x-plus-y)
that adds two fields and stores the result in a third:

```fortran
  type, public, extends(kernel_type) :: X_plus_Y
     private
     type(arg_type) :: meta_args(3) = (/                              &
          arg_type(GH_FIELD, GH_WRITE, ANY_SPACE_1),                  &
          arg_type(GH_FIELD, GH_READ,  ANY_SPACE_1),                  &
          arg_type(GH_FIELD, GH_READ,  ANY_SPACE_1)                   &
          /)
     integer :: iterates_over = DOFS
   contains
     procedure, nopass :: X_plus_Y_code
  end type X_plus_Y
```

The metadata is very similar to a user-defined LFRic kernel metadata
with one major difference: built-ins are called from [PSy layer](
../background/LFRic_PSy.md) loops over degrees of freedom (*DoFs*) of
fields in the built-in, hence metadata identifier for this way of looping,
`DOFS`. This means that fields passed to a built-in call **must be on
the same function space**.

The above mentioned built-in `X_plus_Y` is one of the built-ins to be
used in this example, specifically to calculate

```
field_out_w0 = field1_out_w0 + field2_out_w0
```

which can be done by using

```fortran
    call invoke( ...
                 X_plus_Y(field_out_w0, field1_in_w0, field2_in_w0), &
                 ... )
```

The [*Generated code to support built-in calls* section](
../background/LFRic_PSy.md#generated-code-to-support-built-in-calls)
in the [PSy layer intro](../background/LFRic_PSy.md) gives examples
of how this built-in call is represented in the LFRic PSy layer.

## Appendix

As for [LFRic kernels](../background/LFRic_kernel.md#appendix), the
kernel metadata for the iteration spaces are changing to indicate the
subset of domain the built-in operates on rather than the PSy-layer
looping. In the next PSyclone release `iterates_over = DOFS` will
become `operates_on = DOF`.

The current built-ins are defined for `real`-valued field data.
Built-ins for `integer`-valued fields will be introduced as part
of the support for the field data of other intrinsic types.
