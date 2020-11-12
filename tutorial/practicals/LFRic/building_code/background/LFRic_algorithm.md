# LFRic algorithm

[*Algorithm layer in LFRic in PSyclone documentation*](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#algorithm)

## What algorithms do

LFRic algorithms perform mathematical operations on LFRic objects:
**field**s, **operator**s and **scalar**s. Each of these objects
contains data and functionality to manipulate this data (for instance,
a field can write itself out). For the purpose of this documentation
they will be referred to as "LFRic data objects".

The important thing to note here is that algorithms (as well as other
high-level code such as drivers) operate on [full objects](
https://psyclone.readthedocs.io/en/stable/algorithm_layer.html) but
**do not operate directly on object data** (or, in OO terminology, *must
not break encapsulation*). The data is accessed in the [PSy layer](
LFRic_PSy.md) via the required object accessor class, referred to as
*proxy* in the LFRic code.

### Objects and encapsulation

The above mentioned classes are defined in the LFRic infrastructure as:

* Fields: `field_type` and `integer_field_type`;
* Operators: `operator_type` and `columnwise_operator_type`;
* Scalars: `scalar_type`;

and their implementations can be found in similarly-named modules in
the `infrastructure` directory of the LFRic repository.

The **object data is private**, whilst the methods (Fortran procedures)
can be `private` (if used only by the object) or `public`ly available
(e.g. a method to initialise a field or return a pointer to it).

## Algorithm structure

The structure of LFRic algorithms is not as prescribed as the structure
of the [LFRic kernels](LFRic_kernel.md). Their main function is to
manipulate data objects via `invoke` calls to kernels and [built-ins.](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins)

In this section we use the
[`../example1/example1_alg_mod.x90`](
../example1/example1_alg_mod.x90) and [`../example2/example2_alg_mod.x90`](
../example2/example2_alg_mod.x90) to illustrate provisional structure of
LFRic algorithms with `invoke` calls to kernels and built-ins. The
complete examples are provided in relevant [solutions of Example 1]
(../example1/solutions/part1) and [Example 2.](../example2/solutions)

Both examples create fields on different function spaces on basis of the
information about the mesh and finite-element method order passed from
the driver layer. This is not a prescribed rule, however, as fields can
be created in the driver layer and passed to an algorithm. Also, one
algorithm can call another algorithm and pass fields and other data
objects into it.

Roughly speaking, the main parts of an LFRic algorithm are:
* Definitions of global data objects (e.g. fields) and supporting
  structures (e.g. function space, mesh);
* `invoke` calls to kernels and built-ins.

All of this is stored in a module. The naming convention for an algorithm
module is not as strict as for kernels and it can be summarised as
`<base_name>_alg_mod`. Also, unlike for kernels, it is usual for an
algorithm to have more than one subroutine (for instance, algorithms
that perform timestepping in LFRic often have `<base_name>_init`,
`<base_name>_step` and `<base_name>_final` subroutines for different
requirements in a timestepping scheme).

### `invoke` calls

As said above, the defining feature of an LFRic algorithm is one or more
`invoke` calls to [user-defined LFRic kernels](LFRic_kernel.md) and/or
[PSyclone built-ins.](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins)

The `invoke` calls to kernels are really calls to the *kernel objects*
(defined as derived types, as explained in the [*LFRic kernel*
section](LFRic_kernel.md). An example of such call can be found in
the completed algorithm in [Example 1, solution of Part 1](
../example1/solutions/part1/example1/example1_alg_mod.x90):

```fortran
    call invoke( setval_field_w0_kernel_type(field_w0, scalar_w0) )
```

The kernel objects must be made accessible to an algorithm through `use`
statements, e.g.

```fortran
  use setval_field_w0_kernel_mod, only : setval_field_w0_kernel_type
```

The `invoke` calls to built-ins simply use the defined built-in names
in PSyclone (see [built-ins naming scheme](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#naming-scheme)
for more information), e.g. looking at the completed algorithm in
[Example 2 solution](../example2/solutions/example2_alg_mod.x90):

```fortran
    call invoke( ...
                 X_plus_Y(field_out_w0, field1_in_w0, field2_in_w0), &
                 ... )
```

Built-ins do not need to be defined through the algorithm-level `use`
statements as the code for the mathematical operations they perform is
generated in the [PSy layer](LFRic_PSy.md).

These examples of `invoke` calls illustrate the principle of operating on
objects rather than directly on the object data in the algorithm layer.
In this case it is passing the LFRic fields and not the field data as the
arguments to kernel and built-in calls.

### Grouping `invoke` calls and named `invoke`s

It recommended to group as many kernel and built-in calls within a single
`invoke` as possible when writing algorithm code to enable PSyclone to
be effective in performing optimisations.

As each `invoke` call generates a PSy-layer subroutine (see below for
algorithm code processing), it can be easier to specify a [label (name)](
https://psyclone.readthedocs.io/en/stable/algorithm_layer.html#named-invokes)
for the `invoke` (and hence the subroutine) call to make it easier to search
for in the generated code. For an example of a named invoke look into e.g.
the completed algorithm in [Example 2 solution](
../example2/solutions/example2_alg_mod.x90):

```fortran
    call invoke( name = "Builtins on W0 and W3 fields",              &
                 ...
                 X_plus_Y(field_out_w0, field1_in_w0, field2_in_w0), &
                 ... )
```

### `use` statements and encapsulation

As can be seen from the example algorithms
[`../example1/example1_alg_mod.x90`](
../example1/example1_alg_mod.x90) and [`../example2/example2_alg_mod.x90`](
../example2/example2_alg_mod.x90), the `use` statements in algorithms
mainly serve to access the LFRic infrastructure objects (e.g. mesh,
function space, field).

Unlike [kernels](LFRic_kernel.md), there is no inheritance from an
abstract base type. Similar to kernels, however, the general
encapsulation principle of keeping data and procedures `private`
unless they are called by other objects still holds.

## Processing of algorithm code in LFRic

As can be seen from examples, algorithm code in LFRic has `x90` extensions
to indicate to the LFRic build system that it needs to be processed by
PSyclone (i.e. the full file name is `<base_name>_alg_mod.x90`). The
processed algorithm code is saved as `<base_name>_alg_mod.f90` and the
generated [PSy-layer code](LFRic_PSy.md) is saved as
`<base_name>_alg_mod_psy.f90` (see [here](
https://psyclone.readthedocs.io/en/stable/psyclone_script.html) for more
information on running the `psyclone` script).

This process is mimicked in this tutorial as illustrated in the example
`Makefile`s, see e.g. this code from [`../example1/Makefile`](
../example1/Makefile)

```make
%_psy.f90: %.x90
    psyclone $(PSYCLONE_CMD) --config $(PSYCLONE_RELPATH)/config/psyclone.cfg \
    -opsy $*_psy.f90 -oalg $*.f90 $<
```

To see how this code, from the completed algorithm in
[Example 1, solution of Part 1](
../example1/solutions/part1/example1/example1_alg_mod.x90),

```fortran
    call invoke( setval_field_w0_kernel_type(field_w0, scalar_w0) )
```

translates to call to a PSy-layer subroutine we need to look into the
generated `example1_alg_mod.f90` file (not kept in the repository)

```fortran
    CALL invoke_0_setval_field_w0_kernel_type(field_w0, scalar_w0)
```

The name of the generated `invoke` subroutine in this case consists of
a numerical index and the name of the called kernel (see [PSyclone API](
https://psyclone.readthedocs.io/en/stable/algorithm_layer.html#api)
documentation for more information).

In case of the built-in call from the completed algorithm in
[Example 2 solution](../example2/solutions/example2_alg_mod.x90) the `invoke`
subroutine call in the generated `example2_alg_mod.f90` looks something like

```fortran
    CALL invoke_builtins_on_w0_and_w3_fields(field_out_w0, field_out_w3, &
         field1_in_w0, field2_in_w0, field1_in_w3, field2_in_w3)
```

This code also illustrates how the named `invoke` call translates to the
generated subroutine name.
