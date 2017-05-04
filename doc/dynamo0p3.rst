.. _dynamo0.3-api:

dynamo0.3 API
=============

This section describes the dynamo0.3 application programming interface
(API). This API explains what a user needs to write in order to make
use of the dynamo0.3 API in PSyclone.

As with all PSyclone API's the dynamo0.3 API specifies how a user
needs to write the algorithm layer and the kernel layer to allow
PSyclone to generate the PSy layer. These algorithm and kernel API's
are discussed separately in the following sections.

.. _dynamo0.3-api-algorithm:

Algorithm
---------

The general requirements for the structure of an Algorithm are explained
in the :ref:`algorithm-layer` section. This section explains the
dynamo0.3-specific specialisations and extensions.

.. _dynamo0.3-example:

Example
+++++++

An example dynamo0.3 API invoke call is given below with various
different types of objects supported by the API. These different
objects and their use are discussed in the following sections.

::

  call invoke( kernel1(field1, field2, operator1, qr),           &
               builtin1(scalar1, field2, field3),                &
               kernel2(field1, stencil_extent, field3, scalar1), &
               name="some calculation"                           &
             )

Please see the :ref:`algorithm-layer` section for a description of the
``name`` argument.

Field
+++++

.. note:: To be written.

Field Vector
++++++++++++

.. note:: To be written.

Scalar
++++++

.. note:: To be written.

Operator
++++++++

Represents a matrix constructed on a per-cell basis using Local
Matrix Assembly (LMA).

Column-Wise Operator
++++++++++++++++++++

The Dynamo 0.3 API has support for the construction and use of
column-wise/Column Matrix Assembly (CMA) operators. As the name
suggests, these are operators constructed for a whole column of the
mesh. These are themselves constructed from the Local Matrix Assembly
(LMA) operators of each cell in the column. The rules governing
Kernels that have CMA operators as arguments are given in the
:ref:`dynamo0.3-kernel` section below.

There are three recognised Kernel types involving CMA operations;
construction, application (including inverse application) and
matrix-matrix. The following example sketches-out what the use
of such kernels might look like in the Algorithm layer:
::

  use field_mod, only: field_type
  use operator_mod, only : operator_type, columnwise_operator_type
  type(field_type) :: field1, field2, field3
  type(operator_type) :: lma_op1, lma_op2
  type(columnwise_operator_type) :: cma_op1, cma_op2, cma_op3
  real(kind=r_def) :: alpha
  ...
  call invoke(                                                    &
          assembly_kernel(cma_op1, lma_op1, lma_op2),             &
          assembly_kernel2(cma_op2, lma_op1, lma_op2, field3),    &
          apply_kernel(field1, field2, cma_op1),                  &
          matrix_matrix_kernel(cma_op3, cma_op1, alpha, cma_op2), &
          apply_kernel(field3, field1, cma_op3),                  &
          name="cma_example")

The above invoke uses two LMA operators to construct the CMA operator
``cma_op1``.  A second CMA operator, ``cma_op2``, is assembled from
the same two LMA operators but also uses a field. The first of these
CMA operators is then applied to ``field2`` and the result stored in
``field1`` (assuming that the meta-data for ``apply_kernel`` specifies
that it is the first field argument that is written to). The two CMA
operators are then combined to produce a third, ``cma_op3``. This is
then applied to ``field1`` and the result stored in ``field3``.

Note that PSyclone identifies the type of kernels performing
Column-Wise operations based on their arguments as described in
meta-data (see :ref:`cma_meta_data_rules` below). The names of the
kernels in the above example are purely illustrative and are not used
by PSyclone when determining kernel type.

Quadrature rule
+++++++++++++++

.. note:: To be written.

.. _dynamo0.3-alg-stencil:

Stencils
++++++++

Kernel metadata may specify that a Kernel performs a stencil operation
on a field. Any such metadata must provide a stencil type. See the
:ref:`dynamo0.3-api-meta-args` section for more details. The supported
stencil types are ``X1D``, ``Y1D``, ``XORY1D`` or ``CROSS``.

If a stencil operation is specified by the Kernel metadata the
algorithm layer must provide the ``extent`` of the stencil (the
maximum distance from the central cell that the stencil extends). The
dynamo0.3 API expects this information to be added as an additional
``integer`` argument immediately after the relevant field when specifying
the Kernel via an ``invoke``.

For example::

  integer :: extent = 2
  call invoke(kernel(field1, field2, extent))

where ``field2`` has kernel metadata specifying that it has a stencil
access.

``extent``  may also be passed as a literal. For example::

  call invoke(kernel(field1, field2, 2))

where, again, ``field2`` has kernel metadata specifying that it has a
stencil access.

.. note:: The stencil extent specified in the Algorithm layer is not the same as the stencil size passed in to the Kernel. The latter contains the number of cells in the stencil which is dependent on both the stencil type and extent.

If the Kernel metadata specifies that the stencil is of type
``XORY1D`` (which means ``X1D`` or ``Y1D``) then the algorithm layer
must specify whether the stencil is ``X1D`` or ``Y1D`` for that
particular kernel call. The dynamo0.3 API expects this information to
be added as an additional argument immediately after the relevant
stencil extent argument. The argument should be an ``integer`` with
valid values being ``x_direction`` or ``y_direction``, both being
supplied by the ``LFRic`` infrastructure via the
``flux_direction_mod`` fortran module

For example::

  use flux_direction_mod, only : x_direction
  integer :: direction = x_direction
  integer :: extent = 2
  ! ...
  call invoke(kernel(field1, field2, extent, direction))

``direction`` may also be passed as a literal. For example::

  use flux_direction_mod, only : x_direction
  integer :: extent = 2
  ! ...
  call invoke(kernel(field1, field2, extent, x_direction))

If certain fields use the same value of extent and/or direction then
the same variable, or literal value can be provided.

For example::

  call invoke(kernel1(field1, field2, extent,  field3, extent, direction), &
              kernel2(field1, field2, extent2, field4, extent, direction))

In the above example ``field2`` and ``field3`` in ``kernel1`` and
``field4`` in ``kernel2`` will have the same ``extent`` value but
``field2`` in ``kernel2`` may have a different value. Similarly,
``field3`` in ``kernel1`` and ``field4`` in ``kernel2`` will have the
same ``direction`` value.

An example of the use of stencils is available in ``examples/dynamo0p3/eg5``.

There is currently no attempt to perform type checking in PSyclone so
any errors in the type and/or position of arguments will not be picked
up until compile time. However, PSyclone does check for the correct
number of algorithm arguments. If the wrong number of arguments is
provided then an exception is raised.

For example, running test 19.2 from the dynamo0.3 api test suite gives::

  cd <PSYCLONEHOME>/src/tests
  python ../../src/generator.py test_files/dynamo0p3/19.2_single_stencil_broken.f90 
  "Generation Error: error: expected '5' arguments in the algorithm layer but found '4'.
  Expected '4' standard arguments, '1' stencil arguments and '0' qr_arguments'"

PSy-layer
---------

The general details of the PSy-layer are explained in the
:ref:`PSy-layer` section. This section describes any dynamo0p3 specific
issues.

Module name
+++++++++++

The PSy-layer code is contained within a Fortran module. The name of
the module is determined from the algorithm-layer name with "_psy"
appended. The algorithm-layer name is the algorithm's module name if it
is a module, its subroutine name if it is a subroutine that is not
within a module, or the program name if it is a program.

So, for example, if the algorithm code is contained within a module
called "fred" then the PSy-layer module name will be "fred_psy".

.. _dynamo0.3-kernel:

Kernel
-------

The general requirements for the structure of a Kernel are explained
in the :ref:`kernel-layer` section. In the Dynamo API there are three
different Kernel types; general purpose (user-supplied), CMA
(user-supplied) and :ref:`dynamo_built-ins`. This section explains the
rules for the two user-supplied kernel types and then goes on to
describe their metadata and subroutine arguments.

Rules for all User-Supplied Kernels
+++++++++++++++++++++++++++++++++++

In the following, 'operator' refers to both LMA and CMA operator
types.

 1) A Kernel must have at least one argument that is a field, field
    vector, or operator. This rule reflects the fact that a Kernel
    iterates over a space and therefore must have some representation
    over that space.

 2) The continuity of the iteration space of the Kernel is determined
    from the function space of the modified argument. If more than one
    argument is modified then the iteration space is taken to be the
    largest required by any of those arguments. e.g. if a Kernel
    writes to two fields, the first on W3 (discontinuous) and the
    second on W1 (continuous), then the iteration space of that Kernel
    will be determined by the field on the continuous space.

 3) If the function space of the modified argument(s) cannot be
    determined then they are assumed to be continuous. This is
    the case if any of the modified arguments are declared as ANY_SPACE and
    their actual space cannot be determined statically. This assumption is
    always safe but leads to additional computation if the quantities being
    updated are actually on discontinuous function spaces.

 4) Operators do not have halo operations operating on them as they
    are either cell- (LMA) or column-based (CMA) and therefore act
    like discontinous fields.

 5) Any Kernel that writes to an operator will have its iteration
    space expanded such that valid values for the operator are
    computed in the level-1 halo.

 6) Any Kernel that reads from an operator must not access halos
    beyond level 1. In this case PSyclone will check that the Kernel
    does not require values beyond the level-1 halo. If it does then
    PSyclone will abort.

Rules specific to General-Purpose Kernels without CMA Operators
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 1) General-purpose kernels accept arguments of any of the following
    types: field, field vector, LMA operator, scalar integer, scalar
    real.

 2) A Kernel is permitted to write to more than one
    quantity (field or operator) and these quantities may be on the
    same or different function spaces.

 3) A Kernel may not write to a scalar argument. (Only
    :ref:`dynamo_built-ins` are permitted to do this.) Any scalar
    aguments must therefore be declared in the meta-data as
    "GH_READ" - see below.

.. _cma_meta_data_rules:

Rules for Kernels that work with CMA Operators
++++++++++++++++++++++++++++++++++++++++++++++

The Dynamo 0.3 API has support for kernels that assemble, apply (or
inverse-apply) column-wise/Column Matrix Assembly (CMA) operators.
Such operators may also be used by matrix-matrix kernels. There are
thus three types of CMA-related kernels.  Since, by definition, CMA
operators only act on data within a column, they have no horizontal
dependencies. Therefore, kernels that write to them may be
parallelised without colouring.

All three CMA-related kernel types must obey the following rules:

  1) Since a CMA operator only acts within a single column of data,
     stencil operations are not permitted.

  2) No vector quantities (e.g. "GH_FIELD*3" - see below) are
     permitted as arguments.

There are then additional rules specific to each of the three
kernel types. These are described below.

Assembly
########

CMA operators are themselves constructed from Local-Matrix-Assembly
(LMA) operators. Therefore, any kernel which assembles a CMA
operator must obey the following rules:

1) Have one or more LMA operators as read-only arguments.

2) Have exactly one CMA operator argument which must have write access.

3) Other types of argument (e.g. scalars or fields) are permitted but
   must be read-only.

Application and Inverse Application
###################################

Column-wise operators can only be applied to fields. CMA-Application
kernels must therefore:

1) Have a single CMA operator as a read-only argument.

2) Have exactly two field arguments, one read-only and one that is written to.

3) The function spaces of the read and written fields must match the
   from and to spaces, respectively, of the supplied CMA operator.

Matrix-Matrix
#############

A kernel that has just column-wise operators as arguments and zero or
more read-only scalars is identified as performing a matrix-matrix
operation. In this case:

1) Arguments must be CMA operators and, optionally, one or more scalars.

2) Exactly one of the CMA arguments must be written to while all other
   arguments must be read-only.

Metadata
++++++++

The code below outlines the elements of the dynamo0.3 API kernel
metadata, 1) 'meta_args', 2) 'meta_funcs', 3) 'evaluator_shape', 4)
'iterates_over' and 5) 'procedure'.

::

  type, public, extends(kernel_type) :: my_kernel_type
    type(arg_type) :: meta_args(...) = (/ ... /)
    type(func_type) :: meta_funcs(...) = (/ ... /)
    integer :: evaluator_shape = quadrature_XYoZ
    integer :: iterates_over = cells
  contains
    procedure :: my_kernel_code
  end type

These five metadata elements are discussed in order in the following
sections.

.. _dynamo0.3-api-meta-args:

meta_args
#########

The ``meta_args`` array specifies information about data that the
kernel code expects to be passed to it via its argument list. There is
one entry in the ``meta_args`` array for each **scalar**, **field**,
or **operator** passed into the Kernel and the order that these occur
in the ``meta_args`` array must be the same as they are expected in
the kernel code argument list. The entry must be of ``arg_type`` which
itself contains metadata about the associated argument. The size of
the meta_args array must correspond to the number of **scalars**,
**fields** and **operators** passed into the Kernel.

.. note:: it makes no sense for a Kernel to have only **scalar** arguments (because the PSy layer will call a Kernel for each point in the spatial domain) and PSyclone will reject such Kernels.

For example, if there are a total of 2 **scalar** / **field** /
**operator** entities being passed to the Kernel then the meta_args
array will be of size 2 and there will be two ``arg_type`` entries:

::

  type(arg_type) :: meta_args(2) = (/                                  &
       arg_type( ... ),                                                &
       arg_type( ... )                                                 &
       /)

Argument-metadata (metadata contained within the brackets of an
``arg_type`` entry), describes either a **scalar**, a **field** or an
**operator** (either LMA or CMA).

The first argument-metadata entry describes whether the data that is
being passed is for a real scalar (``GH_REAL``), an integer scalar
(``GH_INTEGER``), a field (``GH_FIELD``) or an operator (either
``GH_OPERATOR`` for LMA or ``GH_COLUMNWISE_OPERATOR`` for CMA). This
information is mandatory.

Additionally, argument-metadata can be used to describe a vector of
fields (see the :ref:`dynamo0.3-api-algorithm` section for more
details). If so, the size of the vector is specified using the
notation ``GH_FIELD*N``, where ``N`` is the size of the vector.

As an example, the following ``meta_args`` metadata describes 4
entries, the first is a real scalar, the next two are fields and the
fourth is an operator. The third entry is a field vector of size 3.

::

  type(arg_type) :: meta_args(4) = (/                                  &
       arg_type(GH_REAL, ...),                                         &
       arg_type(GH_FIELD, ... ),                                       &
       arg_type(GH_FIELD*3, ... ),                                     &
       arg_type(GH_OPERATOR, ...)                                      &
       /)

The second entry to argument-metadata (information contained within
the brackets of an ``arg_type``) describes how the Kernel makes use of
the data being passed into it (the way it is accessed within a
Kernel). This information is mandatory. There are currently 4 possible
values of this metadata ``GH_WRITE``, ``GH_READ``, ``GH_INC`` and
``GH_SUM``. However, not all combinations of metadata entries are
valid and PSyclone will raise an exception if an invalid combination
is specified. Valid combinations are specified later in this section.

* ``GH_WRITE`` indicates the data is modified in the Kernel before
  (optionally) being read.

* ``GH_READ`` indicates that the data is read and is unmodified.

* ``GH_INC`` indicates that different iterations of a Kernel make
  contributions to shared values. For example, values at cell faces
  may receive contributions from cells on either side of the
  face. This means that such a Kernel needs appropriate
  synchronisation (or colouring) to run in parallel.

* ``GH_SUM`` is an example of a reduction and is the only reduction
  currently supported in PSyclone. This metadata indicates that values
  are summed over calls to Kernel code.

For example:

::

  type(arg_type) :: meta_args(4) = (/                                  &
       arg_type(GH_REAL,  GH_SUM),                                     &
       arg_type(GH_FIELD, GH_INC, ... ),                               &
       arg_type(GH_FIELD*3, GH_WRITE, ... ),                           &
       arg_type(GH_OPERATOR, GH_READ, ...)                             &
       /)

.. note:: In the Dynamo 0.3 API only :ref:`dynamo_built-ins` are permitted to write to scalar arguments (and hence perform reductions).

For a scalar the argument metadata contains only these two entries.
However, fields and operators require further entries specifying
function-space information.
The meaning of these further entries differs depending on whether a
field or an operator is being described.

In the case of an operator, the 3rd and 4th arguments describe the
``to`` and ``from`` function spaces respectively. In the case of a
field the 3rd argument specifies the function space that the field
lives on. Supported function spaces are ``w0``, ``w1``, ``w2``, ``w3``,
``wtheta``, ``w2h`` and ``w2v``.

For example, the meta-data for a kernel that applies a Column-wise
operator to a field might look like:

::

  type(arg_type) :: meta_args(3) = (/                     &
       arg_type(GH_FIELD, GH_INC, W1),                    &
       arg_type(GH_FIELD, GH_READ, W2H),                  &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, W1, W2H) &
       /)

It may be that a Kernel is written such that a field and/or operators
may be on/map-between any function space. In this case the metadata
should be specified as being one of ``any_space_1``, ``any_space_2``,
..., ``any_space_9``. The reason for having different names is that a
Kernel might be written to allow 2 or more arguments to be able to
support any function space but for a particular call the function
spaces may have to be the same as each other.

In the example below, the first field entry supports any function space but
it must be the same as the operator's ``to`` function space. Similarly,
the second field entry supports any function space but it must be the same
as the operator's ``from`` function space. Note, the metadata does not
forbid ``ANY_SPACE_1`` and ``ANY_SPACE_2`` from being the same.

::

  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD, GH_INC, ANY_SPACE_1 ),                       &
       arg_type(GH_FIELD*3, GH_WRITE, ANY_SPACE_2 ),                   &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2)        &
       /)

Note also that the scope of this naming of any-space function spaces is
restricted to the argument list of individual kernels. i.e. if an
Invoke contains say, two kernel calls that each support arguments on
any function space, e.g. ``ANY_SPACE_1``, there is no requirement that
these two function spaces be the same. Put another way, if an Invoke
contained two calls of a kernel with arguments described by the above
meta-data then the first field argument passed to each kernel call
need not be on the same space.

.. note:: A GH_FIELD argument that specifies GH_WRITE as its access
          pattern must be a discontinuous function in the
          horizontal. At the moment that means it must be ``w3`` but
          in the future there will be more discontinuous function
          spaces. A GH_FIELD that specifies GH_INC as its access
          pattern may be continuous in the vertical (and discontinuous
          in the horizontal), continuous in the horizontal (and
          discontinuous in the vertical), or continuous in both. In
          each case the code is the same. However, if a field is
          discontinuous in the horizontal then it will not need
          colouring and, if is described as being on any space, there
          is currently no way to determine this from the metadata
          (unless we can statically determine the space of the field
          being passed in). At the moment this type of Kernel is
          always treated as if it is continuous in the horizontal,
          even if it is not.

Valid Access Modes
^^^^^^^^^^^^^^^^^^

As mentioned earlier, not all combinations of metadata are
valid. Valid combinations are summarised here. All types of data
(``GH_INTEGER``, ``GH_REAL``, ``GH_FIELD``, ``GH_OPERATOR`` and
``GH_COLUMNWISE_OPERATOR``) may be read within a Kernel and this is
specified in metadata using ``GH_READ``. At least one kernel argument
must be listed as being modified. When data is *modified* in a Kernel
then the permitted access modes depend on the type of data it is and
the function space it is on. Valid values are given in the table
below.

======================	============================    =======================
Argument Type     	Function space                  Access type
======================	============================    =======================
GH_INTEGER        	n/a                             GH_SUM (Built-ins only)
GH_REAL           	n/a                             GH_SUM (Built-ins only)
GH_FIELD                Discontinuous (w3)              GH_WRITE
GH_FIELD                Continuous (not w3)             GH_INC
GH_OPERATOR             Any for both 'to' and 'from'    GH_WRITE
GH_COLUMNWISE_OPERATOR  Any for both 'to' and 'from'    GH_WRITE
======================  ============================    =======================

Note that only Built-ins may modify scalar arguments. There is no
restriction on the number and function-spaces of other quantities that
a general-purpose kernel can modify other than that it must modify at
least one. The rules for kernels involving CMA operators, however, are
stricter and only one argument may be modified (the CMA operator
itself for assembly, a field for CMA-application and a CMA operator
for matrix-matrix kernels). If a kernel writes to quantities on
different function spaces then PSyclone generates loop bounds
appropriate to the largest iteration space. This means that if a
single kernel updates one quantity on a continuous function space and
one on a discontinuous space then the resulting loop will include
cells in the level 1 halo since they are required for a quantity on a
continuous space. As a consequence, any quantities on a discontinuous
space will then be computed redundantly in the level 1 halo. Currently
PSyclone makes no attempt to take advantage of this (by e.g. setting
the appropriate level-1 halo to 'clean').

PSyclone ensures that both CMA and LMA operators are computed
(redundantly) out to the level-1 halo cells. This permits their use in
kernels which modify quantities on continuous function spaces and also
in subsequent redundant computation of other quantities on
discontinuous function spaces. In conjunction with this, PSyclone also
checks (when generating the PSy layer) that any kernels which read
operator values do not do so beyond the level-1 halo. If any such
accesses are found then PSyclone aborts.

Stencil Metadata
^^^^^^^^^^^^^^^^

Field metadata supports an optional 4th argument which specifies that
the field is accessed as a stencil operation within the
Kernel. Stencil metadata only makes sense if the associated field is
read within a Kernel i.e. it only makes sense to specify stencil
metadata if the first entry is ``GH_FIELD`` and the second entry is
``GH_READ``.

Stencil metadata is written in the following format:

::

  STENCIL(type)

where ``type`` may be one of ``X1D``, ``Y1D``, ``XORY1D`` or
``CROSS``.  As the stencil ``extent`` (the maximum distance from the
central cell that the stencil extends) is not provided in the metadata,
it is expected to be provided by the algorithm writer as part of the
``invoke`` call (see Section :ref:`dynamo0.3-alg-stencil`). As there
is currently no way to specify a fixed extent value for stencils in the
Kernel metadata, Kernels must therefore be written to support
different values of extent (i.e. stencils with a variable number of
cells).

The ``XORY1D`` stencil type indicates that the Kernel can accept
either ``X1D`` or ``Y1D`` stencils. In this case it is up to the
algorithm developer to specify which of these it is from the algorithm
layer as part of the ``invoke`` call (see Section
:ref:`dynamo0.3-alg-stencil`).

For example, the following stencil (with ``extent=2``):

::

  | 4 | 2 | 1 | 3 | 5 |

would be declared as

::

  STENCIL(X1D)

and the following stencil (with ``extent=2``)

::

  |   |   | 9 |   |   |
  |   |   | 5 |   |   |
  | 6 | 2 | 1 | 3 | 7 |
  |   |   | 4 |   |   |
  |   |   | 8 |   |   |

would be declared as

::

  STENCIL(CROSS)

Below is an example of stencil information within the full kernel metadata.

::

  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD, GH_INC, W1),                                 &
       arg_type(GH_FIELD, GH_READ, W2H, STENCIL(CROSS)),               &
       arg_type(GH_OPERATOR, GH_READ, W1, W2H)                         &
       /)

There is a full example of this distributed with PSyclone. It may
be found in ``examples/dynamo0p3/eg5``.

Column-wise Operators (CMA)
^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this section we provide example metadata for each of the three
recognised kernel types involving CMA operators.

Column-wise operators are constructed from cell-wise (local) operators.
Therefore, in order to **assemble** a CMA operator, a kernel must have at
least one read-only LMA operator, e.g.:
::
  type(arg_type) :: meta_args(2) = (/                                       &
       arg_type(GH_OPERATOR,            GH_READ,  ANY_SPACE_1, ANY_SPACE_2),&
       arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE, ANY_SPACE_1, ANY_SPACE_2) &
       /)

CMA operators (and their inverse) are **applied** to fields. Therefore any
kernel of this type must have one read-only CMA operator, one read-only
field and a field that is updated, e.g.:
::
  type(arg_type) :: meta_args(3) = (/                                      &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                        &
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2),                        &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2) &
       /)

**Matrix-matrix** kernels compute the product/linear combination of CMA
operators. They must therefore have one such operator that is updated while
the rest are read-only. They may also have read-only scalar arguments, e.g.:
::
   type(arg_type) :: meta_args(3) = (/                                        &
        arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE, ANY_SPACE_1, ANY_SPACE_2), &
        arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2),  &
        arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2),  &
        arg_type(GH_REAL, GH_READ) /)

.. note:: The order with which arguments are specified in meta-data for CMA kernels does not affect the process of identifying the type of kernel (whether it is assembly, matrix-matrix etc.)

meta_funcs
##########

The (optional) second component of kernel meta-data specifies
whether any quadrature or evaluator data is required for a given
function space. (If no quadrature or evaluator data is required then
this meta-data should be omitted.) Consider the
following kernel meta-data:

::
   
    type, extends(kernel_type) :: testkern_operator_type
      type(arg_type), dimension(3) :: meta_args =   &
          (/ arg_type(gh_operator,gh_write,w0,w0),  &
             arg_type(gh_field*3,gh_read,w1),       &
             arg_type(gh_integer,gh_read)           &
          /)
      type(func_type) :: meta_funcs(2) =            &
          (/ func_type(w0, gh_basis, gh_diff_basis) &
             func_type(w1, gh_basis)                &
          /)
      integer, parameter :: evaluator_shape = quadrature_XYoZ
      integer, parameter :: iterates_over = cells
    contains
      procedure() :: code => testkern_operator_code
    end type testkern_operator_type

The ``arg_type`` component of this meta-data describes a kernel that
takes three arguments (an operator, a field and an integer
scalar). Following the ``meta_args`` array we now have a
``meta_funcs`` array. This allows the user to specify that the kernel
requires basis functions (``gh_basis``) and/or the differential of the
basis functions (``gh_diff_basis``) on one or more of the function
spaces associated with the arguments listed in ``meta_args``.  In this
case we require both for the W0 function space but only basis
functions for W1.

evaluator_shape
###############

If a kernel requires basis or differential-basis functions then the
meta-data must also specify the set of points on which these functions
are required. This information is provided by the ``evaluator_shape``
component of the meta-data.  Currently PSyclone supports two shapes;
``quadrature_XYoZ`` for Gaussian quadrature points and
``evaluator_XYZ`` for evaluation at nodal points.

Note that it is an error for kernel meta-data to specify an
``evaluator_shape`` if no basis or differential-basis functions are
required.

iterates over
#############

The fourth type of metadata provided is ``ITERATES_OVER``. This
specifies that the Kernel has been written with the assumption that it
is iterating over the specified entity. For user-supplied kernels this
currently only has one valid value which is ``CELLS``.

Procedure
#########

The fifth and final type of metadata is ``procedure`` metadata. This
specifies the name of the Kernel subroutine that this metadata
describes.

For example:

::

  procedure :: my_kernel_subroutine

Subroutine
++++++++++

.. _stub-generation-rules:

Rules for General-Purpose Kernels
#################################

The arguments to general-purpose kernels (those that do not involve
CMA operators) follow a set of rules which have been specified for
the dynamo0.3 API. These rules are encoded in the ``generate()``
method within the ``ArgOrdering`` abstract class in the
``dynamo0p3.py`` file. The rules, along with PSyclone's naming
conventions, are:

1) If an LMA operator is passed then include the ``cells`` argument.
   ``cells`` is an integer and has intent ``in``.
2) Include ``nlayers``, the number of layers in a column. ``nlayers``
   is an integer and has intent ``in``.
3) For each scalar/field/vector_field/operator in the order specified by
   the meta_args metadata:

    1) if the current entry is a scalar quantity then include the Fortran
       variable in the argument list. The intent is determined from the
       metadata (see :ref:`dynamo0.3-api-meta-args` for an explanation).
    2) if the current entry is a field then include the field array. The
       field array name is currently specified as being
       ``"field_"<argument_position>"_"<field_function_space>``. A field
       array is a real array of type ``r_def`` and dimensioned as the
       unique degrees of freedom for the space that the field operates on.
       This value is passed in separately. Again, the intent is determined
       from the metadata (see :ref:`dynamo0.3-api-meta-args`).

       1) If the field entry has a stencil access then add an integer stencil-size argument with intent ``in``. This will supply the number of cells in the stencil.
       2) If the field entry stencil access is of type ``XORY1D`` then add an integer direction argument with intent ``in``.

    3) if the current entry is a field vector then for each dimension of the vector, include a field array. The field array name is specified as being using ``"field_"<argument_position>"_"<field_function_space>"_v"<vector_position>``. A field array in a field vector is declared in the same way as a field array (described in the previous step).
    4) if the current entry is an operator then first include a dimension size. This is an integer. The name of this size is ``<operator_name>"_ncell_3d"``. Next include the operator. This is a real array of type ``r_def`` and is 3 dimensional. The first two dimensions are the local degrees of freedom for the ``to`` and ``from`` function spaces respectively. The third dimension is the dimension size mentioned before. The name of the operator is ``"op_"<argument_position>``. Again the intent is determined from the metadata (see :ref:`dynamo0.3-api-meta-args`).

4) For each function space in the order they appear in the metadata arguments (the ``to`` function space of an operator is considered to be before the ``from`` function space of the same operator as it appears first in lexicographic order)

    1) Include the number of local degrees of freedom for the function space. This is an integer and has intent ``in``. The name of this argument is ``"ndf_"<field_function_space>``.
    2) If there is a field on this space

        1) Include the unique number of degrees of freedom for the function space. This is an integer and has intent ``in``. The name of this argument is ``"undf_"<field_function_space>``.
        2) Include the dofmap for this function space. This is an integer array with intent ``in``. It has one dimension sized by the local degrees of freedom for the function space.

    3) For each operation on the function space (``basis``, ``diff_basis``, ``orientation``) in the order specified in the metadata

        1) If it is a basis or differential basis function, include the associated argument. This is a real array of kind ``r_def`` with intent ``in``. The rank and extents of this array depend upon the ``evaluator_shape``:

            1) If ``evaluator_shape`` is of type ``_XYZ`` then basis and diff basis are ``real`` arrays of rank 3 with extent (``dimension``, ``number_of_dofs``, ``n_xyz``)
            2) If ``evaluator_shape`` is of type ``_XYoZ`` then basis and diff basis are ``real`` arrays of rank 4 with extent (``dimension``, ``number_of_dofs``, ``n_xy``, ``n_z``)
            3) If ``evaluator_shape`` is of type ``_XoYoZ`` then basis and diff basis are ``real`` arrays of rank 5 with extent (``dimension``, ``number_of_dofs``, ``n_x``, ``n_y``, ``n_z``)

           where ``dimension`` is 1 or 3 and depends upon the function space and whether or not it is a basis or a differential basis function. For the former it is (w0=1, w1=3, w2=3, w3=1, wtheta=1, w2h=3, w2v=3). For the latter it is (w0=3, w1=3, w2=1, w3=3, wtheta=3, w2h=1, w2v=1). ``number_of_dofs`` is the number of degrees of freedom associated with the function space. The name of the argument is ``"basis_"<field_function_space>`` or ``"diff_basis_"<field_function_space>``, as appropriate.

        2) If it is an orientation array, include the associated argument. The argument is an integer array with intent ``in``. There is one dimension of size the local degrees of freedom for the function space. The name of the array is ``"orientation_"<field_function_space>``.

5) If Quadrature or an Evaluator is required (this is the case if any of the function spaces require basis or differential basis functions)

    1) include integer scalar arguments with intent ``in`` that specify the extent of the basis/diff-basis arrays:

       1) If ``evaluator_shape`` is of type ``*_XYZ`` then pass ``n_xyz``
       2) If ``evaluator_shape`` is of type ``*_XYoZ`` then pass ``n_xy`` and ``n_z``
       3) If ``evaluator_shape`` is of type ``*_XoYoZ`` then pass ``n_x``, ``n_y`` and ``n_z``

    2) if Quadrature is required (``evaluator_shape`` is of type ``quadrature_type_*``) then include weights which are real arrays of kind ``r_def``:

       1) If ``quadrature_type_XYZ`` pass in ``w_XZY(n_xyz)``
       2) If ``quadrature_type_XYoZ`` pass in ``w_XZ(n_xy)`` and ``w_z(n_z)``
       3) If ``quadrature_type_XoYoZ`` pass in ``w_X(n_x)``, ``w_Y(n_y)`` and ``w_z(n_z)``

Rules for CMA Kernels
#####################

Kernels involving CMA operators are restricted to just three types;
assembly, application/inverse-application and matrix-matrix.
We give the rules for each of these in the sections below.

Assembly
^^^^^^^^

An assembly kernel requires the column-banded dofmap for both the to-
and from-function spaces of the CMA operator being assembled as well
as the number of dofs for each of the dofmaps. The full set of rules is:

    1) Include the ``cell`` argument. ``cell`` is an integer and has
       intent ``in``.
    2) Include ``nlayers``, the number of layers in a column. ``nlayers``
       is an integer and has intent ``in``.
    3) Include the number of cells in the 2D mesh, ``ncell_2d``, which is
       an integer with intent ``in``.
    4) Include the total number of cells, ``ncell_3d``, which is an integer
       with intent ``in``.
    5) For each argument in the ``meta_args`` meta-data array:
       
       1) If it is a LMA operator, include a real, 3-dimensional
          array of type ``r_def``. The first two dimensions are the local
          degrees of freedom for the ``to`` and ``from`` spaces,
          respectively. The third dimension is ``ncell_3d``.
	  
       2) If it is a CMA operator, include a real, 3-dimensional array
          of type ``r_def``. The first dimension is is
          ``"bandwidth_"<operator_name>``, the second is
          ``"nrow_"<operator_name>``, and the third is ``ncell_2d``.
	  
	  1) Include the number of rows in the banded matrix.  This is
	     an integer with intent ``in`` and is named as
	     ``"nrow_"<operator_name>``.
          2) If the from-space of the operator is *not* the same as the 
	     to-space then include the number of columns in the banded
	     matrix.  This is an integer with intent ``in`` and is named as
	     ``"ncol_"<operator_name>``.
	  3) Include the bandwidth of the banded matrix. This is an
	     integer with intent ``in`` and is named as
	     ``"bandwidth_"<operator_name>``.
	  4) Include banded-matrix parameter ``alpha``. This is an integer
	     with intent ``in`` and is named as ``"alpha_"<operator_name>``.
	  5) Include banded-matrix parameter ``beta``. This is an integer
	     with intent ``in`` and is named as ``"beta_"<operator_name>``.
	  6) Include banded-matrix parameter ``gamma_m``. This is an integer
	     with intent ``in`` and is named as ``"gamma_m_"<operator_name>``.
	  7) Include banded-matrix parameter ``gamma_p``. This is an integer
	     with intent ``in`` and is named as ``"gamma_p_"<operator_name>``.

       3) If it is a field or scalar argument then include arguments following
          the same rules as for general-purpose kernels. 
	  
    6) For each unique function space in the order they appear in the
       metadata arguments (the ``to`` function space of an operator is
       considered to be before the ``from`` function space of the same
       operator as it appears first in lexicographic order):

       1) Include the number of degrees of freedom per cell for
	  the space. This is an integer with intent ``in``. The name of this
	  argument is ``"ndf_"<arg_function_space>``.

       2) If there is a field on this space then:

	  1) Include the unique number of degrees of freedom for the
 	     function space. This is an integer and has intent ``in``.
             The name of this argument is ``"undf_"<field_function_space>``.

	  2) Include the dofmap for this space. This is an integer array
	     with intent ``in``. It has one dimension sized by the local
	     degrees of freedom for the function space.

	3) If the CMA operator has this space as its to/from space then
	   include the column-banded dofmap, the list of offsets for the
	   to/from-space. This is an integer array of rank 2. The first
	   dimension is ``"ndf_"<arg_function_space>```` and the second
           is ``nlayers``.


Application/Inverse-Application
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A kernel applying a CMA operator requires the column-indirection
dofmap for both the to- and from-function spaces of the CMA
operator. Since it does not have any LMA operator arguments it does
not require the ``ncell_3d`` and ``nlayers`` scalar arguments.
The full set of rules is then:

    1) Include the ``cell`` argument. ``cell`` is an integer and has
       intent ``in``.
    2) Include the number of cells in the 2D mesh, ``ncell_2d``, which is
       an integer with intent ``in``.
    3) For each argument in the ``meta_args`` meta-data array:

       1) If it is a field, include the field array. This is a real
          array of kind ``r_def`` and is of rank 1.  The field array name
	  is currently specified as being
	  ``"field_"<argument_position>"_"<field_function_space>``. The
	  extent of the array is the number of unique degrees of freedom
	  for the function space that the field is on.  This value is
	  passed in separately. The intent of the argument is determined
	  from the metadata (see :ref:`dynamo0.3-api-meta-args`).

       2) If it is a CMA operator, include it and its associated
          parameters (see Rule 5 of CMA Assembly kernels).

    4) For each of the unique function spaces encountered in the
       meta-data arguments (the ``to`` function space of an operator
       is considered to be before the ``from`` function space of the
       same operator as it appears first in lexicographic order):

       1) Include the number of degrees of freedom for the associated
	  function space. This is an integer with intent ``in``. The name
	  of this argument is ``"ndf_"<field_function_space>``.
       2) Include the number of unique degrees of freedom for the associated
	  function space. This is an integer with intent ``in``. The name
	  of this argument is ``"undf_"<field_function_space>``.
       3) Include the dofmap for this function space. This is a rank-1 integer
	  array with extent equal to the number of degrees of freedom of
	  the space (``"ndf_"<field_function_space>``).

    5) Include the indirection map for the to-space of the CMA operator.
       This is a rank-1 integer array with extent ``nrow``.
    6) If the from-space of the operator is *not* the same as the to-space
       then include the indirection map for the from-space of the CMA operator.
       This is a rank-1 integer array with extent ``ncol``.

Matrix-Matrix
^^^^^^^^^^^^^

Does not require any dofmaps and also does not require the ``nlayers``
and ``ncell_3d`` scalar arguments. The full set of rules are then:

    1) Include the ``cell`` argument. ``cell`` is an integer and has
       intent ``in``.
    2) Include the number of cells in the 2D mesh, ``ncell_2d``, which is
       an integer with intent ``in``.
    3) For each CMA operator or scalar argument specifed in meta-data:

       1) If it is a CMA operator, include it and its associated
	  parameters (see Rule 5 of CMA Assembly kernels).

       2) If it is a scalar argument include the corresponding Fortran
	  variable in the argument list with intent ``in``.

.. _dynamo_built-ins:

Built-ins
---------

The basic concept of a PSyclone Built-in is described in the
:ref:`built-ins` section.  In the Dynamo 0.3 API, calls to
built-ins generally follow a convention that the field/scalar written
to comes last in the argument list. Dynamo 0.3 built-ins must conform to the
following four rules:

 1) Built-in kernels must have one and only one modified (i.e. written
    to) argument.

 2) There must be at least one field in the argument list. This is so
    that we know the number of dofs to iterate over.

 3) Kernel arguments must be either fields or scalars.

 4) All field arguments to a given built-in must be on the same
    function space. This is because all current built-ins iterate over
    dofs and therefore all fields should have the same number. It also
    means that we can determine the number of dofs uniquely when a
    scalar is written to.

The built-ins supported for the Dynamo 0.3 API are
listed in alphabetical order below. For clarity, the calculation
performed by each built-in is described using Fortran array syntax; this
does not necessarily reflect the actual implementation of the
built-in (*e.g.* it could be implemented by PSyclone
generating a call to an optimised maths library).

axpby
+++++

**axpby** (*a*, *field1*, *b*, *field2*, *field3*)

Performs: ::
   
   field3(:) = a*field1(:) + b*field2(:)

where:

* real(r_def), intent(in) :: *a*, *b*
* type(field_type), intent(in) :: *field1*, *field2*
* type(field_type), intent(out) :: *field3*

inc_axpby
+++++++++

**inc_axpby** (*a*, *field1*, *b*, *field2*)

Performs: ::
   
   field1(:) = a*field1(:) + b*field2(:)

where:

* real(r_def), intent(in) :: *a*, *b*
* type(field_type), intent(inout) :: *field1*
* type(field_type),    intent(in) :: *field2*

axpy
++++

**axpy** (*a*, *field1*, *field2*, *field3*)

Performs: ::
   
   field3(:) = a*field1(:) + field2(:)

where:

* real(r_def), intent(in) :: *a*
* type(field_type), intent(in) :: *field1*, *field2*
* type(field_type), intent(out) :: *field3*

inc_axpy
++++++++

**inc_axpy** (*a*, *field1*, *field2*)

Performs an AXPY and returns the result as an increment to the first
field: ::
   
   field1(:) = a*field1(:) + field2(:)

where:

* real(r_def), intent(in) :: *a*
* type(field_type), intent(inout) :: *field1*
* type(field_type),    intent(in) :: *field2*

copy_field
++++++++++

**copy_field** (*field1*, *field2*)

Copy the values from *field1* into *field2*: ::

   field2(:) = field1(:)

where:

* type(field_type), intent(in) :: *field1*
* type(field_type), intent(out) :: *field2*

copy_scaled_field
+++++++++++++++++

**copy_scaled_field** (*value*, *field1*, *field2*)

Multiplies a field by a scalar and stores the result in a second field: ::
  
  field2(:) = value * field1(:)

where:

* real(r_def), intent(in) :: *value*
* type(field_type), intent(in) :: *field1*
* type(field_type), intent(out) :: *field2*

divide_field
++++++++++++

**divide_field** (*field1*, *field2*)

Divides the first field by the second and returns it: ::

   field1(:) = field1(:) / field2(:)

where:

* type(field_type), intent(inout) :: *field1*
* type(field_type),    intent(in) :: *field2*

divide_fields
+++++++++++++

**divide_fields** (*field1*, *field2*, *field3*)

Divides the first field by the second and returns the result in the third: ::

   field3(:) = field1(:) / field2(:)

where:

* type(field_type), intent(in) :: *field1*, *field2*
* type(field_type), intent(out) :: *field3*

inner_product
+++++++++++++

**inner_product** (*field1*, *field2*, *sumval*)

Computes the inner product of the fields *field1* and *field2*, *i.e.*: ::

  sumval = SUM(field1(:)*field2(:))

where:

* type(field_type), intent(in) :: *field1*, *field2*
* real(r_def), intent(out) :: *sumval*

.. note:: when used with distributed memory this built-in will trigger
          the addition of a global sum which may affect the
          performance and/or scalability of the code.

inc_field
+++++++++

**inc_field** (*field1*, *field2*)

Adds the second field to the first and returns it: ::

  field1(:) = field1(:) + field2(:)

where:

* type(field_type), intent(inout) :: *field1*
* type(field_type),    intent(in) :: *field2*

minus_fields
++++++++++++

**minus_fields** (*field1*, *field2*, *field3*)

Subtracts the second field from the first and stores the result in
the third. *i.e.* performs the operation: ::
  
  field3(:) = field1(:) - field2(:)

where:

* type(field_type), intent(in) :: *field1*
* type(field_type), intent(in) :: *field2*
* type(field_type), intent(out) :: *field3*

multiply_fields
+++++++++++++++

**multiply_fields** (*field1*, *field2*, *field3*)

Multiplies two fields together and returns the result in a third field: ::

  field3(:) = field1(:)*field2(:)

where:

* type(field_type), intent(in) :: *field1*, *field2*
* type(field_type), intent(out) :: *field3*

plus_fields
+++++++++++

**plus_fields** (*field1*, *field2*, *field3*)

Sums two fields: ::
  
  field3(:) = field1(:) + field2(:)

where:

* type(field_type), intent(in) :: *field1*
* type(field_type), intent(in) :: *field2*
* type(field_type), intent(out) :: *field3*

scale_field
+++++++++++

**scale_field** (*scalar*, *field1*)

Multiplies a field by a scalar value and returns the field: ::

  field1(:) = scalar * field1(:)

where:

* real(r_def),      intent(in) :: *scalar*
* type(field_type), intent(inout) :: *field1*

set_field_scalar
++++++++++++++++

**set_field_scalar** (*value*, *field*)

Set all elements of the field *field* to the value *value*.
The field may be on any function space.

* type(field_type), intent(out) :: *field*
* real(r_def), intent(in) :: *value*

sum_field
+++++++++

**sum_field** (*field*, *sumval*)

Sums all of the elements of the field *field* and returns the result
in the scalar variable *sumval*: ::
  
  sumval = SUM(field(:))

where:

* type(field_type), intent(in) :: field
* real(r_def), intent(out) :: sumval

.. note:: when used with distributed memory this built-in will trigger
          the addition of a global sum which may affect the
          performance and/or scalability of the code.

Boundary Conditions
-------------------

In the dynamo0.3 API, boundary conditions for a field can be enforced
by the algorithm developer by calling a particular Kernel called
``enforce_bc_type``. This kernel takes a field as input and applies
boundary conditions. For example:

::

  call invoke( kernel_type(field1, field2), &
               enforce_bc_type(field1)      &
             )

The particular boundary conditions that are applied are not known by
PSyclone, PSyclone simply recognises this kernel by its name and passes
pre-specified dofmap and boundary_value arrays into its kernel
implementation, the contents of which are set by the LFRic
infrastructure.

There is one situation where boundary conditions are applied without
the algorithm developer having to specify them explicitly. Boundary
conditions are added automatically after a call to
``matrix_vector_type`` if the fields being passed into the call are on
a vector function space (one of ``w1``, ``w2``, ``w2h`` or
``w2v``). This functionality was requested by the scientists to avoid
having to write a large number of ``enforce_bc_type`` calls in the
algorithm layer as ``matrix_vector_type`` may be used a large number
of times in an algorithm.

Example ``eg4`` in the ``examples/dynamo`` directory includes a call
to ``matrix_vector_type`` so can be used to see the boundary condition
code that is added by PSyclone. See the ``README`` in the
``examples/dynamo`` directory for instructions on how to run this
example.


Conventions
-----------

There is a convention in the dynamo0.3 API kernel code that if the
name of the operation being performed is ``<name>`` then a kernel file
is ``<name>_mod.[fF90]``, the name of the module inside the kernel
file is ``<name>_mod``, the name of the kernel metadata in the module
is ``<name>_type`` and the name of the kernel subroutine in the module
is ``<name>_code``. PSyclone does not need this convention to be followed apart from the stub generator (see the :ref:`stub-generation` Section ) where the name of the metadata to be parsed is determined from the module name.

The contents of the metadata is also usually declared private but this
does not affect PSyclone.

Finally, the ``procedure`` metadata (located within the kernel
metadata) usually has ``nopass`` specified but again this is ignored
by PSyclone.

Transformations
---------------

This section describes the dynamo-api-specific transformations. In all
cases these transformations are specialisations of generic
transformations described in the :ref:`transformations` section. The
difference between these transformations and the generic ones are that
these perform dynamo-api-specific checks to make sure the
transformations are valid. In practice these transformations perform
the required checks then call the generic ones internally.

The use of the dynamo-api-specific transformations is exactly the same
as the equivalent generic ones in all cases excepting
**DynamoLoopFuseTrans**. In this case an additional optional argument
**same_space** has been added to the **apply** method. The reason for
this is to allow loop fusion when one or more of the iteration-spaces
is determined by a function space that is unknown by PSyclone at
compile time. This is the case when the **ANY_SPACE** function space
is specified in the Kernel metadata. By default PSyclone will not
allow loop fusion if it does not know the spaces are the same. The
**same_space** option allows the user to specify that
the spaces are the same. This option should therefore be used with
caution. Note, if PSyclone knows the spaces are different this option
has no effect and the transformation will always raise an exception.

The Dynamo-specific transformations currently available are given
below. If the name of a transformation includes "Dynamo0p3" it means
that the transformation is only valid for this particular API. If the
name of the transformation includes "Dynamo" then it should work with
all versions of the Dynamo API.

.. autoclass:: transformations.DynamoLoopFuseTrans
    :members:
    :noindex:

.. autoclass:: transformations.DynamoOMPParallelLoopTrans
    :members:
    :noindex:

.. autoclass:: transformations.Dynamo0p3OMPLoopTrans
    :members:
    :noindex:

.. autoclass:: transformations.Dynamo0p3ColourTrans
    :members:
    :noindex:
