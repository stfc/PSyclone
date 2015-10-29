This type must contain two arguments and a procedure.  The procedure
points to the associated kernel subroutine. The procedure definition
is required as there may be more than one kernel subroutine within the
host module.

::

    type, extends(kernel_type) :: integrate_one_kernel
      ...
      contains
      procedure, nopass :: code => integrate_one_code
    end type integrate_one_kernel

The first of the two variables is called ``meta_args``. This is an
array which describes the type of fields expected by the kernel
code which should be passed from the algorithm layer. For example:

::

    type, extends(kernel_type) :: integrate_one_kernel
      type(arg) :: meta_args(2) = (/&
           arg(READ, (CG(1)*CG(1))**3, FE), &
           arg(SUM, R, FE)/)
      ...
      contains
      procedure, nopass :: code => integrate_one_code
    end type integrate_one_kernel

The content of meta_ags is discussed in the following section.

The second of the two variables is an integer called
``iterates_over``. This defines what the kernel code is written to
iterate over. For example:

::

    type, extends(kernel_type) :: integrate_one_kernel
      type(arg) :: meta_args(2) = (/&
           arg(READ, (CG(1)*CG(1))**3, FE), &
           arg(SUM, R, FE)/)
      integer :: ITERATES_OVER = CELLS
      contains
      procedure, nopass :: code => integrate_one_code
    end type integrate_one_kernel

The meaning and valid values of ``iterates_over`` are discussed in Section
:ref:`iterates-over-label-dyn0p1`.

Meta_args
+++++++++

The ``meta_args`` array specifies information about the data that
the kernel code expects to be passed to it via the argument
list. There is one entry in the ``meta_args`` array for each argument and
the order that these occur in the ``meta_args`` array must be the same
as they are expected in the kernel code argument list.

In the example below, the kernel code expects two arguments to be
passed to the kernel from the algorithm layer.

::

    type, extends(kernel_type) :: integrate_one_kernel
      type(arg) :: meta_args(2) = ...
      ...
    end type integrate_one_kernel

.. note:: The total number of arguments expected by the kernel code is greater that the number of fields passed to the kernel layer by the algorithm layer. This is discussed in Section XXX.

.. warning:: Not sure what to do for 0 argument kernels and do we need an array or a scalar for 1 argument kernels?

The information in the meta_args array is provided by initialising the
array where it is declared using Fortran's array initialisation
support ( ``(/ /)`` ).

::

    type, extends(kernel_type) :: integrate_one_kernel
      type(arg) :: meta_args(2) = (/ ... /)
      ...
    end type integrate_one_kernel

Each entry in the ``meta_args`` array is provided using a ``structure
constructor`` for the ``arg`` type ( ``arg(...)``). The main reason
for doing it this way is that it is valid Fortran 2003 that can be
compiled. However, this information is only ever read by the PSygen
system and therefore could be provided in a different format without
affecting the kernel code.

::

    type, extends(kernel_type) :: integrate_one_kernel
      type(arg) :: meta_args(2) = (/&
           arg(...), &
           arg(...) /)
      ...
    end type integrate_one_kernel

The arg type structure constructor contains 3 pieces of information;
the intent of the field, the function space of the field and the
access type of the field.

In the example below the value of the first piece of information for
the first field (its intent) is ``READ``, the value of the second
piece of information for the first field (its function space) is
``(CG(1)*CG(1))**3`` and the value of the third piece of information
for the first field (its access type) is ``FE``.

::

    type, extends(kernel_type) :: integrate_one_kernel
      type(arg) :: meta_args(2) = (/&
           arg(READ, (CG(1)*CG(1))**3, FE), &
           ...
           /)
      ...
    end type integrate_one_kernel

The content and meaning of the intent, functionspace and access
arguments are discussed separately in the next 3 sections.

Intent
******

The ``intent`` of the array being passed to the kernel routines may be one of READ, WRITE, READWRITE or INC if the kernel treats the data as a ``Field`` and READ, MIN, MAX or SUM if the kerkel treats the data as a ``Global``. See section XXX for a discussion on the differences between Field and Global data.

For fields:

* READ : means that the kernel only reads the data. data must therefore be valid on entry to the kernel.
* WRITE : means that the kernel writes to the data (it may perform subsequent reads and writes). data must therefore be valid on exit from the kernel.
* READWRITE : means that the kernel reads the data first and then at some later point writes to the data (it may perform additional reads anytime after the first read and additional writes after the first write). data must therefore be valid on entry to and on exit from the kernel.
* INC : ??? what does this do ???

For globals:

* READ : means the same as for fields
* MIN : means the minimum value of the array should be returned ??? as a scalar ???
* MAX : means the maximum value of the global array should be returned ?? as a scalar ??
* SUM : means that the data in the global array should be summed up ?? how does this work ??

PSygen is not currently able to check that the values of intents are
set correctly so it is up to the kernel code developer to make sure
that the kernel code and the metadata descriptions are consistent.

Function Space
**************

The function space of an argument specifies how it maps onto the
underlying topology and, additionally, whether the data at a point is
a vector.

As GungHo meshes are extruded to 3D in the vertical from 2D in the
horizontal, they can be split into a horizontal part and a vertical
part. The function space metadata is structured as the cross product
of the mapping of the data to the horizontal part of the mesh and the
mapping of the data to the vertical part of the mesh.

The supported mappings are:

* DG(X), where X can be 0, 1, 2, or 3 : discontinuous galerkin.
* CG(Y), where Y can be 1, 2, or 3 : continuous galerkin
* Lagrange(Y), where Y can be 1, 2, or 3 : same as continuous galerkin
* R : real space

In the case of discontinous galerkin the values at the boundaries
between elements do not need to be continuous. DG(0) has one degree of
freedom per cell and maps to the centre of a cell.

?? DG(1,2,3) explanation ??

In the case of continuous galerkin the values at the boundaries between element need to be continuous. CG(1) has a degree of freedom for each vertex in a cell.

?? CG(2,3) explanation ?? mention CG(0) - same everywhere ??

These discriptions are combined in the function space metadata to give the required positions in a cell. For example:

* DG(0)*DG(0) : centre of the 2d horizontal cell * the centre of the 1d vertical cell giving the centre of the 3d cell.
* CG(1)*CG(1) : vertices of the 2d horizontal cell * vertices of the 1d vertical cell giving the vertices of the 3d cell.
* DG(0)*CG(1) : centre of the 2d horizontal cell * vertices of the 1d vertical cell giving the upper and lower faces of the 3d cell
* CG(1)*DG(0) : vertices of the 2d horizontal cell * the centre of the 1d vertical cell giving the centre of the vertical edges of the 3d cell.

.. note: RT0 needs to be added at some point as that will allow values at edges

It is also possible to add the above descriptions together to create
more complex function spaces. For example DG(0)*DG(0) + CG(1)*CG(1)
which has values at all vertices and the centre of a cell.

The above function space descriptions allow the kernel to specify the
expected degrees of freedom for the associated field. There is an
additional option which, additionally, specifies whether the data
itself is vector valued.

The 3D coordinate field, for example, has x,y,z values at the nodes
and therefore has a vector size of 3. Vector fields are represented as
``**X`` where X is the vector size. So, for example,
``(CG(1)*CG(1))**3`` is the correct specification of the 3D coordinate
field. Notice the extra brackets around the function space declaration
so that vector values are associated with the whole function space.

Access
******

Access can currently be FE or POINTWISE

* FE : means ??? something about stencils ???
* POINTWISE : means that the kernel code knows nothing about any
  degrees of freedom associated with the data so all topological
  information (if any exists) is not passed to the kernel. See Section
  XXX for an explanation of field types.

If iterates_over (see subsequent section) is set to DOFS then the
access to all data in the subroutine must be set to POINTWISE.

.. _iterates-over-label-dyn0p1:

Iterates_over
+++++++++++++

The algorithm layer operates on (logically) whole fields and the
kernel layer operates on a subset of a field. One of the jobs of the
PSy layer is to ``iterate over`` the specified kernel code the
requisite number of times.

The appropriate number of times depends on the type of fields that the
kernel is written for and the way it is written. As a simple
illustration, a field on elements will have a different size to a
field on nodes.

This information is not easy to infer from code, therefore the PSy
layer needs to be informed of what to ``iterate over``. The purpose of
the ``iterates_over`` metadata is for the kernel developer to specify
this information explicitely.

``iterates_over`` currently supports two values: ``CELLS`` and
``DOFS``. These are separately discussed in the following 2 sections.

CELLS
*****

When a kernel specifies ``iterates_over=CELLS`` it means that the
kernel writer has written the kernel so that it expects to perform its
computation over each element. In the current implementation a kernel
operates over a single column of elements therefore the PSy layer
simply needs to call the kernel for all columns.

DOFS
****

When a kernel specifies ``iterates_over=DOFS`` it means that the
kernel writer wants to apply the kernel operations to *all* degrees of
freedom in a field, irrespective of its topology. Such kernels have an
additional length argument for efficiency so that the kernel can
operate over a number of dofs at a time (as determined by the PSy
layer). The PSy layer must call the kernel subroutine length The PSy
layer therefore needs to call the kernel for all degrees of freedom
associated with the function space.

So that there is not confusion over function spaces, all arguments
must have the same function space when ``iterates_over=DOFS`` is
specified. However, the function spaces may have different vector
lengths.

If a function space is a vector then the PSy layer still iterates over
the degrees of freedom and it is up to the kernel to deal with the
vector at each dof correctly.

Kernel Subroutines
++++++++++++++++++

The algorithm layer deals with a generic field type whereas the kernel
subroutines deal directly with the data arrays. It is the
responsibility of the PSy layer to map between the two
representations.

Additional arguments are passed to the kernel subroutines so that they
can work correctly with the data being passed to them.

There are two types of kernel, ones which iterate of topological
entities and ones which iterate over degrees of freedom. These two
kernels differ in their metadata specification (the latter must
specify ``iterates_over=DOFS``) and in the number and type of
additional arguments that are passed.

If the kernel iterates over topological entities then it needs to know
about how to index into the array to get the required data. Therefore
an integer array called a dofmap is passed in that the kernel can use
as an array index.

A dofmap is passed for each argument with a unique topological
entity. The order in which these are passed is determined by the order
in which they are encountered in the argument list.

As all current topological entity kernels are written to work on a
single column of data, an integer column index is also passed in
addition to the dofmaps.

The integer column index is the first argument in a topological entity
kernel, this is followed by any required dofmaps and lastly the data
arrays are passed.

As an example ...

EXAMPLE HERE

A kernel that iterates over degrees of freedom does not know about the
undelying topological structure and therefore is not passed any
dofmaps. For performance purposes such kernels are passed an integer
length variable which tells the kernel how many degrees of freedom to
compute. This argument is the first argument to the subroutine with
subsequent arguments being the field arrays.

As an example ...

EXAMPLE HERE

TBD
+++

1: overview of PSyKAl
2: terminology (see below)

topological entity is ...
cell is the topological entity
element is a particular function space on a topological entity
facet is 1 dimension less that the topological entity
face
node
vertex
line

DG(0), DG(1)
CG(1), CG(0), CG(2)
RT0
R_SPACE

vector field if ** is >1
