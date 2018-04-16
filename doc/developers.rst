Developers guide
****************

.. Generating API-specific code
.. ============================
.. 
.. This section explains how to create a new API in PSyclone. PSyclone
.. currently supports the following API's; the original prototype gungho
.. implementation, dynamo versions 0.1 and 0.3, and gocean versions 0.1
.. and 1.0.
.. 
.. config.py
.. ---------
.. 
.. The names of the supported API's and the default API are specified in
.. `config.py`. When adding a new API you must add the name you would like
.. to use to the `SUPPORTEDAPIS` list (and change the `DEFAULTAPI` if
.. required).
.. 
.. parse.py
.. --------
.. 
.. The parser reads the algorithm code and associated kernel metadata.
.. 
.. The parser currently assumes that all API's will use the `invoke()`
.. API for the algorithm-to-psy layer but that the content and structure
.. of the metadata in the kernel code may differ. If the algorithm API
.. differs, then the parser will need to be refactored. This is beyond
.. the scope of this document and is currently not considered in the
.. PSyclone software architecture.
.. 
.. The kernel metadata however, will be different from one API to
.. another. To parse this kernel-API-specific metadata a
.. `KernelTypeFactory` is provided which should return the appropriate
.. `KernelType` object. When adding a new API a new API-specific subclass
.. of `KernelType` should be created and added to the `create()` method
.. in the `KernelTypeFactory` class. If the kernel metadata happens to be
.. the same as another existing API then the existing `KernelType`
.. subclass can be used for the new API.
.. 
.. The `KernelType` subclass needs to specialise the `KernelType
.. __init__` method and initialise the `KernelType` base class with the
.. supplied arguments. The role of the `KernelType` subclass is to create
.. a kernel-metadata-specific subclass of the `Descriptor` class and
.. populate this with the relevant API-specific metadata. After doing
.. this is appends the kernel-metadata-specific subclass instance is
.. appended to the `_arg_descriptors` list provided by the `KernelType`
.. base class.
.. 
.. TBC
.. 
.. This information
.. 
.. KernelType base class assumes kernel metadata stored as a type. Searches for that type.
.. Checks whether the metadata is public (it should be ?)
.. Assumes iterates_over variable.
.. Binding to a procedure - assumes one of two styles.
.. Assumes a meta_args type
.. *What about our func_args type???*
.. 
.. type x
.. meta_args=
.. *meta_func=*
.. iterates_over=
.. code => or code =
.. end type x
.. 
.. The descriptor class ...
.. 
.. psyGen.py
.. ---------
.. 
.. factory
.. +++++++
.. 
.. A new file needs to be created and the following classes found in
.. psyGen.py need to be subclassed.
.. 
.. PSy, Invokes, Invoke, Schedule, Loop, Kern, Arguments, Argument
.. You may also choose to subclass the Inf class if required.
.. 
.. The subclass of the PSy class then needs to be added as an option to
.. the create method in the PSyFactory class.
.. 
.. Initialisation
.. ++++++++++++++
.. 
.. The parser information passed to the PSy layer is used to create an
.. invokes object which in turn creates a list of invoke objects. Each
.. invoke object contains a schedule and a schedule consists of loops and
.. calls. Finally, a call contains an arguments object which itself
.. contains a list of argument objects.
.. 
.. To make sure the subclass versions of the above objects are created
.. the __init__ methods of the subclasses must make sure they create
.. the appropriate objects.
.. 
.. Some of the baseclass constructors (__init__ methods) support the
.. classname being provided. This allow them to instantiate the
.. appropriate objects without knowing what they are.
.. 
.. gen_code()
.. ++++++++++
.. 
.. All of the above classes (with the exception of PSy which supports a
.. gen() method) have the gen_code() method. This method passes the
.. parent of the generation tree and expect the object to add the code
.. associated with the object as a child of the parent. The object is
.. then expected to call any children. This approach is powerful as it
.. lets each object concentrate on the code that it is responsible for.
.. 
.. Adding code in gen_code()
.. +++++++++++++++++++++++++
.. 
.. The f2pygen classes have been developed to help create appropriate
.. fortran code in the gen_code() method.
.. 
.. When writing a gen_code() method for a particular object and API it is
.. natural to add code as a child of the parent provided by the callee of
.. the method. However, in some cases we do not want code to appear at
.. the current position in the hierarchy.
.. 
.. The add() method
.. ++++++++++++++++
.. 
.. PSyclone supports this via the add() method
.. 
.. explicitly place at the appropriate place in the hierarchy. For example,
.. parent.parent.add(...)
.. 
.. optional argument. default is auto. This attempts to place code in the
.. expected place. For example, specify a declaration. auto finds a
.. correct place to put this code.
.. 
.. Specify position explicitly
.. "before", "after", "first", "last"
.. 
.. Sometimes don't know exactly where to place. On example that is
.. supported is when you want to add something before or after a loop
.. nest. start_parent_loop(). This method recurses up until the parent is
.. not a loop, it then skips any comments (as they may be directives) and
.. return this position. Therefore supports an arbitrary number of loops
.. and directives.

Dynamo0p3
=========

Mesh
----

The dynamo0.3 api support meshes that are unstructured in the
horizontal and structured in the vertical. This is often thought of as
a horizontal 2D unstructured mesh which is extruded into the
vertical. The LFRic infrastructure represents this mesh as a list of
2D cells with a scalar value capturing the number of levels in the
vertical "column".

Cells
-----

The dynamo0.3 api currently assumes that all kernels which support
iterating over cells work internally on a column of
cells. This means that PSyclone need only be concerned with iterating
over cell-columns in the horizontal. As a result the LFRic
infrastructure presents the mesh information to PSyclone as if the
mesh were 2-dimensional. From now on this 2D view will be assumed
i.e. a cell will actually be a column on cells. The LFRic
infrastracture provides a global 2D cell number from 1 to the number
of cells.

For example, a simple quadrilateral element mesh with 4 cells might be
numbered in the following way.

.. image:: cells_global.png
	   :width: 120

When the distributed memory option is switched on in the dynamo0.3 api
(see the :ref:`distributed_memory` Section) the cells in the model are
partitioned amongst processors and halo cells are added at the
boundaries to a depth determined by the LFRic infrastructure. In this
case the LFRic infrastructure maintains the global cell number and
adds a unique local cell number from 1 to the number of cells in each
partition, including any halo cells.

An example for a depth-1 halo implementation with the earlier mesh
split into 2 partitions is given below, with the halo cells being
coloured red for clarity. An example local numbering is also provided
below the cells. Notice the local numbering ensures that owned cells
have lower numbers than halo cells.

.. image:: cells_distributed.png
	   :width: 200

Dofs
----

The LFRic infrastracture provides a global degrees-of-freedom (dof)
number from 1 to the number of dofs. The infrastructure also numbers
dofs so that the values in a column are contiguous and their values
increase in the vertical.  Thus, given the dof numbers for the
"bottom" cell, the rest of the dof numbers can be determined for the
column. This set of dof numbers for the bottom cell is called a
dofmap.

Dofs represent fields in the mesh. Fields can either be continuous or
discontinuous. Continuous fields have values that are coordinated
between neighbouring cells (there are no discontinuities across cell
boundaries). Dofs that represent continuous fields a shared between
neighbouring cells. Discontinuous fields have values that are not
necessarily related between neighbouring cells (there can be
discontinuities between across cell boundaries). Dofs that represent
discontinuous fields are local to a cell.

Discontinuous Dofs
------------------

A simple discontinuous dof example is given below. In this case each
cell contains 1 dof and there are 10 cells in a column. We only show
the bottom cells and their corresponding dof values. As explained
earlier the dof numbers increase up the column contigiously, so the
dof numbered 1 has a dof in the cell above it with value 2, the cell
above that has a dof with value 3 etc.

.. image:: dofs_disc_global.png
	   :width: 120

As discussed in the previous section, when the distributed memory
option is switched on in the dynamo0.3 api (see the
:ref:`distributed_memory` Section) the cells in the model are
partitioned amongst processors and halo cells are added at the
boundaries to a depth determined by the LFRic infrastructure. This
results in the dofs being replicated in the halo cells, resulting in a
dof halo. As for cells, the LFRic infrastructure maintains the global
dof number and adds a unique local dof number from 1 to the number of
cells in each partition, including any halo dofs.

An example for a depth-1 halo implementation with the earlier mesh
split into 2 partitions is given below, with the halo cells being
drawn in grey and halo dofs being coloured red for clarity. An example
local partition numbering is also provided below the dofs. As with
cells, notice the local numbering ensures that owned dofs have lower
numbers than halo dofs.

.. image:: dofs_disc_distributed.png
	   :width: 200

Continuous Dofs
---------------

A simple continuous dof example is given below for the same mesh as
before. In this case dofs are on cell edges in the horizontal and
there are 10 cells in a column. Again we only show the bottom cells
and their corresponding dof values. As explained earlier the dof
numbers increase up the column contigiously, so the dof numbered 1 has
a dof in the cell above it with value 2, the cell above that has a dof
with value 3 etc.

.. image:: dofs_cont_global.png
	   :width: 140

As already explained, when the distributed memory
option is switched on in the dynamo0.3 api (see the
:ref:`distributed_memory` Section) the cells in the model are
partitioned amongst processors and halo cells are added at the
boundaries to a depth determined by the LFRic infrastructure.

In the example below we ignore the adding of halo cells and just look
at the partitioning of cells amongst processors (with the same mesh
and 2 partitions as shown earlier). It can be seen that the dofs
shared between cells which are on different partitions now need to be
replicated if fields on continuous dofs are going to be able to be
computed locally on each partition. This concept is different to halos
as there are no halo cells here, the fact that the cells are
partitioned has meant that continuous dofs on the edge of the
partition are replicated. The convention used in dynamo0.3 is that the
cell with the lowest global id determines which partition owns the
dofs and which has a copy. Dofs which are copies are called
annexed. Annexed dofs are denoted by blue in the example.

.. image:: dofs_cont_annexed.png
	   :width: 160

An example for a depth-1 halo implementation with the earlier mesh
split into 2 partitions is given below, with the halo cells being
drawn in grey and halo dofs being coloured red for clarity. An example
local partition numbering is also provided below the dofs. Notice the
local numbering ensures that owned dofs have lower numbers than annexed
dofs, which in turn have lower numbers than halo dofs.

.. image:: dofs_cont_halos.png
	   :width: 230

Cell and Dof Ordering
---------------------

Cells in a partition are sequentially numbered by the LFRic
infrastructure, starting at 1, so that local cells occur first, then
level1 halo cells, then level2 halo cells etc. A benefit of this
layout is that it makes it easy for PSyclone to specify the required
iteration space for cells as a single range, allowing a single fortran
do loop (or other language construct if required) to be generated. The
LFRic infrastructure provides an API that returns the number of the
last owned cell, the number of the last halo cell at a particular
depth and the number of the last halo cell, to support PSyclone code
generation.

Dofs on a processor are also sequentially numbered by the LFRic
infrastructure, starting at 1, so that local dofs occur first, then
annexed dofs (if the field is continuous), then level1 halo dofs, then
level2 halo dofs etc. Again, a benefit of this layout makes it easy
for PSyclone to specify the required iteration space for dofs as a
single range. As before the LFRic infrastructure provides an API that
returns the number of the last owned dof, the number of the last
annexed dof, the number of the last halo dof at a particular depth and
the number of the last halo dof, to support PSyclone code generation.


Loop iterators
--------------

In the current implementation of the dynamo0.3 API it is possible to
iterate (loop) either over cells or over dofs. At the moment all coded
kernels are written to iterate over cells and all builtin kernels are
written to iterate over dofs, but that does not have to be the
case.

The loop iteration information is specified in the kernel metadata. In
the case if builtin's there is kernel metadata but it is part of
PSyclone and is specified in
`src/psyclone/dynamo0p3_builtins_mod.f90`.

Dof iterators
-------------

When a kernel, that is written to iterate over dofs, modifies a field,
it is PSyclone's role to ensure that all dofs in that field are
updated. If the distributed memory flag is set to `False` then
PSyclone must iterate over all dofs. PSyclone simply needs to create a
loop that iterates from 1 to the total number of dofs. The latter
value is provided by the LFRic API.

If the distributed memory flag is set to `True` then PSyclone must
ensure that each partition iterates over owned dofs. Again PSyclone
just needs to create a loop that iterates from 1 to total number of
owned dofs on that partition. The latter value is provided by the
LFRic API.

When the distributed memory flag is set to True an aditional
configuration option can be set which makes PSyclone always iterate
over both owned and annexed dofs. Whilst this is not necessary for
correctness, it can improve performance by reducing the number of halo
exchanges required (at the expense of computing annexed dofs
redundantly). The only change for PSyclone is that it calls a
different LFRic routine which returns the id of the last annexed
dof. This iteration space will necessarily also include all owned dofs due
to the ordering of dof id's discussed earlier.

The configuration variable is called `COMPUTE_ANNEXED_DOFS` and is
found in the the `config.py` configuration file. If it is `True` then
annexed dofs are always computed in loops that iterate over dofs and
if it is `False` then annexed dofs are not computed. The default in
PSyclone is `True`.

The computation of annexed dofs could have been added as a
transformation optimisation. The reason for using a configuration
switch is that it is then guaranteed that annexed dofs are always
computed for loops that iterate over dofs which then allows us to
always remove certain halo exchanges without needing to add any new
ones. This is discussed later.

Cell iterators: Continuous
--------------------------

When a kernel is written to iterate over cells and modify a continuous
field, PSyclone always computes dofs on owned cells and redundantly
computes dofs in the level 1 halo. Users can apply a redundant
computation transformation to increase the halo depth for additional
redundant computation but it must always at least computed the level 1
halo. The reason for this is to ensure that the shared dofs on cells
on the edge of the partition (both owned and annexed) are always
correctly computed. Note that the outermost halo dofs are not
correctly computed and therefore the outermost halo of the modified
field is dirty after redundant computation. Also note that if we do
not know whether a modified field is discontinuous or continuous then
must assume it is continuous.

An alternative solution could have been adopted in dynamo0.3 where no
redundant computation is performed and partial-sum results
shared between processors in a communication pattern similar to halo
exchanges, but a decision was made to always perform redundant
computation.

A downside of performing redundant computation in the level 1 halo is
that any fields being read by the kernel must have their level 1 halo
clean, which can result in halo exchanges. Note that this is not the
case for the modified field, it does not need its halo to be clean,
however, at the moment a halo exchange is added in this case. This
unecessary halo exchange will be removed in a future release.

As it is guaranteed that annexed dofs are correctly computed for
kernels that are written to iterate over cells, and modify a
continuous field, it therefore means that when we also always
redundantly compute annexed dofs (via the configuration switch
described earlier) then we know that annexed dofs are always kept up
to date. If annexed dofs are always kept up to date that we never need
a halo exhange when annexed dofs are computed. Therefore redundantly
computing annexed dofs when iterating over dofs does not result in any
additional halo exchanges. This is one of the reasons for using a
switch rather than supporting a redundant computation transformation
for annexed dofs.

Cell iterators: Discontinuous
-----------------------------

When a kernel is written to iterate over cells and modify a
discontinuous field, PSyclone only needs to compute dofs on owned
cells. Users can apply a redundant computation transformation to
redundantly compute into the halo but this is not done by default.

When a loop iterates over owned cells then any fields that are read by
that loop also only need to access values from owned cells (ignoring
stencil accesses). If, however, one of the fields that is read is
continuous then its annexed dofs must be correct before the kernel is
called.

Whilst loops that iterate over cells always ensure that annexed dofs are
correct, loops that iterate over dofs do not if the
`COMPUTE_ANNEXED_DOFS` configuration flag is set to `False`. In this
case, if the previous update to the continuous field is known to be
from a loop over dofs, then the annexed dofs must be updated. Also, if
the previous update to the continuous field is unknown (as it is from
a previous invoke) then we have to assume that the annexed dofs might
be dirty and therefore have to be updated.

Currently, the only way to update annexed dofs is to perform a depth 1
halo exchange. This halo exchange will update both level 1 halo dofs
and annexed dofs. We therefore update more than we need to.

Of course, if the continuous field with unknown status has its level 1
halo clean due to previous redundant computation then no halo exchange
will be performed. However, if the previous halo exchange has its
annexed dofs clean but the halo dofs dirty then a halo exchange will
be performed despite it not being required for correctness as the run
time dirty flags only maintain information for cell halos.

In contrast, if the `COMPUTE_ANNEXED_DOFS` configuration flag is set
to `True` then the annexed dofs are guaranteed to be up-to-date and
therefore halo exchanges are not required in the cases described
above. This is another reason for using a configuration switch rather
than selective transformations.

Halo Exchange Logic
-------------------

Halo exchanges are required when the `DISTRIBUTED_MEMORY` flag is set to
`True` in order to make sure any accesses to a field's halo or to its
annexed dofs receive the correct value.

Operators and Halo Exchanges
++++++++++++++++++++++++++++

Halo Exchanges are only created for fields. This causes an issue for
operators. If a loop iterates over halos to a given depth and the loop
includes a kernel that reads from an operator then the operator must
have valid values in the halos to that depth. In the current
implementation of PSyclone all loops which write to, or update an
operator are computed redundantly in the halo up to depth 1 (see the
load() method in the DynLoop class). This implementation therefore
requires a check that any loop which includes a kernel that reads from
an operator is limited to iterating in the halo up to
depth 1. PSyclone will raise an exception if an optimisation attempts
to increase the iteration space beyond this (see the gen_code() method
in the DynKern class).

To alleviate the above restriction one could add a configurable depth with
which to compute operators e.g. operators are always computed up to
depth 2, or perhaps up to the maximum halo depth. An alternative would
be to halo exchange operators as required in the same way that halo
exchanges are used for field.

First Creation
++++++++++++++

When first run, PSyclone creates a separate schedule for each of the
invokes found in the algorithm layer. A schedule includes all required
loops and kernel calls that need to be generated in the PSy layer for
the particular invoke call. Once the loops and kernel calls have been
created then (if the DISTRIBUTED_MEMORY flag is set to True) PSyclone
adds any required halo exchanges and global sums. This work is all
performed in the DynInvoke constructor (__init__) method.

In PSyclone we apply a lazy halo exchange approach (as opposed to an
eager one) adding a halo exchange just before it is required.

It is simple to determine where halo exchanges should be added for the
initial schedule. There are two cases:

1) loops that iterate over cells and modify a continuous field will
access the level 1 halo. This means that any field that is read within
such a loop must have its level 1 halo clean and therefore requires a
halo exchange. Note, at the moment PSyclone adds a halo exchange for
the modified field (as it is specified as GH_INC which requires a read
before a write), however this is definitely not required if there is
only one field updated in the kernel.

2) fields that have a stencil access will access the halo and need halo
   exchange calls added.

Halo exchanges are created separately (for fields that read their
halo) for each loop by calling the create_halo_exchanges() method
within the DynLoop class.

In the situation where a field reads from its halo in more than one
kernel in different loops we do not want to add too many halo
exchanges, one will be enough as long as it is placed correctly. To
avoid this problem we add halo exchange calls for loops in schedule
order (first loop to last). A halo exchange will be added before the
first loop for a field but the same field in the second loop will find
that there is a dependence on the previously inserted halo exchange so
no additional halo exchange will be added.

The algorithm iterates over loops in schedule order. The
create_halo_exchanges() method then iterates over each field that
reads from its halo (determined by the unique_fields_with_halo_reads()
method in the DynLoop class).

For each field we then look for its previous dependencies (the
previous writer(s) to that field) using PSyclone's dependence
analysis. Three cases can occur, 1: there is no dependence, 2: there
are multiple dependencies and 3: there is one dependence.

1) If no previous dependence is found then we add a halo exchange call
   before the loop (using the internal helper method
   _add_halo_exchange()). If the field is a vector field then a halo
   exchange is added for each vector. The internal helper method
   _add_halo_exchange itself uses the internal helper method
   _add_halo_exchange_code(). This method creates an instance of the
   DynHaloExchange class for the field in question and adds it to the
   schedule before the loop. You might notice that this method then
   checks that the halo exchange is actually required and removes it
   again if not. In our current situation the halo exchange will
   always be needed so this check is not required but in more complex
   situations after transformations have been applied to the schedule
   this may not be the case. This case is found later.

2) If multiple previous dependencies are found then the field must be
   a vector field as this is the only case where this can occur. We
   then choose the closest one and treat it as a single previous
   dependency (see 3)

3) If a single previous dependency is found and it is a halo exchange
   then we do nothing, as it is already covered. In our case this will
   only happen when more than one reader depends on a writer, as
   discussed earlier. If the dependence is not a halo exchange then we
   add one.

After completing the above we have all the halo exchanges required for
correct execution.

Note that we do not need to worry about halo depth or whether a halo
is definitely required, or whether it might be required, as this is
determined by the halo exchange itself at code generation time. The
reason for deferring this information is that it can change as
transformations are added.

Modifying the Schedule
----------------------

Transformations modify the schedule. At the moment only one of these
transformations - the `Dynamo0p3RedundantComputationTrans` class in
`transformations.py` - affects halo exchanges. This transformation can
mean there is a requirement for new halo exchanges, it can mean
existing halo exchanges are no longer required and it can mean that
the properties of a halo exchange (e.g. depth) can change.

The redundant computation transformation is applied to a loop in a
schedule. When this is done the `update_halo_exchanges()` method for
that loop is called - see the `apply()` method in
`Dynamo0p3RedundantComputationTrans`.

The first thing that the `update_halo_exchanges()` method does is call
the `create_halo_exchanges()` method to add in any new halo exchanges
that are required before this loop, due to any fields that now read
their halo when they did not previously. For example a loop might
previously have iterated up to `ncells` but now iterates up to halo
depth 1. However, a field reading into its halo no longer guarantees
that a halo exchange is required as the previous dependence may now
compute redundantly to depth 2, for example. The solution employed in
`create_halo_exchanges()` is to add a halo exchange speculatively and
then remove it if it is not required. The halo exchange itself
determines whether it is required or not via `required()` method. The
removal code is found in the `_add_halo_exchange_code()` method in the
`DynLoop()` class.

The second thing that the `update_halo_exchanges()` method does is check
that any halo exchanges after this loop are still required. It finds
all relevant halo exchanges, asks them if they are required and if
they are not it removes them.

We only need to look at adding halo exchanges before the loop and
removing halo exchanges after the loop as redundant computation can
only increase the depth of halo to which a loop computes so can not
remove existing halo exchanges before a loop (as an increase in depth
will only increase the depth of an existing halo exchange before the
loop) or add existing halo exchanges after a loop (as an increase in
depth will only make it more likely that a halo exchange is no longer
required after the loop).

.. gocean
.. ------
.. 
.. TBD
.. 
.. OpenMP Support
.. --------------
.. 
.. Loop directives are treated as first class entities in the psyGen
.. package. Therefore they can be added to psyGen's high level
.. representation of the fortran code structure in the same way as calls
.. and loops. Obviously it is only valid to add a loop directive outside
.. of a loop.
.. 
.. When adding a call inside a loop the placement of any additional calls
.. or declarations must be specified correctly to ensure that they are
.. placed at the correct location in the hierarchy. To avoid accidentally
.. splitting the loop directive from its loop the start_parent_loop()
.. method can be used. This is available as a method in all fortran
.. generation calls. *We could have placed it in psyGen instead of
.. f2pygen*.  This method returns the location at the top of any loop
.. hierarchy and before any comments immediately before the top level
.. loop.
.. 
.. The OpenMPLoopDirective object needs to know which variables are
.. shared and which are private. In the current implementation default
.. shared is used and private variables are listed. To determine the
.. objects private variables the OpenMP implementation uses its internal
.. xxx_get_private_list() method. This method first finds all loops
.. contained within the directive and adds each loops variable name as a
.. private variable. this method then finds all calls contained within
.. the directive and adds each calls list of private variables, returned
.. with the local_vars() method. Therefore the OpenMPLoopDirective object
.. relies on calls specifying which variables they require being local.
.. 
.. Next ...
.. 
.. Update transformation for colours
.. 
.. OpenMPLoop transformation in transformations.py. 
.. 
.. Create third transformtion which goes over all loops in a schedule and
.. applies the OpenMP loop transformation.
