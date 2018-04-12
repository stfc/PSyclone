Developers guide
****************

Generating API-specific code
============================

This section explains how to create a new API in PSyclone. PSyclone
currently supports the original prototype gungho api, the dynamo
version 0.1 api and the gocean api.

config.py
---------

The names of the supported API's and the default API are specified in
config.py. When adding a new API you must add the name you would like
to use to the SUPPORTEDAPIS list (and change the DEFAULTAPI if
required).

parse.py
--------

The parser reads the algorithm code and associated kernel
metadata.

The parser currently assumes that all API's will use the invoke() API
for the algorithm-to-psy layer but that the content and structure of
the metadata in the kernel code may differ. If the algorithm API
differs, then the parser will need to be refactored. This is beyond
the scope of this document and is currently not considered in the
PSyclone software architecture.

The kernel metadata however, will be different from one API to
another. To parse this kernel-API-specific metadata a
KernelTypeFactory is provided which should return the appropriate
KernelType object. When adding a new API a new API-specific subclass
of KernelType should be created and added to the create() method in
the KernelTypeFactory class. If the kernel metadata happens to be the
same as another existing API then the existing KernelType subclass can
be used for the new API.

The KernelType subclass needs to specialise the KernelType __init__ method and
initialise the KernelType base class with the supplied arguments. The role of the
KernelType subclass is to create a kernel-metadata-specific subclass of the
Descriptor class and populate this with the relevant API-specific
metadata. After doing this is appends the kernel-metadata-specific
subclass instance to the _arg_descriptors list provided by the
KernelType base class. This information

KernelType base class assumes kernel metadata stored as a type. Searches for that type.
Checks whether the metadata is public (it should be ?)
Assumes iterates_over variable.
Binding to a procedure - assumes one of two styles.
Assumes a meta_args type
*What about our func_args type???*

type x
meta_args=
*meta_func=*
iterates_over=
code => or code =
end type x

The descriptor class ...

psyGen.py
---------

factory
+++++++

A new file needs to be created and the following classes found in
psyGen.py need to be subclassed.

PSy, Invokes, Invoke, Schedule, Loop, Kern, Arguments, Argument
You may also choose to subclass the Inf class if required.

The subclass of the PSy class then needs to be added as an option to
the create method in the PSyFactory class.

Initialisation
++++++++++++++

The parser information passed to the PSy layer is used to create an
invokes object which in turn creates a list of invoke objects. Each
invoke object contains a schedule and a schedule consists of loops and
calls. Finally, a call contains an arguments object which itself
contains a list of argument objects.

To make sure the subclass versions of the above objects are created
the __init__ methods of the subclasses must make sure they create
the appropriate objects.

Some of the baseclass constructors (__init__ methods) support the
classname being provided. This allow them to instantiate the
appropriate objects without knowing what they are.

gen_code()
++++++++++

All of the above classes (with the exception of PSy which supports a
gen() method) have the gen_code() method. This method passes the
parent of the generation tree and expect the object to add the code
associated with the object as a child of the parent. The object is
then expected to call any children. This approach is powerful as it
lets each object concentrate on the code that it is responsible for.

Adding code in gen_code()
+++++++++++++++++++++++++

The f2pygen classes have been developed to help create appropriate
fortran code in the gen_code() method.

When writing a gen_code() method for a particular object and API it is
natural to add code as a child of the parent provided by the callee of
the method. However, in some cases we do not want code to appear at
the current position in the hierarchy.

The add() method
++++++++++++++++

PSyclone supports this via the add() method

explicitly place at the appropriate place in the hierarchy. For example,
parent.parent.add(...)

optional argument. default is auto. This attempts to place code in the
expected place. For example, specify a declaration. auto finds a
correct place to put this code.

Specify position explicitly
"before", "after", "first", "last"

Sometimes don't know exactly where to place. On example that is
supported is when you want to add something before or after a loop
nest. start_parent_loop(). This method recurses up until the parent is
not a loop, it then skips any comments (as they may be directives) and
return this position. Therefore supports an arbitrary number of loops
and directives.

dynamo0p3
=========

Halo's And Annexed DOFs
-----------------------

TBD

When the distributed memory option is switched on in the dynamo0.3 api
(see the :ref:`distributed_memory` Section) a design choice was made
to always compute redundantly into the level 1 halo when a kernel that
iterates over cells updates a continuous field. The reason for this is
to ensure that shared degrees of freedom (dofs) at the boundary
between processors have their values computed correctly. This means
that all dofs associated with owned cells end up having correct values
on all processors.


unlike when iterating over cells, this results in any 'annexed' dofs being dirty (having potentially incorrect old values).

in certain cases this approoach results in halo exchange calls to 

When the distributed memory is switched on in the dynamo0.3 api (See section xxx) the columns of Finite elements (called cells from now on) in the model are partitioned amongst processors and halo cells are added at the boundaries. For example, if we a halo of depth 1 in 



x-x-x   x-x-x
1 2 3   2 3 4

The DoFs on the cells may be discontinuous or continuous. If discontinuous then DoFs are local to cells so can be numbered separately. For example

pic : xxx

If continuous then replicated at the boundary.
Choose one as owner (typically lowest element ID) and the other DoFs are called "annexed".

pic : xxx

A design choice in dynamo0.3 api is that when iterating over cells we always compute always commputes annexed dofs due to redundant computation. Design choice in the dynamo0.3 api.

As discussed in section xxx may iterate over dofs. For correctness only need to iterate over owned dofs. However, if this is done then annexed dofs are dirty and if they are required then the only way to make them clean is by performing a halo exchange.

Example of this is dofs loop then cells loop with a continuous read.

Second example is when unknown writer, need one just in case.



Loop iterators and bounds
-------------------------

TBC

In the current implementation it is possible to iterate (loop) over
cells or over dofs. At the moment all coded kernels iterate over cells
and all builtin kernels iterate over dofs, but that does not have to
be the case. This is determined from the kernel within the loop which
specifies what it is written to iterate over.

Iterating over dofs computes all local dofs. Applying redundant
computation can specify computing over different halo depths, or to
the maximum halo depth. Currently there is no way to compute local
dofs + annexed dofs. To compute annexed dofs you must compute in the
halo i.e. the level 1 halo exchange includes annexed dofs.

continuous, always compute to level 1 halo. After doing this the level
1 halo is left dirty but any dofs on edges, including annexed dofs,
are correct. Therefore we never need worry about annexed dofs unless
previously iterating over DOFs. Actually we compute the partial sum
and could update this instead.


Halo Exchange Logic
-------------------

Halo exchanges are required when the DISTRIBUTED_MEMORY flag is set to
True in order to make sure any accesses to a field's halo or to its
annexed DOFs receive the correct value.

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
transformations - the Dynamo0p3RedundantComputationTrans class in
transformations.py - affects halo exchanges. This transformation can
mean there is a requirement for new halo exchanges, it can mean
existing halo exchanges are no longer required and it can mean that
the properties of a halo exchange (e.g. depth) can change.

The redundant computation transformation is applied to a loop in a
schedule. When this is done the update_halo_exchanges() method for
that loop is called - see the apply() method in
Dynamo0p3RedundantComputationTrans.

The first thing that the update_halo_exchanges() method does is call
the create_halo_exchanges() method to add in any new halo exchanges
that are required before this loop, due to any fields that now read
their halo when they did not previously. For example a loop might
previously have iterated up to ncells but now iterates up to halo
depth 1. However, a field reading into its halo no longer guarantees
that a halo exchange is required as the previous dependence may now
compute redundantly to depth 2, for example. The solution employed in
create_halo_exchanges() is to add a halo exchange speculatively and
then remove it if it is not required. The halo exchange itself
determines whether it is required or not via required() method. The
removal code is found in the _add_halo_exchange_code() method in the
DynLoop() class.

The second thing that the update_halo_exchanges() method does is check
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

gocean
------

TBD

OpenMP Support
--------------

Loop directives are treated as first class entities in the psyGen
package. Therefore they can be added to psyGen's high level
representation of the fortran code structure in the same way as calls
and loops. Obviously it is only valid to add a loop directive outside
of a loop.

When adding a call inside a loop the placement of any additional calls
or declarations must be specified correctly to ensure that they are
placed at the correct location in the hierarchy. To avoid accidentally
splitting the loop directive from its loop the start_parent_loop()
method can be used. This is available as a method in all fortran
generation calls. *We could have placed it in psyGen instead of
f2pygen*.  This method returns the location at the top of any loop
hierarchy and before any comments immediately before the top level
loop.

The OpenMPLoopDirective object needs to know which variables are
shared and which are private. In the current implementation default
shared is used and private variables are listed. To determine the
objects private variables the OpenMP implementation uses its internal
_get_private_list() method. This method first finds all loops
contained within the directive and adds each loops variable name as a
private variable. this method then finds all calls contained within
the directive and adds each calls list of private variables, returned
with the local_vars() method. Therefore the OpenMPLoopDirective object
relies on calls specifying which variables they require being local.

Next ...

Update transformation for colours

OpenMPLoop transformation in transformations.py. 

Create third transformtion which goes over all loops in a schedule and
applies the OpenMP loop transformation.
