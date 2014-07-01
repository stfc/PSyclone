Developers guide
****************

Generating api specific code.
=============================

This section explains how to create a new API in PSyclone. PSyclone
currently supports the original prototype gungho api, the dynamo
version 0.1 api and the gocean api.

config.py
---------

The names of the supported API's and the default API are specified in
config.py. When adding a new API you must add the name you would like
to use to the SUPPORTEDAPIS list (and change the DEFAULTAPI if
required).



Parse.py
--------

The parser reads the algorithm code and associated kernel
metadata.

The parser currently assumes that all API's will use the algorithm
invoke() API but that the content and structure of the metadata in the
kernel code may differ. Assuming this is the case then a new subclass
of the KernelType class needs to be created and added to the create
method in the KernelTypeFactory class. If the metadata is the same as
another existing version then that KernelType subclass can be used for
the new API.

If the algorithm API differs, then the parser will need to be
refactored. This is beyond the scope of this document and is not part
of the PSyclone design.

The KernelType subclass needs to specialise the __init__ method,
initialise the base class with the supplied arguments and populate the
_arg_descriptors list with a list of an API specific class which
subclasses the Descriptor class.

* More details required here *

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

explicitely place at the appropriate place in the hierarchy. For example,
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

GOceanProto
-----------

Not being actively developed so not described here. Can look at source code.

dynamo
------

TBD

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
generation calls. ** We could have placed it in psyGen instead of
f2pygen **.  This method returns the location at the top of any loop
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
