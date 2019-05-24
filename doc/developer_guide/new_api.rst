.. -----------------------------------------------------------------------------
   BSD 3-Clause License

   Copyright (c) 2017-2019, Science and Technology Facilities Council.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

   * Neither the name of the copyright holder nor the names of its
     contributors may be used to endorse or promote products derived from
     this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
   -----------------------------------------------------------------------------
   Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab

.. highlight:: python

Supporting a new API
####################

PSyclone has been designed to support multiple APIs and to allow new
APIs to be added. This section explains how to support a new, or
updated, API in PSyclone.

Support can be split into two main parts. First, the parser code
needs to be extended to allow it to parse the new kernel metadata
structure. Second, the PSy generation code needs to be extended so
that appropriate PSy-layer code is generated to conform to the new API
when making use of the new kernel metadata.

The parser code extensions and PSy-generation code extensions are
treated in the sections below.

Modifying the parser code
=========================

parser code is contained in the parser submodule of psyclone and
itself contains two submodules, `kernel.py
<https://github.com/stfc/PSyclone/blob/master/src/psyclone/parse/kernel.py>`_
and `algorithm.py
<https://github.com/stfc/PSyclone/blob/master/src/psyclone/parse/algorithm.py>`_.
The latter takes an algorithm code as input. It parses the algorithm
code and finds and parses any kernels that are referenced by the
algorithm code. It returns the parsed algorithm code as an ast and an
object containing all the required algorithm invocation information
and its associated kernel information. The parser can be run in the
following way::

    from parse import parse
    ast, info = parse("example.F90")

The parser currently assumes that all APIs will use the standard
invoke() approach within the algorithm layer but that the content
and structure of the metadata in the kernel code may differ.

If the algorithm API differs from this expectation, then the parser
will need to be refactored. Any such refactoring is beyond the scope
of this document and is currently not part of the PSyclone software
design.

To add support for a new API, three classes need to be modified and/or
created in the :py:mod:`psyclone.parse` module in parse/kernel.py <https://github.com/stfc/PSyclone/blob/master/src/psyclone/parse/kernel.py>_
and parse/algorithm.py <https://github.com/stfc/PSyclone/blob/master/src/psyclone/parse/algorithm.py>_. The
:class:KernelTypeFactory class needs to be modified, a new subclass
of the :class:`psyclone.parse.kernel.KernelType` class needs to be created and a new
subclass of the :class:`psyclone.parse.kernel.Descriptor` class needs to be created. The
modifications and/or additions for each class are detailed in the
following three sections.

Modifying the :py:class:`psyclone.psyGen.KernelTypeFactory` Class
-----------------------------------------------------------------

Kernel metadata, is likely to be different from one API to another. To
parse this kernel-API-specific metadata a :class:KernelTypeFactory is
provided which is responsible for returning the appropriate
KernelType object.

.. automodule:: psyclone.parse.kernel
.. autoclass:: psyclone.parse.kernel.KernelTypeFactory
:members:

For example, assuming we want a new :class:KernelType for the 0.3 dynamo
API which we decided to call :class:DynKernelType03 we would modify the
create method of :class:KernelTypeFactory as shown below::

    def create(self,name,ast):
    if self._type=="gunghoproto":
    return GHProtoKernelType(name,ast)
    elif self._type=="dynamo0.1":
    return DynKernelType(name,ast)
    elif self._type=="gocean":
    return GOKernelType(name,ast)
    elif self._type=="dynamo0.3":
    return DynKernelType03(name,ast)
    else:
    raise ParseError("KernelTypeFactory: Internal Error: Unsupported kernel type '{0}' found.
    Should not be possible.".format(self._type))

If the Kernel metadata happens to be the same as another existing API
then the existing :class:`psyclone.parse.kernel.KernelType` subclass
can be used for the new API.

For example, assuming the 0.3 Dynamo API uses the same metadata as the
0.1 Dynamo API we could modify the create method of
:class:`psyclone.parse.kernel.KernelTypeFactory` in the following way, leading to the
:class:`psyclone.parse.kernel.DynKernelType` object being returned for both the dynamo0.1 and
dynamo0.3 APIs.::

    def create(self,name,ast):
    if self._type=="gunghoproto":
    return GHProtoKernelType(name,ast)
    elif self._type in ["dynamo0.1","dynamo0.3"]:
    return DynKernelType(name,ast)
    elif self._type=="gocean":
    return GOKernelType(name,ast)
    else:
    raise ParseError("KernelTypeFactory: Internal Error: Unsupported kernel type '{0}' found.
    Should not be possible.".format(self._type))

Sub-classing the :class:`psyclone.parse.kernel.KernelType` Class
----------------------------------------------------------------

The role of the API-specific :class:KernelType subclass is to capture
all the required Kernel metadata and code for a Kernel using a
particular API. This information will be used by :mod:psyGen to
generate the PSy-layer code and by :mod:AlgGen to modify the
algorithm-layer code.

.. automodule:: parse
.. autoclass:: KernelType
:members:

The :class:KernelType class makes the assumption that Kernel metadata
will be stored within a Fortran module (which contains the Kernel
code) as a Fortran type with a defined structure.

Required structure
++++++++++++++++++

Below outlines the required structure of the Fortran type from the
KernelType class' perspective. XML-style brackets (<.../>) are used
where information is allowed to vary from one API to another.
::

    type, public :: typename
    type() :: meta_args(nargs) = (/ &
    (,,...,), &
    (,,...,), &
    ... &
    /)
    integer :: iterates_over =
    contains
    procedure :: kernelname
    end type

Therefore a public type is expected. The name of the type is used by
the algorithm layer to reference the kernel from within an invoke()
call. The public type contains a meta_args array of types (one for
each field passed to the Kernel). The meta_args types are each
initialised via a structure constructor, with the content of the
structure constructor being API-specific. The type also contains the
integer iterates_over which is set to an API-specific value. Finally
the type also contains the name of the kernel code procedure.

An Example
++++++++++

As an example, consider an algorithm code which specifies it wants to
the rhs_v3_code kernel code by including an invoke() call and
referencing v3_kernel_type from within it.
::

    ...
    use v3_kernel_mod, only: v3_kernel_type
    ...
    call invoke (v3_kernel_type(rhs) )
    ...

The associated rhs_v3_code kernel metadata (for the dynamo0.1 API)
looks like the following::

    type, public, extends(kernel_type) :: v3_kernel_type
    private
    type(arg_type) :: meta_args(1) = (/ &
    arg_type(gh_rw,v3,fe,.true.,.false.,.true.) &
    /)
    integer :: iterates_over = cells
    contains
    procedure, nopass :: rhs_v3_code
    end type

In the above example, the structure of the metadata conforms to
the required format specified earlier. However the
vocabulary and ammount of metadata is specific to the dynamo0.1 API.

Specifically, the type of meta_args is "arg_type", iterates_over has
the name "cells", and the single field passed into the Kernel from the
Algorithm layer has the following list of metadata describing it
"gh_rw,v3,fe,.true.,.false.,.true.".

Conforming to the Required Structure
++++++++++++++++++++++++++++++++++++

If the new API provides metadata in the above format then the
:class:KernelType sub-class can use the :class:KernelType class to
extract the metadata. To do this the the :class:KernelType sub-class needs
to specialise the :class:KernelType init method and initialise the
:class:KernelType class with the supplied arguments. As an illustration,
the required code for the Dynamo0.3 API sub-class is shown below.
::

    class DynKernelType03(KernelType):
    def init(self,name,ast):
    KernelType.init(self,name,ast)

At this point, the sub-class (:class:DynKernelType03) provides access to the
following information:

    the name of Fortran type containing the metadata via the name variable
    the iterates_over metadata value for this kernel via the
    iterates_over variable,
    the ast of the kernel code via the procedure variable.

However, the :class:KernelType class does not know anything about the
content and structure of the metadata for each argument as this
information is API-specific. For example, we have the
following information "gh_rw,v3,fe,.true.,.false.,.true." for the
first (and in this case only) argument in the earlier example.

Therefore, the :class:KernelType class provides the metadata arguments
as a set of tokens (using the expression class) in its internal _inits
variable and an (empty) internal _arg_descriptors list which it
expects the sub-class to fill with a processed list.

The :class:Descriptor class (see next section) is provided to help with
this task. The :class:Descriptor class can be specialised to process
the API-specific metadata for each argument.

Extending our earlier :class:DynKernelType03 0.3 Dynamo API example, we now
make use of an :class:DynArgDescriptor03 sub-class of the :class:Descriptor
class to capture the API-specific metadata about each argument::

    class DynKernelType03(KernelType):
        def init(self,name,ast):
            KernelType.init(self,name,ast)

            self._arg_descriptors=[]
            for arg_type in self._inits:
                self._arg_descriptors.append(DynArgDescriptor03(arg_type))

In the above case we simply pass the raw argument information directly
into the :class:DynArgDescriptor03 sub-class for processing and add it
to the internal _arg_descriptors list as expected. Where an API has
a fixed number of arguments, the metadata could be extracted before
being passed to the associated sub-class. For example, in the dynamo
0.1 API we first extract the access, funcspace, stencil, x1, x2 and
x3 metadata then pass these into our :class:DynDescriptor sub-class of
the :class:Descriptor class.
::

    class DynKernelType(KernelType):
        def init(self,name,ast):
            KernelType.init(self,name,ast)

            self._arg_descriptors=[]
            for init in self._inits:
                access=init.args[0].name
                funcspace=init.args[1].name
                stencil=init.args[2].name
                x1=init.args[3].name
                x2=init.args[4].name
                x3=init.args[5].name
                self._arg_descriptors.append(DynDescriptor(access,funcspace,stencil,x1,x2,x3))

At this point (assuming the sub-class of the :class:Descriptor
class has been created, see the next section), the sub-class
(:class:DynKernelType03) provides access to the following variables:

    name : the name of Fortran type containing the metadata.
    iterates_over : the "iterates_over" metadata value for this kernel.
    procedure : the ast of the kernel code.
    nargs : the number of arguments specified in the metadata
    arg_descriptors : a list of metadata for each argument

Finally, the :class:KernelType sub-class is also responsible for
checking the validity of the metadata values held by the
sub-class. For example, the sub-class should check whether the
value of iterates_over is recognised by the API and raise a
ParseError error if not.

Extending the Required Structure
++++++++++++++++++++++++++++++++

For simplicitity it is recommended that new APIs try to conform to
the supported structure. However, in some cases additional information
may be required.

.. note::
Not yet on the trunk
In the dynamo0.3 API, for example, two different types of metadata
are required when dealing with the description of dynamo fields;
these are metadata associated with arguments and metadata
associated with function space. In this case there is a meta_args
variable, which is supported, and a separate meta_funcs variable
which is not supported. As the structure of the function space is
similar to the structure of the arguments metadata we can use the
same baseclass method - getkerneldescriptors() - (see the code
for more details) with the optional var_name being set to the
appropriate name to parse the metadata.

Supporting a non-conformant Structure
-------------------------------------

.. warning::
TBD: what hierarchy is expected/required?

:class:Descriptor Class
++++++++++++++++

The role of the :class:descriptor class is to store the information
in each of the metadata entries in the meta_args (or equivalent)
array. It should also be used to check whether the content, structure
and order of the arrays are valid.

.. automodule:: parse
.. autoclass:: Descriptor
:members:

The base class provides variables for 3 types of metadata, "access
type", "function space" and "stencil type".

If the base class provides all the metadata required by the API then
it may be used directly. If not, an api-specific version of this class
should be created.

Finally, the :class:Descriptor sub-class is also responsible for
checking the validity of the metadata values held by the
sub-class. For example, the sub-class should check whether the
number of metadata arguments match what is expected by the API. If
this is not the case then the code return a ParseError.

Test
++++

At this point you should be able to successfully parse the kernel
metadata provided by the new API. However, at this point it may be
difficult to know whether the data you are capturing is correct or
not.

One option is to provide a string str() method to the
:class:descriptor sub-class and then print the object when it is
created, or print them all of the objects at a later date.
psyGen.py

UP TO HERE, UP TO HERE

psyGen.py contains the code to generate the PSy layer from the
metadata provided by the parser.

In order to support the generation of PSy code for a new api in
PSyclone a new file needs to be created and the following classes
found in psyGen.py need to be subclassed within it:

PSy, Invokes, Invoke, Schedule, Loop, Kern, Arguments, Argument

You may also need to subclass the Inf class depending on the api.

If there is already a similar API available a simple way to start
would be to make a copy of the associated file, renaming it
appropriately.

PSyFactory Class
++++++++++++++++

Once the subclass of the PSy class has been created, it should be
added as an option to the create() method in the PSyFactory class
within psyGen.py.

Class Initialisation
++++++++++++++++++++

The parser information passed to the PSy layer is used to create an
invokes object which in turn creates a list of invoke objects. Each
invoke object contains a schedule and a schedule consists of loops and
calls. Finally, a call contains an arguments object which itself
contains a list of argument objects.

To make sure the subclass versions of the above objects are created
the init methods of the subclasses must make sure they create
the appropriate objects.

Some of the baseclass constructors (init methods) support the
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

Adding code in gen_code() : f2pygen
+++++++++++++++++++++++++++++++++++

The f2pygen classes have been developed to help create appropriate
fortran code in the gen_code() method.

When writing a gen_code() method for a particular object and API it is
natural to add code as a child of the parent provided by the callee of
the method. However, in some cases we do not want code to appear at
the current position in the hierarchy.

The add() method
++++++++++++++++

PSyclone supports the addition of code to an ast via the add() method

explicitely place at the appropriate place in the hierarchy. For example,
parent.parent.add(...)

optional argument. default is auto. This attempts to place code in the
expected place. For example, specify a declaration. auto finds a
correct place to put this code.

Specify position explicitly "before", "after", "first", "last"

Sometimes don't know exactly where to place. On example that is
supported is when you want to add something before or after a loop
nest. start_parent_loop(). This method recurses up until the parent is
not a loop, it then skips any comments (as they may be directives) and
return this position. Therefore supports an arbitrary number of loops
and directives.
.. OpenMP Support

Loop directives are treated as first class entities in the psyGen
package. Therefore they can be added to psyGen's high level
representation of the fortran code structure in the same way as calls
and loops. Obviously it is only valid to add a loop directive outside
of a loop.

.. When adding a call inside a loop the placement of any additional calls
or declarations must be specified correctly to ensure that they are
placed at the correct location in the hierarchy. To avoid accidentally
splitting the loop directive from its loop the start_parent_loop()
method can be used. This is available as a method in all fortran
generation calls. ** We could have placed it in psyGen instead of
f2pygen **. This method returns the location at the top of any loop
hierarchy and before any comments immediately before the top level
loop.

.. The OpenMPLoopDirective object needs to know which variables are
shared and which are private. In the current implementation default
shared is used and private variables are listed. To determine the
objects private variables the OpenMP implementation uses its internal
_get_private_list() method. This method first finds all loops
contained within the directive and adds each loops variable name as a
private variable. this method then finds all calls contained within
the directive and adds each calls list of private variables, returned
with the local_vars() method. Therefore the OpenMPLoopDirective object
relies on calls specifying which variables they require being local.
