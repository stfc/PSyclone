.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2020, Science and Technology Facilities Council.
.. All rights reserved.
..
.. Redistribution and use in source and binary forms, with or without
.. modification, are permitted provided that the following conditions are met:
..
.. * Redistributions of source code must retain the above copyright notice, this
..   list of conditions and the following disclaimer.
..
.. * Redistributions in binary form must reproduce the above copyright notice,
..   this list of conditions and the following disclaimer in the documentation
..   and/or other materials provided with the distribution.
..
.. * Neither the name of the copyright holder nor the names of its
..   contributors may be used to endorse or promote products derived from
..   this software without specific prior written permission.
..
.. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
.. "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
.. LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
.. FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
.. COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
.. INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
.. BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
.. LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
.. CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.. LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
.. ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
.. POSSIBILITY OF SUCH DAMAGE.
.. -----------------------------------------------------------------------------
.. Written by R. W. Ford, A. R. Porter and S. Siso STFC Daresbury Lab

The PSyclone Internal Representation (PSyIR)
############################################

The PSyclone Internal Representation (PSyIR) is a language-independent
AST that PSyclone uses to represent the PSy layer and the kernel
code. The PSyIR can be constructed from scratch or produced from
existing code using one of the front-ends (Readers) and it can be
transformed back to a particular language using the back-ends (Writers)
provided in PSyclone.

Nodes
=====

All nodes in the AST are sub-classes of the abstract `Node` base class, which
provides the following common interface:

.. autoclass:: psyclone.psyir.nodes.Node
    :members:

.. _container-label:

Container
=========

The Container node contains one or more Containers and/or
KernelSchedules (see :ref:`kernel_schedule-label`). Similarly to
KernelSchedule it contains a SymbolTable
(`psyclone.psyGen.psyir.symbols.SymbolTable`) that keeps a record of
the Symbols (`psyclone.psyGen.psyir.symbols.Symbol`) specified in the
Container scope (see :ref:`user_guide:symbol-label`).

A Container can be used to capture a hierarchical grouping of
KernelSchedules and a hierarchy of Symbol scopes i.e. a Symbol
specified in a Container is visible to all Containers and
KernelSchedules within it and their descendents.

.. autoclass:: psyclone.psyir.nodes.Container
    :members:

Schedule
========

The Schedule node represents a sequence of statements. It is an important node
in PSyclone because two of its specialisations: `InvokeSchedule` and
`KernelSchedule` (described below), are used as the root nodes of PSy-layer
invokes and kernel subroutines. This makes them the starting points for any
walking of the PSyIR tree in PSyclone transformation scripts and a common
target for the application of transformations.

.. autoclass:: psyclone.psyir.nodes.Schedule
    :members:


InvokeSchedule
--------------

The `InvokeSchedule` is a PSyIR node that represents an invoke subroutine in
the PSy-layer. It extends the `psyclone.psyir.nodes.Schedule` functionality
with a `psyclone.psyir.symbols.SymbolTable` and a reference to its associated
`psyclone.psyGen.Invoke` object.

.. autoclass:: psyclone.psyGen.InvokeSchedule
    :members:

.. _kernel_schedule-label:

KernelSchedule
---------------

The `KernelSchedule` is a PSyIR node that represents a kernel
subroutine. It extends the `psyclone.psyir.nodes.Schedule` functionality
with a SymbolTable (`psyclone.psyGen.psyir.symbols.SymbolTable`) that
keeps a record of the Symbols (`psyclone.psyGen.psyir.symbols.Symbol`)
used in the kernel scope (see :ref:`user_guide:symbol-label`).


Control Flow Nodes
==================

The PSyIR has two control flow nodes: `IfBlock` and `Loop`. These
nodes represent the canonical structure with which conditional
branching constructs and iteration constructs are built. Additional
language-specific syntax for branching and iteration will be
normalized to use these same constructs.  For example, Fortran has the
additional branching constructs `ELSE IF` and `CASE`: when a Fortran
code is translated into the PSyIR, PSyclone will build a semantically
equivalent implementation using `IfBlocks`.  Similarly, Fortran also
has the `WHERE` construct and statement which are represented in the
PSyIR with a combination of `Loop` and `IfBlock` nodes. Such nodes in
the new tree structure are annotated with information to enable the
original language-specific syntax to be recreated if required (see
below).

Node annotation
---------------

If the PSyIR is constructed from existing code (using e.g. the
fparser2 frontend) then it is possible that information about that
code may be lost.  This is because the PSyIR is only semantically
equivalent to certain code constructs. In order that information is
not lost (making it possible to e.g. recover the original code
structure if desired) Nodes may have `annotations` associated with
them. The annotations, the Node types to which they may be applied and
their meanings are summarised in the table below:

=================  =================  =================================
Annotation         Node types         Origin
=================  =================  =================================
`was_elseif`       `IfBlock`          `else if`
`was_single_stmt`  `IfBlock`, `Loop`  `if(logical-expr)expr` or Fortran
                                      `where(array-mask)array-expr`
`was_case`         `IfBlock`          Fortran `select case`
`was_where`        `Loop`, `IfBlock`  Fortran `where` construct
=================  =================  =================================

.. note:: a `Loop` may currently only be given the `was_single_stmt` annotation
	  if it also has the `was_where` annotation. (Thus indicating that
	  this `Loop` originated from a WHERE *statement* in the original
	  Fortran code.) Representing single-statement loops in Fortran is
	  the subject of GitHub Issue
	  `#412 <https://github.com/stfc/PSyclone/issues/412>`_.

Branching construct
-------------------

.. autoclass:: psyclone.psyir.nodes.IfBlock
    :members:


Iteration construct
-------------------

.. autoclass:: psyclone.psyir.nodes.Loop
    :members:

Ranges
======

The PSyIR has the `Range` node which represents a range of integer
values with associated start, stop and step properties. e.g. the list
of values [4, 6, 8, 10] would be represented by a `Range` with a start
value of 4, a stop value of 10 and a step of 2 (all stored as `Literal`
nodes). This class is intended to simplify the construction of Loop nodes
as well as to support array slicing (see below). However, this
functionality is under development and at this stage neither of those
options have been implemented.

The `Range` node must also provide support for array-slicing
constructs where a user may wish to represent either the entire range
of possible index values for a given dimension of an array or a
sub-set thereof. e.g. in the following Fortran::

    real, dimension(10, 5) :: my_array
    call some_routine(my_array(1, :))

the argument to `some_routine` is specified using array syntax where
the lone colon means *every* element in that dimension. In the PSyIR,
this argument would be represented by an `Array` node with the first
entry in its `shape` being an integer `Literal` (with value 1) and the
second entry being a `Range`. In this case the `Range` will have a
start value of `LBOUND(my_array, 1)`, a stop value of
`UBOUND(my_array, 1)` and a step of `Literal("1")`. Note that `LBOUND`
and `UBOUND` are not yet implemented (Issue #651) but will be
instances of `BinaryOperation`. (For the particular code fragment
given above, the values are in fact known [1 and 5, respectively] and
could be obtained by querying the Symbol Table.)

.. autoclass:: psyclone.psyir.nodes.ranges.Range
    :members:

Operation Nodes
===============

Arithmetic operations and various intrinsic/query functions are represented
in the PSyIR by sub-classes of the `Operation` node:

.. autoclass:: psyclone.psyir.nodes.Operation
   :members:

The operations are classified according to the number of operands.
Those having one operand are represented by
`psyclone.psyir.nodes.UnaryOperation` nodes:

.. autoclass:: psyclone.psyir.nodes.UnaryOperation.Operator
   :members:
   :undoc-members:

those having two operands are represented by
`psyclone.psyir.nodes.BinaryOperation` nodes:

.. autoclass:: psyclone.psyir.nodes.BinaryOperation
   :members: Operator

and those having more than two by `psyclone.psyir.nodes.NaryOperation`
nodes:

.. autoclass:: psyclone.psyir.nodes.NaryOperation.Operator
   :members:
   :undoc-members:

Note that where an intrinsic (such as
Fortran's `MAX`) can have a variable number of arguments, the class
used to represent it in the PSyIR is determined by the actual number
of arguments in a particular instance. e.g. `MAX(var1, var2)` would be
represented by a `psyclone.psyir.nodes.BinaryOperation` but `MAX(var1,
var2, var3)` would be represented by a
`psyclone.psyir.nodes.NaryOperation`.

CodeBlock Node
==============

The PSyIR CodeBlock node contains code that has no representation in
the PSyIR. It is useful as it allows the PSyIR to represent complex
code by using CodeBlocks to handle the parts which contain unsupported
language features. One approach would be to work towards capturing all
language features in the PSyIR, which would gradually remove the need
for CodeBlocks. However, the purpose of the PSyIR is to capture code
concepts that are relevant for performance, not all aspects of a code,
therefore it is likely that that CodeBlocks will continue to be an
important part of the PSyIR.

.. autoclass:: psyclone.psyir.nodes.CodeBlock
   :members:
   :undoc-members:

The code represented by a CodeBlock is currently stored as a list of
fparser2 nodes. Therefore, a CodeBlock's input and output language is
limited to being Fortran. This means that only the fparser2 front-end
and Fortran back-end can be used when there are CodeBlocks within a
PSyIR tree. In theory, language interfaces could be written between
CodeBlocks and other PSyIR Nodes to support different back-ends but
this has not been implemented.

The CodeBlock ``structure`` method indicates whether the code contains
one or more Fortran expressions or one or more statements (which may
themselves contain expressions). This is required by the Fortran
back-end as expressions do not need indentation and a newline whereas
statements do.

A feature of the fparser2 node list is that if the first node in the
list is a statement then so are all the other nodes in the list and
that if the first node in the list is an expression then so are all
the other nodes in the list. This allows the ``structure`` method to
return a single value that represents all nodes in the list.

The structure of the PSyIR hierarchy is used to determine whether the
code in a CodeBlock contains expressions or statements. This is
achieved by looking at the parent PSyIR Node. If the parent Node is a
Schedule then the CodeBlock contains one or more statements, otherwise
it contains one or more expressions. This logic works for existing
PSyIR nodes and relies on any future PSyIR nodes being constructed so
this continues to be true. The one exception to this rule is
Directives. Directives currently do not place their children in a
Schedule. As the structure of Directives is under discussion, it was
decided to raise an exception if the parent node of a CodeBlock is a
Directive (for the time being).

Reference Node
==============

The PSyIR Reference Node represents a scalar variable access. It keeps
a reference to a Symbol which (for all APIs except NEMO) will be
stored in a symbol table.

.. autoclass:: psyclone.psyir.nodes.Reference
    :members:

Array Node
==========

The PSyIR Array Node represents an array access. It keeps a reference
to a Symbol which (for all APIs except NEMO) will be stored in a
symbol table and the indices used to access the array. Array Node
inherits from Reference Node.

.. autoclass:: psyclone.psyir.nodes.Array
    :members:

Dependence Analysis
===================

Dependence Analysis in PSyclone produces ordering constraints between
instances of the `Argument` class within a PSyIR.

The `Argument` class is used to specify the data being passed into and
out of instances of the `Kern` class, `HaloExchange` class and
`GlobalSum` class (and their subclasses).

As an illustration consider the following invoke::

   invoke(           &
       kernel1(a,b), &
       kernel2(b,c))

where the metadata for `kernel1` specifies that the 2nd argument is
written to and the metadata for `kernel2` specifies that the 1st
argument is read.

In this case the PSyclone dependence analysis will determine that
there is a flow dependence between the second argument of `Kernel1`
and the first argument of `Kernel2` (a read after a write).

Information about arguments is aggregated to the PSyIR node level
(`kernel1` and `kernel2` in this case) and then on to the parent
`loop` node resulting in a flow dependence (a read after a write)
between a loop containing `kernel1` and a loop containing
`kernel2`. This dependence is used to ensure that a transformation is
not able to move one loop before or after the other in the PSyIR
schedule (as this would cause incorrect results).

Dependence analysis is implemented in PSyclone to support
functionality such as adding and removing halo exchanges,
parallelisation and moving nodes in a PSyIR schedule. Dependencies
between nodes in a PSyIR schedule can be viewed as a DAG using the
`dag()` method within the `Node` base class.

DataAccess Class
----------------

The `DataAccess` class is at the core of PSyclone data dependence
analysis. It takes an instance of the `Argument` class on
initialisation and provides methods to compare this instance with
other instances of the `Argument` class. The class is used to
determine 2 main things, called `overlap` and `covered`.

Overlap
+++++++

`Overlap` specifies whether accesses specified by two instances of the
`Argument` class access the same data or not. If they do access the
same data their accesses are deemed to `overlap`. The best way to
explain the meaning of `overlap` is with an example:

Consider a one dimensional array called `A` of size 4 (`A(4)`). If one
instance of the `Argument` class accessed the first two elements of
array `A` and another instance of the `Argument` class accessed the
last two elements of array `A` then they would both be accessing array
`A` but their accesses would *not* `overlap`. However, if one instance
of the `Argument` class accessed the first three elements of array `A`
and another instance of the `Argument` class accessed the last two
elements of array `A` then their accesses would `overlap` as they are
both accessing element `A(3)`.

Having explained the idea of `overlap` in its general sense, in
practice PSyclone currently assumes that *any* two instances of the
`Argument` class that access data with the same name will always
`overlap` and does no further analysis (apart from halo exchanges and
vectors, which are discussed below). The reason for this is that
nearly all accesses to data, associated with an instance of the
`Argument` class, start at index 1 and end at the number of elements,
dofs or some halo depth. The exceptions to this are halo exchanges,
which only access the halo and boundary conditions, which only access
a subset of the data. However these subset accesses are currently not
captured in metadata so PSyclone must assume subset accesses do not
exist.

If there is a field vector associated with an instance of an
`Argument` class then all of the data in its vector indices are
assumed to be accessed when the argument is part of a `Kern` or a
`GlobalSum`. However, in contrast, a `HaloExchange` only acts on a
single index of a field vector. Therefore there is one halo exchange
per field vector index. For example::

    InvokeSchedule[invoke='invoke_0_testkern_stencil_vector_type', dm=True]
    ... HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
    ... HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
    ... HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
    ... Loop[type='',field_space='w0',it_space='cells', upper_bound='cell_halo(1)']
    ... ... CodedKern testkern_stencil_vector_code(f1,f2) [module_inline=False]

In the above PSyIR schedule, the field `f1` is a vector field and the
`CodedKern` `testkern\_stencil\_vector\_code` is assumed to access data in
all of the vector components. However, there is a separate `HaloExchange`
for each component. This means that halo exchanges accessing the
same field but different components do not `overlap`, but each halo
exchange does overlap with the loop node. The current implementation
of the `overlaps()` method deals with field vectors correctly.

Coverage
++++++++

The concept of `coverage` naturally follows from the discussion in the
previous section.

Again consider a one dimensional array called `A` of size 4
(`A(4)`). If one instance (that we will call the `source`) of the
`Argument` class accessed the first 3 elements of array `A`
(i.e. elements 1 to 3) and another instance of the `Argument` class
accessed the first two elements of array `A` then their accesses would
`overlap` as they are both accessing elements `A(1) and A(2)` and
elements `A(1) and A(2)` would be `covered`. However, access `A(3)`
for the `source Argument` class would not yet be `covered`. If a
subsequent instance of the `Argument` class accessed the 2nd and 3rd
elements of array `A` then all of the accesses (`A(1), A(2) and A(3)`)
would now be `covered` so the `source argument` would be deemed to be
covered.

In PSyclone the above situation occurs when a vector field is accessed
in a kernel and also requires halo exchanges e.g.::

   InvokeSchedule[invoke='invoke_0_testkern_stencil_vector_type', dm=True]
      HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
      HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
      HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
      Loop[type='',field_space='w0',it_space='cells', upper_bound='cell_halo(1)']
         CodedKern testkern_stencil_vector_code(f1,f2) [module_inline=False]

In this case the PSyIR loop node needs to know about all 3 halo
exchanges before its access is fully `covered`. This functionality is
implemented by passing instances of the `Argument` class to the
`DataAccess` class `update_coverage()` method and testing the
access.covered property until it returns `True`.

::

   # this example is for a field vector 'f1' of size 3
   # f1_index[1,2,3] are halo exchange accesses to vector indices [1,2,3] respectively
   access = DataAccess(f1_loop)
   access.update_coverage(f1_index1)
   result = access.covered  # will be False
   access.update_coverage(f1_index2)
   result = access.covered  # will be False
   access.update_coverage(f1_index3)
   result = access.covered  # will be True
   access.reset_coverage()

Note the `reset_coverage()` method can be used to reset internal state
so the instance can be re-used (but this is not used by PSyclone at
the moment).

The way in which halo exchanges are placed means that it is not
possible for two halo exchange with the same index to depend on each
other in a schedule. As a result an exception is raised if this
situation is found.

Notice there is no concept of read or write dependencies here. Read or
write dependencies are handled by classes that make use of the
DataAccess class i.e. the `_field_write_arguments()` and
`_field_read_arguments()` methods, both of which are found in the
`Arguments` class.

Variable Accesses
=================

Especially in the NEMO API, it is not possible to rely on pre-defined
kernel information to determine dependencies between loops. So an additional,
somewhat lower-level API has been implemented that can be used to determine
variable accesses (READ, WRITE etc.), which is based on the PSyIR information.
The only exception to this is if a kernel is called, in which case the
metadata for the kernel declaration will be used to determine the variable
accesses for the call statement. The information about all variable usage
of a node can be gathered by creating an object of type
`psyclone.core.access_info.VariablesAccessInfo`, and then calling
the function `reference_accesses()` for the node:

.. autofunction:: psyclone.psyir.nodes.Node.reference_accesses

.. autoclass:: psyclone.core.access_info.VariablesAccessInfo
    :members:
    :special-members: __str__

This class collects information for each variable used in the tree
starting with the given node. A `VariablesAccessInfo` instance can store
information about variables in any arbitrary code, not only for a PSyIR
node. You can pass it to more than one `reference_accesses()` function
to add more variable access information, or use the `merge()` function to
combine two `VariablesAccessInfo` objects into one. It is up to the user to
keep track of which access information is stored in a `VariablesAccessInfo`
instance.

For each variable used an instance of
`psyclone.core.access_info.VariableAccessInfo` is created, which collects
all accesses for that variable using `psyclone.config.access_info.AccessInfo`
instances:

.. autoclass:: psyclone.core.access_info.VariableAccessInfo
    :members:

.. autoclass:: psyclone.core.access_info.AccessInfo
    :members:

Access Location
---------------

Variable accesses are stored in the order in which they happen. For example,
an assignment `a=a+1` will store two access for the variable `a`, the
first one being a READ access, followed by a WRITE access, since this is the
order in which the accesses are executed.
Additionally, the function `reference_accessess()` keeps track of the location
at which the accesses happen. A location is an integer number, starting with 0,
which is increased for each new statement. This makes it possible to
compare accesses to variables: if two accesses have the same location value,
it means the accesses happen in the same statement, for example `a=a+1`:
the READ and WRITE access to `a` will have the same location number. If on the
other hand the accesses happen in two separate statements, e.g. `a=b+1; c=a+1`
then the first access to `a` (and the access to `b`) will have a smaller
location number than the second access to `a` (and the access to `c`).
If two statements have consecutive locations, this does not necessarily mean
that the statements are executed one after another. For example in if-statements
the statements in the if-body are counted first, then the statements in the
else-body. It is the responsibility of the user to handle these cases - for
example by creating separate `VariablesAccessInfo` for statements in the if-body
and for the else-body.

.. note:: When using different instances for an if- and else-body, the first
    statement of the if-body will
    have the same location number as the first statement of the else-body. So
    you can only compare location numbers from the same `VariablesAccessInformation`
    instance. If you merge two instances together, the locations of the merged-in
    instance will be appropriately increased to follow the locations of the
    instance to which it is merged.


The location number is not exactly a line number - several statements can be
on one line, which will get different location numbers. And certain lines
will not have a location number (e.g. comment lines).

As stated above, one instance of `VariablesAccessInfo` can be extended by adding
additional variable information. It is the responsibility of the user to make
sure the accesses are added in the right order - the `VariablesAccessInfo` object
will always assume accesses happen at the current location, and a call to 
`next_location()` is required to increase the location number.

.. note:: It is not possible to add access information about an earlier
     usage to an existing `VariablesAccessInfo` object. 


Access Examples
---------------

Below we show a simple example of how to use this API. This is from the
`psyclone.psyGen.OMPParallelDirective` (so `self` is an instance of this
node), and this code is used to determine a list of all the scalar
variables that must be declared as thread-private::

  var_accesses = VariablesAccessInfo()
  self.reference_accesses(var_accesses)
  for var_name in var_accesses.all_vars:
      accesses = var_accesses[var_name].all_accesses
      # Ignore variables that are arrays, we only look at scalar ones.
      # If we do have a symbol table, use the shape of the variable
      # to determine if the variable is scalar or not
      if symbol_table:
          if len(symbol_table.lookup(var_name).shape) > 0:
              continue

      # If there is no symbol table, check instead if the first access of
      # the variable has indices, and assume it is an array if it has:
      elif accesses[0].indices is not None:
          continue

      # If a variable is only accessed once, it is either a coding error
      # or a shared variable - anyway it is not private
      if len(accesses) == 1:
          continue

      # We have at least two accesses. If the first one is a write,
      # assume the variable should be private:
      if accesses[0].access_type == AccessType.WRITE:
          result.add(var_name.lower())


The next, hypothetical example shows how the `VariablesAccessInfo` class
can be used iteratively. Assume that you have a function that determines
if the given variable accesses can be parallelised, and the aim is to
determine the largest consecutive block of statements that can be
executed in parallel. The accesses of one statement at a time are added
until we find accesses that would prevent parallelisation::

   # Create an empty instance to store accesses
   accesses = VariablesAccessInfo()
   list_of_parallelisable_statements = []
   while next_statement is not None:
       # Add the variable accesses of the next statement to
       # the existing accesses:
       next_statement.reference_accesses(accesses)
       # Stop when the next statement can not be parallelised
       # together with the previous accesses:
       if not can_be_parallelised(accesses):
           break
       list_of_parallelisable_statements.append(next_statement)
       # Assume there is a function that gives you the next statement:
       next_statement = next_statement.next()


.. note:: There is a certain overlap in the dependency analysis code
          and the variable access API. More work on unifying those two
          approaches will be undertaken in the future. Also, when calling
          `reference_accesses()` for a Dynamo or GOcean kernel, the 
          variable access mode for parameters is taken
          from the kernel metadata, not from the actual kernel source 
          code.

Dependency Tools
----------------
PSyclone contains a class that provides useful tools for dependency analaysis.
It especially provides messages for the user to indicate why parallelisation
was not possible.

.. autoclass:: psyclone.psyir.tools.dependency_tools.DependencyTools
    :members:

.. note:: There is limited support for detecting index expression that are
    identical because of the commutative law, e.g. `i+k` and `k+i` would be
    considered equal. But this only applies if two items are switched that
    are part of the same PSyIR node. An expression like `i+k+1` is stored as
    `(i+k)+1`, so if it is compared with `i+1+k` they are not considered to
    be equal, because `i+1` and `i+k` are not the same.


An example of how to use this class is shown below. It takes a list of statements
(i.e. nodes in the PSyIR), and adds 'OMP DO' directives around loops that
can be parallelised::

  parallel_loop = OMPLoopTrans()
  # The loops in the Fortran functions that must be parallelised
  # are over the 'grid' domain. Note that the psyclone config
  # file specifies the mapping of loop variable to type, e.g.:
  #
  #   mapping-grid = var: np, start: Ns, stop: Ne,  order: 0
  #
  # This means any loop using the variable 'np' is considered a
  # loop of type 'grid'
  dt = DependencyTools(["grid"])

  for statement in statements:
      if isinstance(statement, NemoLoop):
          # Check if there is a variable dependency that might 
          # prevent this loop from being parallelised:
          if dt.can_loop_be_parallelised(statement):
              parallel_loop.apply(statement)
          else:
              # Print all messages from the dependency analysis
              # as feedback for the user:
              for message in dt.get_all_messages():
                  print(message)
