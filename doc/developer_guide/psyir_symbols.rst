.. -----------------------------------------------------------------------------
   BSD 3-Clause License

   Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
   Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

.. The following section imports those Python modules that are needed in
   subsequent doctest snippets.
.. testsetup::

        from psyclone.psyir.symbols import DataSymbol, ScalarType, ArrayType, \
	    REAL4_TYPE, REAL8_TYPE, INTEGER_TYPE, BOOLEAN_TYPE
	from psyclone.psyir.nodes import Reference

PSyIR Types, Symbols and Data dependencies
##########################################

DataTypes
=========

PSyIR DataTypes currently support Scalar, Array, Structure and empty
types via the ``ScalarType``, ``ArrayType``, ``StructureType`` and
``NoType`` classes, respectively.  The ``StructureType`` simply
contains an ``OrderedDict`` of namedtuples, each of which holds the
name, type and visibility of a component of the type. These types are
designed to be composable: one might have an ``ArrayType`` with
elements that are of a ``StructureType`` or a ``StructureType`` that
has components that are also of (some other) ``StructureType``.
``NoType`` is the equivalent of C's ``void`` and is currently only
used with ``RoutineSymbols`` when the corresponding routine has no
return type (such as Fortran subroutines).

There are two other types that are used in situations where the full
type information is not currently available: ``UnknownType`` means
that the type-declaration is not supported by the PSyIR (or the PSyIR
frontend) and ``DeferredType`` means that the type of a particular
symbol has not yet been resolved. Since ``UnknownType`` captures the
original, unsupported symbol declaration, it is subclassed for each
language for which a PSyIR frontend exists. Currently therefore this
is limited to ``UnknownFortranType``.

It was decided to include datatype intrinsic as an attribute of ScalarType
rather than subclassing. So, for example, a 4-byte real scalar is
defined like this:

.. doctest::

    >>> scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)

and has the following pre-defined shortcut

::

   scalar_type = REAL4_TYPE

If we were to subclass, it would have looked something like this::

   scalar_type = RealType(4)

where ``RealType`` subclasses ``ScalarType``. It may be that the
latter would have provided a better interface, but both approaches have
the same functionality.


Symbols
=======

At the moment, nodes that represent a scope (all `Schedules` and `Containers`)
have a symbol table which contains the symbols used by their descendant nodes.
Nested scopes with their associated symbol table are allowed in the PSyIR.


The `new_symbol` method is provided to create new symbols while avoiding name
clashes:

.. automethod:: psyclone.psyir.symbols.SymbolTable.new_symbol

However, if this symbol needs to be retrieved later on, one must keep track
of the symbol or the returned name. As this is not always feasible when
accessed from different routines, there is also the option to provide a tag to
uniquely identify the symbol internally (the tag is not displayed in the
generated code). Therefore, to create a new symbol and associate it with a
tag, the following code can be used:

.. code-block:: python

    variable = node.symbol_table.new_symbol("variable_name",
                                            tag="variable_with_the_result_x"
                                            symbol_type=DataSymbol,
                                            datatype=DataType.INTEGER)

There are two ways to retrieve the symbol from a symbol table. Using the
`name` or using the `tag` as lookup keys. This is done with the two following
methods:

.. automethod:: psyclone.psyir.symbols.SymbolTable.lookup

.. automethod:: psyclone.psyir.symbols.SymbolTable.lookup_with_tag

Sometimes, we have no way of knowing if a symbol we need has already been
defined. In this case we can use a try/catch around
the `lookup_with_tag` method and if a KeyError is raised (the tag was not
found), then proceed to declare the symbol. As this situation occurs frequently
the Symbol Table provides the `symbol_from_tag` helper method that encapsulates
the described behaviour and declares symbols when needed.

.. automethod:: psyclone.psyir.symbols.SymbolTable.symbol_from_tag

By default the `get_symbol`, `new_symbol`, `add`, `lookup`,
`lookup_with_tag`, and `symbol_from_tag` methods in a symbol table will also
take into account the symbols in any ancestor symbol tables. Ancestor symbol
tables are symbol tables attached to nodes that are ancestors of the
node that the current symbol table is attached to. These are found in order
with the `parent_symbol_table` method. This method provides a `scope_limit`
argument to limit the extend of the upwards recursion provided to each
method that uses it.

Sibling symbol tables are currently not checked. The argument for
doing this is that a symbol in a sibling scope should not be visible
in the current scope so can be ignored. However, it may turn out to
make sense to check both in some circumstances. One result of this is
that names and tags do not need to be unique in the symbol table
hierarchy (just with their ancestor symbols). It makes sense for
symbol names to not be unique in a hierarchy as names can be re-used
within different scopes. However this may not be true for all names
and it may even make sense to have a separate global symbol table in
the future, as well as the existing nested ones. It is less clear
whether tags should be unique or not.

All other methods act only on symbols in the local symbol table. In
particular `__contains__`, `remove`, `get_unresolved_data_symbols`,
`symbols`, `datasymbols`, `local_datasymbols`, `argument_datasymbols`,
`imported_symbols`, `precision_datasymbols` and `containersymbols`. It
is currently not clear whether this is the best solution and it is
possible that these should reflect a global view. One issue is that
the `__contains__` method has no mechanism to pass a `scope_limit`
optional argument. This would probably require a separate `setter` and
`getter` to specify whether to check ancestors or not.

Specialising Symbols
====================

When code is translated into PSyIR there may be symbols with unknown
types, perhaps due to symbols being declared in different files. For
example, in the following declaration it is not possible to know the
type of symbol `fred` without knowing the contents of the `my_module`
module:

.. code-block:: fortran

    use my_module, only : fred

In such cases a generic `Symbol` is created and added to the symbol
table.

Later on in the code translation it may be that `fred` is used as the
name of a subroutine call:

.. code-block:: fortran

    call fred()

It is now known that `fred` is a `RoutineSymbol` so the original
`Symbol` should be replaced by a `RoutineSymbol`.

A simple way to do this would be to remove the original symbol for
`fred` from the symbol table and replace it with a new one that is a
`RoutineSymbol`. However, the problem with this is that there may be
separate references to this symbol from other parts of the PSyIR and
these references would continue to reference the original symbol.

One solution would be to search through all places where references
could occur and update them accordingly. Another would be to modify
the current implementation so that either a) references went in both
directions or b) references were replaced with names and lookups. Each
of these solutions has their benefits and disadvantages.

What is currently done is to specialise the symbol in place (so that
any references to it do not need to change). This is implemented by the
`specialise` method in the `Symbol` class. It takes a subclass of a
`Symbol` as an argument and modifies the instance so that it becomes
the subclass. For example:

.. code-block:: python

    sym = Symbol("a")
    # sym is an instance of the Symbol class
    sym.specialise(RoutineSymbol)
    # sym is now an instance of the RoutineSymbol class

In the current implementation, any additional properties (associated
with a `RoutineSymbol` in the above example) that are not in the
original class would need to be set explicitly after
specialisation. It may be possible to structure `Symbol` (and subclasses
of `Symbol`) constructors so that the `specialise` method could take
additional arguments to initialise properties.


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
of a PSyIR node or a list of nodes can be gathered by creating an object of
type `psyclone.core.access_info.VariablesAccessInfo`.
This class uses a `Signature` object to keep track of the variables used.
A signature can be thought of as a tuple that consists of the variable name
and structure members used in an access - called components. For example,
an access like
`a(1)%b(k)%c(i,j)` would be stored with a signature `(a, b, c)`, giving
three components `a`, `b`, and `c`.
A simple variable such as `a` is stored as a one-element tuple `(a, )`, having
a single component.

.. autoclass:: psyclone.core.access_info.Signature
    :members:
    :special-members: __hash__, __eq__, __lt__

To collect access information in a `VariablesAccessInfo` object, the
function `reference_accesses()` for the code region must be called:

.. automethod:: psyclone.psyir.nodes.Node.reference_accesses

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
`psyclone.core.access_info.SingleVariableAccessInfo` is created, which collects
all accesses for that variable using `psyclone.core.access_info.AccessInfo`
instances:

.. autoclass:: psyclone.core.access_info.SingleVariableAccessInfo
    :members:

.. autoclass:: psyclone.core.access_info.AccessInfo
    :members:

Indices
-------
The `AccessInfo` class stores the original PSyIR node that contains the
access, but it also stores the indices used in a simplified form, which
makes it easier to analyse dependencies without having
to analyse a PSyIR tree for details. The indices are stored in the
ComponentIndices object that each access has, which can be accessed
using the `component_indices` property of an `AccessInfo` object.

.. autoclass:: psyclone.core.access_info.ComponentIndices
    :members:
    :special-members: __getitem__, __len__


This object internally stores the indices as a list of indices for each
component. For example, an access like `a(i)%b(j,k)%c(l)` would
be stored as the list `[ [i], [j,k], [l] ]`. In case of non-array accesses,
the corresponding index list will be empty, e.g. `a%b(j)%c` will
be stored as `[ [], [j], [] ]`, and a scalar `a` will just
return `[ [] ]`. Each member of this list of lists is the PSyIR node
describing the array expression used.

The component indices provides an array-like access to
this data structure, you can use `len(component_indices)` to get the
number of components for which array indices are stored.
The information can be accessed using array subscription syntax, e.g.:
`component_index[0]` will return the list of array indices used in the
first component. You can also use a 2-tuple to select a component
and a dimension at the same time, e.g. `component_indices[(0,1)]`, which
will return the index used in the second dimension of the first component.

`ComponentIndices` provides an easy way
to iterate over all indices using its `iterate()` method, which returns all
valid 2-tuples of component index and dimension index. For example:

.. code-block:: python

  for indx in access_info.component_indices:
      # indx is a 2-tuple of (component_index, dimension_index)
      psyir_index = access_info.component_indices[indx]
      ...
  # Using enumerate:
  for count, indx in enumerate(component_indices.iterate()):
      # fortran writer convers a PSyIR node to Fortran:
      print("Index-id", count, fortran_writer(indx))

To find out details about an index expression, you can either analyse
the tree (e.g. using `walk`), or use the variable access functionality again.
Below is an example that shows how this is done to determine if an array
expression contains a reference to a given variable `index_variable`. The
variable `access_info` is an instance of `AccessInfo` and contains the
information about one reference.The function `reference_accesses` is used
to analyse the index expression.

.. code-block:: python

  for indx in access_info.component_indices:
      index_expression = component_indices[indx]

      # Create an access info object to collect the accesses
      # in the index expression
      accesses = VariablesAccessInfo(index_expression)
      
      # Then test if the index variable is used. Note that
      # the key of `access` is a signature
      if Signature(index_variable) in accesses:
          # The index variable is used as an index
          # at the specified location.
          return indx


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
`psyclone.psyir.nodes.OMPParallelDirective` (so `self` is an instance of this
node), and this code is used to determine a list of all the scalar
variables that must be declared as thread-private::

  var_accesses = VariablesAccessInfo()
  self.reference_accesses(var_accesses)
  for signature in var_accesses.all_signatures:
      var_name = str(signature)
      access_info = var_accesses[signature]
      symbol = symbol_table.lookup(var_name)

      # Ignore variables that are arrays, we only look at scalar ones.
      # The `is_used_as_array` function will take information from
      # the access information as well as from the symbol table
      # into account.
      is_array = symbol.is_used_as_array(access_info=acess_info)

      if is_array:
          # It's not a scalar variable, so it will not be private
          continue

      # If a scalar variable is only accessed once, it is either a coding
      # error or a shared variable - anyway it is not private
      accesses = var_accesses[signature].all_accesses
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
