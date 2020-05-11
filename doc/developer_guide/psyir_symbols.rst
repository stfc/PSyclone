.. -----------------------------------------------------------------------------
   BSD 3-Clause License

   Copyright (c) 2020, Science and Technology Facilities Council.
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


PSyIR Types, Symbols and Data dependencies
##########################################

DataTypes
=========

PSyIR DataTypes currently support Scalar and Array types via the
``ScalarType`` and ``ArrayType`` classes. However, they are designed
to be easily extended. For example, a structure could be created thus:

::
   
    Class StructureType(DataType):
       ''' My Structure class.

           :param str: the name of this derived type.
           :param type_list: a list of datatypes.
           :type type_list: list of :py:class:`psyclone.psyir.symbols.DataType`

       '''
       def __init__(self, name, type_list):
           # Check validity of arguments here
           self.name = name
           self.types = type_list


It was decided to include datatype intrinsic as an attribute of ScalarType
rather than subclassing. So, for example, a 4 byte real scalar is
defined like this

::

   scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)

and has the following pre-defined shortcut

::

   scalar_type = REAL4_TYPE

If we were to subclass, it would have looked something like this:

::

   scalar_type = RealType(4)

where ``RealType`` subclasses ``ScalarType``. It may be that the
latter would have provided a better interface, but both approaches have
the same functionality.


Symbols
=======

At the moment, root nodes (e.g. `InvokeSchedules`, `KernelSchedules`
and `Containers`) have a symbol table which contains the symbols used by their
descendant nodes.


.. note:: Some symbols are still hardwired as strings inside some of the PSyIR
    nodes, but there are issues raised to replace these.

The `new_symbol_name` method is provided to avoid name clashes when defining
a new symbol in the symbol table.

.. automethod:: psyclone.psyir.symbols.SymbolTable.new_symbol_name

However, if this variable needs to be retrieved later on, one must keep track
of the symbol or the returned name. As this is not always feasible when
accessed from different routines, there is also the option to provide a tag to
uniquely identify the symbol internally (the tag is not displayed in the
generated code). Therefore, to create a new symbol and associate it with a
tag, the following lines can be used:

.. code-block:: python

    variable = node.symbol_table.new_symbol_name("variable_name")
    node.symbol_table.add(DataSymbol(variable, DataType.INTEGER),
                          tag="variable_with_the_result_x")

There are two ways to retrieve the symbol from a symbol table. Using the
`name` or using the `tag` as lookup keys. This is done with the two following
methods:

.. automethod:: psyclone.psyir.symbols.SymbolTable.lookup

.. automethod:: psyclone.psyir.symbols.SymbolTable.lookup_with_tag

Sometimes, particularly in the dynamo0p3 API, we have no way of knowing if
a symbol has already been defined. In this case we can use a try/catch around
the `lookup_with_tag` method and if a KeyError is raised (the tag was not
found), then proceed to declare the symbol. As this situation occurs frequently
the Symbol Table provides the `name_from_tag` helper method that encapsulates the
described behaviour and declares generic symbols, which have no datatype
properties, when needed.

.. automethod:: psyclone.psyir.symbols.SymbolTable.name_from_tag

.. warning:: The `name_from_tag` method should not be used for new
    code as the method will be deprecated in favour of a finer control
    of when variables are defined and used.

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
