.. -----------------------------------------------------------------------------
   BSD 3-Clause License

   Copyright (c) 2021, Science and Technology Facilities Council.
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
   Written by: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
               J. Henrichs, Bureau of Meteorology

.. testsetup::

    from psyclone.core import AccessType, Signature, VariablesAccessInfo
    from psyclone.psyir.frontend.fortran import FortranReader
    from psyclone.psyir.backend.fortran import FortranWriter
    from psyclone.psyir.nodes import Loop
    from psyclone.psyir.tools import DependencyTools
    from psyclone.transformations import OMPLoopTrans

    # Make sure we use nemo here, otherwise depending on order the
    # wrong API might be set.
    from psyclone.configuration import Config
    Config.get().api = "nemo"

    code = '''subroutine sub()
    integer :: i, j, k, a(10, 10)
    a(i,j) = 1
    do i=1, 10
       j = 3
       a(i,i) = j + k
    enddo
    end subroutine sub
    '''
    psyir = FortranReader().psyir_from_source(code)
    all_var_accesses = VariablesAccessInfo(psyir.children)
    # Get all accesses to the variable 'a', i.e. a(i.j)
    all_a_accesses = all_var_accesses[Signature("a")]
    # Get the first access, which is the write access to 'a(i,j)'
    access_info = all_a_accesses[0]

    # Take the loop node:
    loop = psyir.children[0][1]
    loop_statements = [loop]
    # One example uses code from an OMP directive to determine
    # private variables. But since all this example does is calling
    # `reference_accesses`, we can just pass in the PSyIR node of
    # the loop, which results in two private variables (to avoid
    # creating an OMP Parallel etc)
    omp_directive = loop

    symbol_table = psyir.children[0].symbol_table

    # The next example needs a function 'can_be_parallelised',
    # and a list of statements:
    def can_be_parallelised(access_info):
        return True

    statements = loop.loop_body
    fortran_writer = FortranWriter()


Dependency Analysis Functionality in PSyclone
#############################################

There are two different dependency analysis methods implemented, 
the old :ref:`dependency analysis<old_dependency_analysis>`
one, and a new one based on a 
:ref:`variable access API<variable_accesses>`. There is a certain
overlap between these two methods, and it is expected that the current
dependency analysis, which does not support the NEMO API, will be
integrated with the variable access API in the future (see
`#1148 <https://github.com/stfc/PSyclone/issues/1148>`_).

.. _old_dependency_analysis:

Dependence Analysis
===================

Dependence Analysis in PSyclone produces ordering constraints between
instances of the `Argument` class within a PSyIR tree.

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

.. _variable_accesses:

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

Signature
---------

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


VariablesAccessInfo
-------------------

The `VariablesAccessInfo` class is used to store information about all
accesses in a region of code. To collect access information, the
function `reference_accesses()` for the code region must be called.
It will add the accesses for the PSyIR subtree to the specified instance
of `VariablesAccessInfo`.

.. automethod:: psyclone.psyir.nodes.Node.reference_accesses

.. autoclass:: psyclone.core.access_info.VariablesAccessInfo
    :members:
    :special-members: __str__

This class collects information for each variable used in the tree
starting with the given node. A `VariablesAccessInfo` instance can store
information about variables in high-level concepts such as
a kernel, as well as for language-level PSyIR. You can pass a single
instance to more than one call to `reference_accesses()` in order to
add more variable access information, or use the `merge()` function to
combine two `VariablesAccessInfo` objects into one. It is up to the user to
keep track of which statements (PSyIR nodes) a given `VariablesAccessInfo`
instance is holding information about.

SingleVariableAccessInfo
------------------------
The class `VariablesAccessInfo` uses a dictionary of
`psyclone.core.access_info.SingleVariableAccessInfo` instances to map
from each variable to the accesses of that variable. When a new variable
is detected when adding access information to a `VariablesAccessInfo` instance
via `add_access()`, a new instance of `SingleVariableAccessInfo` is added,
which in turn stores all access to the specified variable.

.. autoclass:: psyclone.core.access_info.SingleVariableAccessInfo
    :members:

AccessInfo
----------
The class `SingleVariableAccessInfo` uses a list of
`psyclone.core.access_info.AccessInfo` instances to store all
accesses to a single variable. A new instance of `AccessInfo`
is appended to the list whenever `add_access_with_location()`
is called.

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

The `ComponentIndices` class provides an array-like accessor for the
internal data structure, you can use `len(component_indices)` to get the
number of components for which array indices are stored.
The information can be accessed using array subscription syntax, e.g.:
`component_index[0]` will return the list of array indices used in the
first component. You can also use a 2-tuple to select a component
and a dimension at the same time, e.g. `component_indices[(0,1)]`, which
will return the index used in the second dimension of the first component.

`ComponentIndices` provides an easy way
to iterate over all indices using its `iterate()` method, which returns all
valid 2-tuples of component index and dimension index. For example:

..
    The testsetup provides the access information for 'a(i,j)=1',
    so it should report the accesses to 'i' and 'j'.

.. testcode::

  # access_info is an AccessInfo instance and contains one access. This
  # could be as simple as `a(i,j)`, but also something more complicated
  # like `a(i+2*j)%b%c(k, l)`.
  for indx in access_info.component_indices.iterate():
      # indx is a 2-tuple of (component_index, dimension_index)
      psyir_index = access_info.component_indices[indx]

  # Using enumerate:
  for count, indx in enumerate(access_info.component_indices.iterate()):
      psyir_index = access_info.component_indices[indx]
      # fortran writer converts a PSyIR node to Fortran:
      print("Index-id {0} of 'a(i,j)': {1}"
            .format(count, fortran_writer(psyir_index)))

.. testoutput::

    Index-id 0 of 'a(i,j)': i
    Index-id 1 of 'a(i,j)': j

To find out details about an index expression, you can either analyse
the tree (e.g. using `walk`), or use the variable access functionality again.
Below is an example that shows how this is done to determine if an array
expression contains a reference to a given variable specified as a
signature in the variable `index_variable`. The
variable `access_info` is an instance of `AccessInfo` and contains the
information about one reference. The function `reference_accesses` is used
to analyse the index expression. Typically, this code would be
wrapped in an outer loop over all accesses.

..
    The testsetup provides the access information for 'a(i,j)=1',
    so the code should output that the index 'i' is used.

.. testcode::

  index_variable = Signature("i")
  # access_info contains the access information for a single
  # reference, e.g. `a(i+2*j)%b%c(k, l)`. Loop over all
  # individual index expressions ("i+2*j", then "k" and "l"
  # in the example above).
  for indx in access_info.component_indices.iterate():
      index_expression = access_info.component_indices[indx]

      # Create an access info object to collect the accesses
      # in the index expression
      accesses = VariablesAccessInfo(index_expression)
      
      # Then test if the index variable is used. Note that
      # the key of `access` is a signature, as is the `index_variable`
      if index_variable in accesses:
          # The index variable is used as an index
          # at the specified location.
          print("Index '{0}' is used.".format(str(index_variable)))
          break
  else:
      print("Index '{0}' is not used.".format(str(index_variable)))


.. testoutput::
    :hide:

    Index 'i' is used.

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
`next_location()` is required (internally) to increase the location number.

.. note:: It is not possible to add access information about an earlier
     usage to an existing `VariablesAccessInfo` object. 


Access Examples
---------------

Below we show a simple example of how to use this API. This is from the
`psyclone.psyir.nodes.OMPParallelDirective`, and it is used to
determine a list of all the scalar variables that must be declared as
thread-private. Note that this code does not handle the usage of
`first-private` declarations.

..
    The testsetup provides the access information to
        do i=1, 10
           j = 3
           a(i,i) = j + k
        enddo
    It should therefore report that 'i' and 'j' should be private
    variables, 'k' is not initialised and therefore assumed to be public.

.. testcode::

  result = set()
  var_accesses = VariablesAccessInfo()
  omp_directive.reference_accesses(var_accesses)
  for signature in var_accesses.all_signatures:
      if signature.is_structure:
          # A lookup in the symbol table for structures are
          # more complicated, so ignore them for this example.
          continue
      var_name = str(signature)
      symbol = symbol_table.lookup(var_name)
      # Ignore variables that are arrays, we only look at scalar ones.
      # The `is_array_access` function will take information from
      # the access information as well as from the symbol table
      # into account.
      access_info = var_accesses[signature]
      if symbol.is_array_access(access_info=access_info):
          # It's not a scalar variable, so it will not be private
          continue

      # If a scalar variable is only accessed once, it is either a coding
      # error or a shared variable - anyway it is not private
      accesses = access_info.all_accesses
      if len(accesses) == 1:
          continue

      # We have at least two accesses. If the first one is a write,
      # assume the variable should be private:
      if accesses[0].access_type == AccessType.WRITE:
          print("Private variable", var_name)
          result.add(var_name.lower())

.. testoutput::
    :hide:

    Private variable i
    Private variable j


The next, hypothetical example shows how the `VariablesAccessInfo` class
can be used iteratively. Assume that you have a function
`can_be_parallelised` that determines
if the given variable accesses can be parallelised, and the aim is to
determine the largest consecutive block of statements that can be
executed in parallel. The accesses of one statement at a time are added
until we find accesses that would prevent parallelisation:

..
    The testsetup provides two statements, and a dummy function
    'can_be_parallelised', which always returns True. It should
    therefore report 2 parallelisable statements.

.. testcode::

   # Create an empty instance to store accesses
   accesses = VariablesAccessInfo()
   list_of_parallelisable_statements = []
   for next_statement in statements:
       # Add the variable accesses of the next statement to
       # the existing accesses:
       next_statement.reference_accesses(accesses)
       # Stop when the next statement can not be parallelised
       # together with the previous accesses:
       if not can_be_parallelised(accesses):
           break
       list_of_parallelisable_statements.append(next_statement)

   print("The first {0} statements can be parallelised."
         .format(len(list_of_parallelisable_statements)))

.. testoutput::
    :hide:

    The first 2 statements can be parallelised.


.. note:: There is a certain overlap in the dependency analysis code
          and the variable access API. More work on unifying those two
          approaches will be undertaken in the future. Also, when calling
          `reference_accesses()` for a Dynamo or GOcean kernel, the
          variable access mode for parameters is taken
          from the kernel metadata, not from the actual kernel source 
          code.

Dependency Tools
================

PSyclone contains a class that builds upon the data-dependency functionality
to provide useful tools for dependency analaysis. It especially provides
messages for the user to indicate why parallelisation was not possible. It
uses `SymPy` internally to compare expressions symbolically.

.. autoclass:: psyclone.psyir.tools.dependency_tools.DependencyTools
    :members:


An example of how to use this class is shown below. It takes a list of statements
(i.e. nodes in the PSyIR), and adds 'OMP DO' directives around loops that
can be parallelised:

..
    The setup passes a single (not-nested) loop as `loop_statements`.
    This loop triggers the message that it is not a nested loop and
    is therefore not be parallelised by default.

.. testcode::

   parallel_loop = OMPLoopTrans()
   # The loops in the Fortran functions that must be parallelised
   # are over the 'lat' domain. Note that the psyclone config
   # file specifies the mapping of loop variable to type, e.g.:
   #
   #   mapping-lat = var: jj, start: 1, stop: jpj
   #
   # This means any loop using the variable 'jj' is considered a
   # loop of type 'lat'
   dt = DependencyTools(["lat"])

   for statement in loop_statements:
       if isinstance(statement, Loop):
           # Check if there is a variable dependency that might
           # prevent this loop from being parallelised:
           if dt.can_loop_be_parallelised(statement):
               parallel_loop.apply(statement)
           else:
               # Print all messages from the dependency analysis
               # as feedback for the user:
               for message in dt.get_all_messages():
                   print(message)

.. testoutput::
    :hide:

    Info: Not a nested loop.
