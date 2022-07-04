.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
.. Written by A. R. Porter, STFC Daresbury Lab
.. Modified by R. W. Ford, STFC Daresbury Lab

.. The following section imports those Python modules that are needed in
   subsequent doctest snippets.
.. testsetup::

        from psyclone.psyir.symbols import DataSymbol, ScalarType, ArrayType, \
	    REAL4_TYPE, REAL8_TYPE, INTEGER_TYPE, BOOLEAN_TYPE
	from psyclone.psyir.nodes import Reference

.. _psyir-ug:

==============================================
 PSyIR : The PSyclone Internal Representation
==============================================

The PSyIR is at the heart of PSyclone, representing code (at both the
PSy- and kernel-layer levels) in a language-agnostic form. A PSyIR may
be constructed from scratch (in Python) or by processing existing
source code using a frontend. Transformations act on the PSyIR and
ultimately the generated code is produced by one of the PSyIR's
backends.

PSyIR Nodes
===========

The PSyIR consists of classes whose instances can be connected
together to form a tree which represent computation in a
language-independent way. These classes all inherit from the ``Node``
baseclass and, as a result, PSyIR instances are often referred to
collectively as 'PSyIR nodes'.

At the present time PSyIR classes can be essentially split into two
types. PSy-layer classes and Kernel-layer classes. PSy-layer classes
make use of a ``gen_code()`` or an ``update()`` method to create
Fortran code whereas Kernel-layer classes make use of PSyIR backends
to create code.

.. note:: This separation will be removed in the future and eventually
	  all PSyIR classes will make use of backends with the
	  expectation that ``gen_code()`` and ``update()`` methods
	  will be removed. Further this separation will be superceded
	  by a separation between ``language-level PSyIR`` and
	  ``domain-specific PSyIR``.

PSy-layer nodes
---------------

PSy-layer PSyIR classes are primarily used to create the
PSy-layer. These tend to be relatively descriptive and do not specify
how a particular PSyclone frontend would implement them. With the
exception of ``Loop``, these classes are currently not compatible with
the PSyIR backends. The generic (non-api-specific) PSy-layer PSyIR
nodes are: ``InvokeSchedule``, ``Directive``, ``GlobalSum``,
``HaloExchange``, ``Loop`` and ``Kern``. The ``Directive`` class is
subclassed into many directives associated with OpenMP and
OpenACC. The ``Kern`` class is subclassed into ``CodedKern``,
``InlinedKern`` and ``BuiltinKern``.


Kernel-layer nodes
------------------

Kernel-layer PSyIR classes are currently used to describe existing
code in a language independent way. Consequently these nodes are more
prescriptive and are independent of a particular PSyclone
frontend. These nodes are designed to be used with PSyIR backends. Two
PSy-layer classes (``Loop`` and ``Schedule``) can also be used as
Kernel-layer classes. Additionally, the ``Schedule`` class is further
subclassed into a ``Routine`` and then a kernel-layer
``KernelSchedule``.  In addition to ``KernelSchedule``, Kernel-layer
PSyIR nodes are: ``Loop``, ``IfBlock``, ``CodeBlock``, ``Assignment``,
``Range``, ``Reference``, ``Operation``, ``Literal``, ``Call``,
``Return`` and ``Container``. The ``Reference`` class is further
subclassed into ``ArrayReference``, ``StructureReference`` and
``ArrayOfStructuresReference``, the ``Operation`` class is further
subclassed into ``UnaryOperation``, ``BinaryOperation`` and
``NaryOperation`` and the ``Container`` class is further subclassed
into ``FileContainer`` (representing a file that may contain more than
one ``Container`` and/or ``Routine``. Those nodes representing
references to structures (derived types in Fortran) have a ``Member``
child node representing the member of the structure being
accessed. The ``Member`` class is further subclassed into
``StructureMember`` (representing a member of a structure that is
itself a structure), ``ArrayMember`` (a member of a structure that is
an array of primitive types) and ``ArrayOfStructuresMember`` (a member
of a structure this is itself an array of structures).


Node Descriptions
=================

The Range node
--------------

.. autoclass:: psyclone.psyir.nodes.Range
    :members: create, start, step, stop

Text Representation
===================

When developing a transformation script it is often necessary to examine
the structure of the PSyIR. All nodes in the PSyIR have the ``view`` method
that writes a text-representation of that node and all of its
descendants to stdout. If the ``termcolor`` package is installed
(see :ref:`getting-going`) then colour highlighting is used for this
output. For instance, part of the Schedule constructed for the second NEMO
`example <https://github.com/stfc/PSyclone/blob/master/examples/nemo/eg2/
omp_levels_trans.py>`_ is rendered as:

.. image:: schedule_with_indices.png

Note that in this view, only those nodes which are children of
Schdules have their indices shown. This means that nodes representing
e.g. loop bounds or the conditional part of ``if`` statements are not
indexed. For the example shown, the PSyIR node representing the
``if(l_hst)`` code would be reached by
``schedule.children[6].if_body.children[1]`` or, using the shorthand
notation (see below), ``schedule[6].if_body[1]`` where ``schedule`` is
the overall parent Schedule node (omitted from the above image).

Tree Navigation
===============

Each PSyIR node provides several ways to navigate the AST:

The `children` and `parent` properties (available in all nodes) provide an
homogeneous method to go up and down the tree hierarchy. This method
is recommended when applying general operations or analysis to the tree,
however, if one intends to navigate the tree in a way that depends on the type
of node, the `children` and `parent` methods should be avoided. The structure
of the tree may change in different versions of PSyclone and the encoded
navigation won't be future-proof.

To solve this issue some Nodes also provide methods for semantic navigation:

- ``Schedule``:
   subscript operator for indexing the statements (children) inside the
   Schedule, e.g. ``sched[3]`` or ``sched[2:4]``.
- ``Assignment``:
   .. automethod:: psyclone.psyir.nodes.Assignment.lhs()
   .. automethod:: psyclone.psyir.nodes.Assignment.rhs()
- ``IfBlock``:
   .. automethod:: psyclone.psyir.nodes.IfBlock.condition()
		
   .. automethod:: psyclone.psyir.nodes.IfBlock.if_body()

   .. automethod:: psyclone.psyir.nodes.IfBlock.else_body()
- ``Array`` nodes (e.g. ``ArrayReference``, ``ArrayOfStructuresReference``):
   .. automethod:: psyclone.psyir.nodes.ArrayReference.indices()
- ``RegionDirective``:
   .. automethod:: psyclone.psyir.nodes.RegionDirective.dir_body()
   .. automethod:: psyclone.psyir.nodes.RegionDirective.clauses()
- Nodes representing accesses of data within a structure (e.g. ``StructureReference``, ``StructureMember``):
   .. automethod:: psyclone.psyir.nodes.StructureReference.member()

These are the recommended methods to navigate the tree for analysis or
operations that depend on the Node type.

Additionally, the `walk` method (available in all nodes) is able to recurse
through the tree and return objects of a given type. This is useful when the
objective is to move down the tree to a specific node or list of nodes without
information about the exact location.

.. automethod:: psyclone.psyir.nodes.Node.walk

Finally, all nodes also provide the `ancestor` method which may be used to
recurse back up the tree from a given node in order to find a node of a
particular type:

.. automethod:: psyclone.psyir.nodes.Node.ancestor

DataTypes
=========

The PSyIR supports the following datatypes: ``ScalarType``,
``ArrayType``, ``StructureType``, ``DeferredType``, ``UnknownType``
and ``NoType``.  These datatypes are used when creating instances of
DataSymbol, RoutineSymbol and Literal (although note that ``NoType`` may
only be used with a RoutineSymbol). ``DeferredType`` and ``UnknownType``
are both used when processing existing code. The former is used
when a symbol is being imported from some other scope (e.g. via a USE
statement in Fortran) that hasn't yet been resolved and the latter is
used when an unsupported form of declaration is encountered.

More information on each of these various datatypes is given in the
following subsections.

Scalar DataType
---------------

A Scalar datatype consists of an intrinsic and a precision.

The intrinsic can be one of ``INTEGER``, ``REAL``, ``BOOLEAN`` and
``CHARACTER``.

The precision can be ``UNDEFINED``, ``SINGLE``, ``DOUBLE``, an integer
value specifying the precision in bytes, or a datasymbol (see Section
:ref:`symbol-label`) that contains precision information. Note that
``UNDEFINED``, ``SINGLE`` and ``DOUBLE`` allow the precision to be set
by the system so may be different for different architectures. For
example:

.. doctest::

    >>> char_type = ScalarType(ScalarType.Intrinsic.CHARACTER,
    ...                        ScalarType.Precision.UNDEFINED)
    >>> int_type = ScalarType(ScalarType.Intrinsic.INTEGER,
    ...                       ScalarType.Precision.SINGLE)
    >>> bool_type = ScalarType(ScalarType.Intrinsic.BOOLEAN, 4)
    >>> symbol = DataSymbol("rdef", int_type, constant_value=4)
    >>> scalar_type = ScalarType(ScalarType.Intrinsic.REAL, symbol)

For convenience PSyclone predefines a number of scalar datatypes:

``REAL_TYPE``, ``INTEGER_TYPE``, ``BOOLEAN_TYPE`` and
``CHARACTER_TYPE`` all have precision set to ``UNDEFINED``;

``REAL_SINGLE_TYPE``, ``REAL_DOUBLE_TYPE``, ``INTEGER_SINGLE_TYPE``
and ``INTEGER_DOUBLE_TYPE``;

``REAL4_TYPE``, ``REAL8_TYPE``, ``INTEGER4_TYPE`` and
``INTEGER8_TYPE``.

Array DataType
--------------

An Array datatype itself has another datatype (or ``DataTypeSymbol``)
specifying the type of its elements and a shape. The shape can have an
arbitrary number of dimensions. Each dimension captures what is known
about its extent. It is necessary to distinguish between four cases:

.. tabularcolumns:: |p{9cm}|L|

+--------------------------------------------+--------------------------------+
|Description                                 | Entry in ``shape`` list        |
+============================================+================================+
|An array has a static extent known at       | ``ArrayType.ArrayBounds``      |
|compile time.                               | containing integer ``Literal`` |
|                                            | values                         |
+--------------------------------------------+--------------------------------+
|An array has an extent defined by another   | ``ArrayType.ArrayBounds``      |
|symbol or (constant) PSyIR expression.      | containing ``Reference`` or    |
|                                            | ``Operation`` nodes            |
+--------------------------------------------+--------------------------------+
|An array has a definite extent which is not | ``ArrayType.Extent.ATTRIBUTE`` |
|known at compile time but can be queried    |                                |
|at runtime.                                 |                                |
+--------------------------------------------+--------------------------------+
|It is not known whether an array has memory | ``ArrayType.Extent.DEFERRED``  |
|allocated to it in the current scoping unit.|                                |
+--------------------------------------------+--------------------------------+

where ``ArrayType.ArrayBounds`` is a ``namedtuple`` with ``lower`` and
``upper`` members holding the lower- and upper-bounds of the extent of a
given array dimension.

The distinction between the last two cases is that in the former the
extents are known but are kept internally with the array (for example
an assumed shape array in Fortran) and in the latter the array has not
yet been allocated any memory (for example the declaration of an
allocatable array in Fortran) so the extents may have not been defined
yet.

For example:

.. doctest::

    >>> array_type = ArrayType(REAL4_TYPE, [5, 10])

    >>> n_var = DataSymbol("n", INTEGER_TYPE)
    >>> array_type = ArrayType(INTEGER_TYPE, [Reference(n_var),
    ...                                       Reference(n_var)])

    >>> array_type = ArrayType(REAL8_TYPE, [ArrayType.Extent.ATTRIBUTE,
    ...                                     ArrayType.Extent.ATTRIBUTE])

    >>> array_type = ArrayType(BOOLEAN_TYPE, [ArrayType.Extent.DEFERRED])

Structure Datatype
------------------

A Structure datatype consists of a dictionary of components where the
name of each component is used as the corresponding key. Each component
is stored as a named tuple with ``name``, ``datatype`` and ``visibility``
members.

For example:

.. code-block:: python

  # Shorthand for a scalar type with REAL_KIND precision
  SCALAR_TYPE = ScalarType(ScalarType.Intrinsic.REAL, REAL_KIND)

  # Structure-type definition
  GRID_TYPE = StructureType.create([
      ("dx", SCALAR_TYPE, Symbol.Visibility.PUBLIC),
      ("dy", SCALAR_TYPE, Symbol.Visibility.PUBLIC)])

  GRID_TYPE_SYMBOL = DataTypeSymbol("grid_type", GRID_TYPE)

  # A structure-type containing other structure types
  FIELD_TYPE_DEF = StructureType.create(
      [("data", ArrayType(SCALAR_TYPE, [10]), Symbol.Visibility.PUBLIC),
       ("grid", GRID_TYPE_SYMBOL, Symbol.Visibility.PUBLIC),
       ("sub_meshes", ArrayType(GRID_TYPE_SYMBOL, [3]),
        Symbol.Visibility.PUBLIC),
       ("flag", INTEGER4_TYPE, Symbol.Visibility.PUBLIC)])

Unknown DataType
----------------

If a PSyIR frontend encounters an unsupported declaration then the
corresponding Symbol is given :ref_guide:`UnknownType psyclone.psyir.symbols.html#psyclone.psyir.symbols.UnknownType`.
The text of the original declaration is stored in the type object and is
available via the ``declaration`` property.


NoType
------

``NoType`` represents the empty type, equivalent to ``void`` in C. It
is currently only used to describe a RoutineSymbol that has no return
type (such as a Fortran subroutine).

.. _symbol-label:

Symbols and Symbol Tables
=========================

Some PSyIR nodes have an associated Symbol Table
(`psyclone.psyir.symbols.SymbolTable`) which keeps a record of the
Symbols (`psyclone.psyir.symbols.Symbol`) specified and used within them.

Symbol Tables can be nested (i.e. a node with an attached symbol table
can be an ancestor or descendent of a node with an attached symbol
table). If the same symbol name is used in a hierachy of symbol tables
then the symbol within the symbol table attached to the closest
ancestor node is in scope. By default, symbol tables are aware of
other symbol tables and will return information about relevant symbols
from all symbol tables.

The ``SymbolTable`` has the following interface:

.. autoclass:: psyclone.psyir.symbols.SymbolTable

Where each element is a ``Symbol`` with an immutable name:

.. autoclass:: psyclone.psyir.symbols.Symbol

There are several ``Symbol`` sub-classes to represent different
labeled entities in the PSyIR. At the moment the available symbols
are:

- .. autoclass:: psyclone.psyir.symbols.ContainerSymbol

- .. autoclass:: psyclone.psyir.symbols.DataSymbol

- .. autoclass:: psyclone.psyir.symbols.RoutineSymbol

See the reference guide for the full API documentation of the
:ref_guide:`SymbolTable psyclone.psyir.symbols.html#psyclone.psyir.symbols.SymbolTable`
and the :ref_guide:`Symbol types psyclone.psyir.symbols.html#module-psyclone.psyir.symbols`.

Symbol Interfaces
-----------------

Each symbol has a Symbol Interface with the information about how the
variable data is provided into the local context. The currently available
Interfaces are:


- .. autoclass:: psyclone.psyir.symbols.LocalInterface

- .. autoclass:: psyclone.psyir.symbols.ImportInterface

- .. autoclass:: psyclone.psyir.symbols.ArgumentInterface

- .. autoclass:: psyclone.psyir.symbols.UnresolvedInterface


Creating PSyIR
==============

Symbol names
------------

PSyIR symbol names can be specified by a user. For example:

.. code-block:: python

   var_name = "my_name"
   symbol_table = SymbolTable()
   data = DataSymbol(var_name, REAL_TYPE)
   symbol_table.add(data)
   reference = Reference(data)

However, the ``SymbolTable`` ``add()`` method will raise an exception if a
user tries to add a symbol with the same name as a symbol already existing
in the symbol table.

Alternatively, the ``SymbolTable`` also provides the ``new_symbol()`` method
(see Section :ref:`symbol-label` for more details) that uses a new distinct
name from any existing names in the symbol table. By default the generated
name is the value ``PSYIR_ROOT_NAME`` variable specified in the ``DEFAULT``
section of the PSyclone config file, followed by an optional "_" and
an integer. For example, the following code:

.. code-block:: python

  from psyclone.psyir.symbols import SymbolTable
  symbol_table = SymbolTable()
  for i in range(0, 3):
      var_name = symbol_table.new_symbol().name
      print(var_name)

gives the following output::
  
  psyir_tmp
  psyir_tmp_0
  psyir_tmp_1

As the root name (``psyir_tmp`` in the example above) is specified in
PSyclone's config file it can be set to whatever the user wants.

.. note:: The particular format used to create a unique name is the
  responsibility of the SymbolTable class and may change in the
  future.

A user might want to create a name that has some meaning in the
context in which it is used e.g. ``idx`` for an index, ``i`` for an
iterator, or ``temp`` for a temperature field. To support more
readable names, the ``new_symbol()`` method allows the user to specify a
root name as an argument to the method which then takes the place of
the default root name. For example, the following code:
  
.. code-block:: python

  from psyclone.psyir.symbols import SymbolTable
  symbol_table = SymbolTable()
  for i in range(0, 3):
      var_name = symbol_table.new_symbol(root_name="something")
      print(var_name)

gives the following output::
  
  something
  something_0
  something_1

By default, ``new_symbol()`` creates generic symbols, but often the user
will want to specify a Symbol subclass with some given parameters. The
``new_symbol()`` method accepts a ``symbol_type`` parameter to specify the
subclass.  Arguments for the constructor of that subclass may be supplied
as keyword arguments. For example, the following code:
 
.. code-block:: python

  from psyclone.psyir.symbols import SymbolTable, DataSymbol, REAL_TYPE
  symbol_table = SymbolTable()
  symbol_table.new_symbol(root_name="something",
                          symbol_type=DataSymbol,
                          datatype=REAL_TYPE,
                          constant_value=3)

declares a symbol named "something" of REAL_TYPE datatype where the
constant_value argument will be passed to the DataSymbol constructor.

An example of using the ``new_symbol()`` method can be found in the
PSyclone ``examples/psyir`` directory.

Nodes
-----

PSyIR nodes are connected together via parent and child methods
provided by the ``Node`` baseclass.

These nodes can be created in isolation and then connected
together. For example:

.. code-block:: python

    assignment = Assignment()
    literal = Literal("0.0", REAL_TYPE)
    reference = Reference(symbol)
    assignment.children = [reference, literal]
    
However, as connections get more complicated, creating the correct
connections can become difficult to manage and error prone. Further,
in some cases children must be collected together within a
``Schedule`` (e.g. for ``IfBlock`` and for ``Loop``).

To simplify this complexity, each of the Kernel-layer nodes which
contain other nodes have a static ``create`` method which helps
construct the PSyIR using a bottom up approach. Using this method, the
above example then becomes:

.. code-block:: python

    literal = Literal("0.0", REAL_TYPE)
    reference = Reference(symbol)
    assignment = Assignment.create(reference, literal)

Creating the PSyIR to represent a complicated access of a member of a
structure is best performed using the ``create()`` method of the
appropriate ``Reference`` subclass. For a relatively straightforward
access such as (the Fortran) ``field1%region%nx``, this would be:

.. code-block:: python

  from psyclone.psyir.nodes import StructureReference
  fld_sym = symbol_table.lookup("field1")
  ref = StructureReference.create(fld_sym, ["region", "nx"])

where ``symbol_table`` is assumed to be a pre-populated Symbol Table
containing an entry for "field1".

A more complicated access involving arrays of structures such as
``field1%sub_grids(idx, 1)%nx`` would be constructed as:

.. code-block:: python

  from psyclone.psyir.symbols import INTEGER_TYPE
  from psyclone.psyir.nodes import StructureReference, Reference, Literal
  idx_sym = symbol_table.lookup("idx")
  fld_sym = symbol_table.lookup("field1")
  ref = StructureReference.create(fld_sym,
      [("sub_grids", [Reference(idx_sym), Literal("1", INTEGER_TYPE)]),
       "nx"])

Note that the list of quantities passed to the ``create()`` method now
contains a 2-tuple in order to describe the array access.

More examples of using this approach can be found in the PSyclone
``examples/psyir`` directory.


Comparing PSyIR nodes
=====================
The ``==`` (equality) operator for PSyIR nodes performs a specialised equality check
to compare the value of each node. This is also useful when comparing entire
subtrees since the equality operator automatically recurses through the children
and compares each child with the appropriate equality semantics, e.g.

.. code-block:: python

    # Is the loop upper bound expression exactly the same?
    if loop1.stop_expr == loop2.stop_expr:
	    print("Same upper bound!")

The equality operator will handle expressions like ``my_array%my_field(:3)`` with the
derived type fields and the range components automatically, but it cannot handle
symbolically equivalent fields, i.e. ``my_array%my_field(:3) != my_array%my_field(:2+1)``.

Annotations and code comments are ignored in the equality comparison since they don't
alter the semantic meaning of the code. So these two statements compare to True:

.. code-block:: fortran
    
    a = a + 1
    a = a + 1 !Increases a by 1

Sometimes there are cases where one really means to check for the specific instance
of a node. In this case, Python provides the ``is`` operator, e.g.

.. code-block:: python

    # Is the self instance part of this routine?
    is_here = any(node is self for node in routine.walk(Node))

Additionally, PSyIR nodes cannot be used as map keys or similar. The easiest way
to do this is just use the id as the key:

.. code-block:: python

    node_map = {}
    node_map[id(mynode)] = "element"


Modifying the PSyIR
===================

Once we have a complete PSyIR AST there are 2 ways to modify its contents
and/or structure: by applying transformations (see next section
:ref:`transformations`), or by direct PSyIR API methods. This section
describes some of the methods that the PSyIR classes provide to
modify the PSyIR AST in a consistent way (e.g. without breaking its many
internal references). Some complete examples of modifying the PSyIR can be found in the
PSyclone ``examples/psyir/modify.py`` script.

The rest of this section introduces examples of the available direct PSyIR
modification methods.

Renaming symbols
----------------
The symbol table provides the method ``rename_symbol()`` that given a symbol
and an unused name will rename the symbol. The symbol renaming will affect
all the references in the PSyIR AST to that symbol. For example, the PSyIR
representing the following Fortran code:

.. code-block:: fortran

    subroutine work(psyir_tmp)
        real, intent(inout) :: psyir_tmp
        psyir_tmp=0.0
    end subroutine

could be modified by the following PSyIR statements:

.. code-block:: python

    symbol = symbol_table.lookup("psyir_tmp")
    symbol_table.rename_symbol(tmp_symbol, "new_variable")

which would result in the following Fortran output code:

.. code-block:: fortran

    subroutine work(new_variable)
        real, intent(inout) :: new_variable
        new_variable=0.0
    end subroutine

Specialising symbols
--------------------

The Symbol class provides the method ``specialise()`` that given a
subclass of Symbol will change the Symbol instance to the specified
subclass. If the subclass has any additional properties then these
would need to be set explicitly.

.. code-block:: python

    symbol = Symbol("name")
    symbol.specialise(RoutineSymbol)
    # Symbol is now a RoutineSymbol

This method is useful as it allows the class of a symbol to be changed
without affecting any references to it.

Replacing PSyIR nodes
---------------------

In certain cases one might want to replace a node in a PSyIR tree with
another node. All nodes provide the `replace_with()` method to replace
the node and its descendants with another given node and its
descendants.

.. code-block:: python

    node.replace_with(new_node)

Detaching PSyIR nodes
---------------------

Sometimes we just may wish to detach a certain PSyIR subtree in order to remove
it from the root tree but we don't want to delete it altogether, as it may
be re-inserted again in another location. To achieve this, all nodes
provide the detach method:

.. code-block:: python

    tmp = node.detach()

Copying nodes
-------------

Copying a PSyIR node and its children is often useful in order to avoid
repeating the creation of similar PSyIR subtrees. The result of the copy
allows the modification of the original and the copied subtrees independently,
without altering the other subtree. Note that this is not equivalent to the
Python ``copy`` or ``deepcopy`` functionality provided in the ``copy`` library.
This method performs a bespoke copy operation where some components of the
tree, like children, are recursively copied, while others, like the top-level
parent reference are not.


.. code-block:: python

    new_node = node.copy()

Named arguments
---------------

The `Call` and three sub-classes of `Operation` node
(`UnaryOperation`, `BinaryOperation` and `NaryOperation`) all support
named arguments.

Named arguments can be set or modified via the `create()`,
`append_named_arg()`, `insert_named_arg()` or `replace_named_arg()`
methods.

If an argument is inserted directly (via the children list) then it is
assumed that this is not a named argument. If the top node of an
argument is replaced then it is assumed that this argument is no
longer a named argument. If arguments are re-ordered then the names
follow the re-ordering.

The names of named arguments can be accessed via the `argument_names`
property. This list has an entry for each argument and either contains
a name or None (if this is not a named argument).

The PSyIR does not constrain which arguments are specified as being
named and what those names are. It is the developer's responsibility
to make sure that these names are consistent with any intrinsics that
will be generated by the back-end. In the future, it is expected that
the PSyIR will know about the number and type of arguments expected by
Operation nodes, beyond simply being unary, binary or nary.

One restriction that Fortran has (but the PSyIR does not) is that all
named arguments should be at the end of the argument list. If this is
not the case then the Fortran backend writer will raise an exception.
