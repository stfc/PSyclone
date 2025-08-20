.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
.. Modified by R. W. Ford, N. Nobre and S. Siso, STFC Daresbury Lab
.. Modified by J. G. Wallwork, Met Office

.. The following section imports those Python modules that are needed in
   subsequent doctest snippets.
.. testsetup::

        from psyclone.psyir.symbols import DataSymbol, ScalarType, ArrayType, \
	    REAL4_TYPE, REAL8_TYPE, INTEGER_TYPE, BOOLEAN_TYPE
	from psyclone.psyir.nodes import Reference

.. _psyir-ug:

===============================================
The PSyIR
===============================================

The PSyIR (PSyclone Intermediate Representation) is at the heart of PSyclone,
representing both existing
code and PSyKAl DSLs (at both the PSy- and kernel-layer levels). A PSyIR
tree may be constructed from scratch (in Python) or by processing existing
source code using a frontend. Transformations act on the PSyIR and
ultimately the generated code is produced by one of the PSyIR's
backends.

PSyIR Nodes
===========

The PSyIR consists of classes whose instances can be connected
together to form a tree which represent computation in a
syntax-independent way. These classes all inherit from the ``Node``
baseclass and, as a result, PSyIR instances are often referred to
collectively as 'PSyIR nodes'.

At the present time PSyIR classes can be essentially split into two
types: language-level nodes, which are nodes that the PSyIR backends
support, and therefore they can be directly translated to code; and
higher-level nodes, which are additional nodes that each domain can
insert. These nodes must implement a `lower_to_language_level` method
in order to be converted to their equivalent representation using only
language-level nodes. This then permits code to be generated for them.

The rest of this document describes only the language-level nodes, but as
all nodes inherit from the same base classes, the methods described here
are applicable to all PSyIR nodes.


Available language-level nodes
==============================

- :ref_guide:`ArrayMember psyclone.psyir.nodes.html#psyclone.psyir.nodes.ArrayMember`
- :ref_guide:`ArrayReference psyclone.psyir.nodes.html#psyclone.psyir.nodes.ArrayReference`
- :ref_guide:`ArrayOfStructuresMember psyclone.psyir.nodes.html#psyclone.psyir.nodes.ArrayOfStructuresMember`
- :ref_guide:`ArrayOfStructuresReference psyclone.psyir.nodes.html#psyclone.psyir.nodes.ArrayOfStructuresReference`
- :ref_guide:`Assignment psyclone.psyir.nodes.html#psyclone.psyir.nodes.Assignment`
- :ref_guide:`BinaryOperation psyclone.psyir.nodes.html#psyclone.psyir.nodes.BinaryOperation`
- :ref_guide:`Call psyclone.psyir.nodes.html#psyclone.psyir.nodes.Call`
- :ref_guide:`CodeBlock psyclone.psyir.nodes.html#psyclone.psyir.nodes.CodeBlock`
- :ref_guide:`Container psyclone.psyir.nodes.html#psyclone.psyir.nodes.Container`
- :ref_guide:`FileContainer psyclone.psyir.nodes.html#psyclone.psyir.nodes.FileContainer`
- :ref_guide:`IfBlock psyclone.psyir.nodes.html#psyclone.psyir.nodes.IfBlock`
- :ref_guide:`IntrinsicCall psyclone.psyir.nodes.html#psyclone.psyir.nodes.IntrinsicCall`
- :ref_guide:`Literal psyclone.psyir.nodes.html#psyclone.psyir.nodes.Literal`
- :ref_guide:`Loop psyclone.psyir.nodes.html#psyclone.psyir.nodes.Loop`
- :ref_guide:`Member psyclone.psyir.nodes.html#psyclone.psyir.nodes.Member`
- :ref_guide:`Node psyclone.psyir.nodes.html#psyclone.psyir.nodes.Node`
- :ref_guide:`Range psyclone.psyir.nodes.html#psyclone.psyir.nodes.Range`
- :ref_guide:`Reference psyclone.psyir.nodes.html#psyclone.psyir.nodes.Reference`
- :ref_guide:`Return psyclone.psyir.nodes.html#psyclone.psyir.nodes.Return`
- :ref_guide:`Routine psyclone.psyir.nodes.html#psyclone.psyir.nodes.Routine`
- :ref_guide:`Schedule psyclone.psyir.nodes.html#psyclone.psyir.nodes.Schedule`
- :ref_guide:`Statement psyclone.psyir.nodes.html#psyclone.psyir.nodes.Statement`
- :ref_guide:`StructureMember psyclone.psyir.nodes.html#psyclone.psyir.nodes.StructureMember`
- :ref_guide:`StructureReference psyclone.psyir.nodes.html#psyclone.psyir.nodes.StructureReference`
- :ref_guide:`UnaryOperation psyclone.psyir.nodes.html#psyclone.psyir.nodes.UnaryOperation`
- :ref_guide:`WhileLoop psyclone.psyir.nodes.html#psyclone.psyir.nodes.WhileLoop`


Text Representation
===================

When developing a transformation script it is often necessary to examine
the structure of the PSyIR. All nodes in the PSyIR have the ``view`` method
that provides a text-representation of that node and all of its descendants.
If the ``termcolor`` package is installed (see :ref:`getting-going`) then
colour highlighting is used as part of the output string.
For instance, part of the Schedule constructed for the second NEMO
`example <https://github.com/stfc/PSyclone/blob/master/examples/nemo/eg2/
omp_levels_trans.py>`_ is rendered as:

.. image:: schedule_with_indices.png

Note that in this view, only those nodes which are children of
Schedules have their indices shown. This means that nodes representing
e.g. loop bounds or the conditional part of ``if`` statements are not
indexed. For the example shown, the PSyIR node representing the
``if(l_hst)`` code would be reached by
``schedule.children[14].if_body.children[1]`` or, using the shorthand
notation (see below), ``schedule[14].if_body[1]`` where ``schedule`` is
the overall parent Schedule node (omitted from the above image).

One problem with the ``view`` method is that the output can become very
large for big ASTs and is not readable for users unfamiliar with the PSyIR.
An alternative to it is the ``debug_string`` method that generates a
text representation with Fortran-like syntax but on which the high abstraction
constructs have not yet been lowered to Fortran level and instead they will be
embedded as `< node >` expressions.

Tree Navigation
===============

Each PSyIR node provides several ways to navigate the AST. These can be
categorised as homogeneous naviation methods (available in all nodes), and
heterogenous or semantic navigation methods (different methods available
depending on the node type). The homogeneous methods must be used for generic
code navigation that should work regardless of its context. However, when
the context is known, we recommend using the semantic methods to increase
the code readability.

The homogeneous navigation methods are:

   .. automethod:: psyclone.psyir.nodes.Node.children()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.siblings()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.parent()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.root()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.walk()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.get_sibling_lists()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.ancestor()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.scope()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.path_from()
       :no-index:

In addition to the navigation methods, nodes also have homogeneous methods to
interrogate their location and surrounding nodes.

   .. automethod:: psyclone.psyir.nodes.Node.immediately_precedes()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.immediately_follows()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.position()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.abs_position()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Node.sameParent()
       :no-index:

The semantic navigation methods are:

- ``Schedule``:
   subscript operator for indexing the statements (children) inside the
   Schedule, e.g. ``sched[3]`` or ``sched[2:4]``.
- ``Assignment``:
   .. automethod:: psyclone.psyir.nodes.Assignment.lhs()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.Assignment.rhs()
       :no-index:
- ``IfBlock``:
   .. automethod:: psyclone.psyir.nodes.IfBlock.condition()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.IfBlock.if_body()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.IfBlock.else_body()
       :no-index:
- ``Loop``:
   .. automethod:: psyclone.psyir.nodes.Loop.loop_body()
       :no-index:
- ``WhileLoop``:
   .. automethod:: psyclone.psyir.nodes.WhileLoop.condition()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.WhileLoop.loop_body()
       :no-index:
- ``Array`` nodes (e.g. ``ArrayReference``, ``ArrayOfStructuresReference``):
   .. automethod:: psyclone.psyir.nodes.ArrayReference.indices()
       :no-index:
- ``RegionDirective``:
   .. automethod:: psyclone.psyir.nodes.RegionDirective.dir_body()
       :no-index:
   .. automethod:: psyclone.psyir.nodes.RegionDirective.clauses()
       :no-index:
- Nodes representing accesses of data within a structure (e.g. ``StructureReference``, ``StructureMember``):
   .. automethod:: psyclone.psyir.nodes.StructureReference.member()
       :no-index:
- ``BinaryOperation``:
   .. automethod:: psyclone.psyir.nodes.BinaryOperation.operands()
       :no-index:
- ``UnaryOperation``:
   .. automethod:: psyclone.psyir.nodes.UnaryOperation.operand()
       :no-index:


DataTypes
=========

The PSyIR supports the following datatypes: ``ScalarType``,
``ArrayType``, ``StructureType``, ``UnresolvedType``, ``UnsupportedType``
and ``NoType``.  These datatypes are used when creating instances of
DataSymbol, RoutineSymbol and Literal (although note that ``NoType`` may
only be used with a RoutineSymbol). ``UnresolvedType`` and ``UnsupportedType``
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
    >>> symbol = DataSymbol("rdef", int_type, initial_value=4)
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
about its extent. It is necessary to distinguish between three cases:

.. tabularcolumns:: |p{9cm}|L|

+--------------------------------------------+--------------------------------+
|Description                                 | Entry in ``shape`` list        |
+============================================+================================+
|An array that has explict bound             | ``ArrayType.ArrayBounds``      |
|expressions.                                | containing two PSyIR DataNodes.|
+--------------------------------------------+--------------------------------+
|An array has a definite extent which is not | ``ArrayType.Extent.ATTRIBUTE`` |
|known at compile time but can be queried    | or ``ArrayType.ArrayBounds``   |
|at runtime.                                 | with the lower bound given by a|
|                                            | ``DataNode`` and the upper     |
|                                            | bound  by                      |
|                                            | ``ArrayType.Extent.ATTRIBUTE`` |
+--------------------------------------------+--------------------------------+
|It is not known whether an array has memory | ``ArrayType.Extent.DEFERRED``  |
|allocated to it in the current scoping unit.|                                |
+--------------------------------------------+--------------------------------+

where ``ArrayType.ArrayBounds`` is a ``dataclass`` with ``lower`` and
``upper`` members holding the lower- and upper-bounds of the extent of a
given array dimension. Sometimes, the lower bound of an array will be
known while its extent is unknown. This is represented by having the
``upper`` member set to ``ArrayType.Extent.ATTRIBUTE``.

The distinction between the last two cases is that in the former the
extents are known but are kept internally with the array (for example
an assumed-shape array in Fortran) while in the latter, the array has not
yet been allocated any memory (for example the declaration of an
allocatable array in Fortran) so the extents may have not yet been defined.

For example:

.. doctest::

    >>> array_type = ArrayType(REAL4_TYPE, [5, 10])

    >>> n_var = DataSymbol("n", INTEGER_TYPE)
    >>> array_type = ArrayType(INTEGER_TYPE, [Reference(n_var),
    ...                                       Reference(n_var)])

    >>> array_type = ArrayType(REAL8_TYPE, [ArrayType.Extent.ATTRIBUTE,
    ...                                     ArrayType.Extent.ATTRIBUTE])

    >>> array_type = ArrayType(BOOLEAN_TYPE, [ArrayType.Extent.DEFERRED])

Note that Fortran "assumed-size" arrays (which have the last dimension
specified with a ``*``) are not supported in the PSyIR and any such
declaration will result in a ``DataSymbol`` of ``UnsupportedFortranType``.

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
corresponding Symbol is given :ref_guide:`UnsupportedType psyclone.psyir.symbols.html#psyclone.psyir.symbols.UnsupportedType`.
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
can be an ancestor or descendant of a node with an attached symbol
table). If the same symbol name is used in a hierarchy of symbol tables
then the symbol within the symbol table attached to the closest
ancestor node is in scope. By default, symbol tables are aware of
other symbol tables and will return information about relevant symbols
from all symbol tables.

The ``SymbolTable`` has the following interface:

.. autoclass:: psyclone.psyir.symbols.SymbolTable
    :no-index:

Where each element is a ``Symbol`` with an immutable name:

.. autoclass:: psyclone.psyir.symbols.Symbol
    :no-index:

There are several ``Symbol`` sub-classes to represent different
labeled entities in the PSyIR. At the moment the available symbols
are:

- .. autoclass:: psyclone.psyir.symbols.ContainerSymbol
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.DataSymbol
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.DataTypeSymbol
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.IntrinsicSymbol
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.RoutineSymbol
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.GenericInterfaceSymbol
    :no-index:

See the reference guide for the full API documentation of the
:ref_guide:`SymbolTable psyclone.psyir.symbols.html#psyclone.psyir.symbols.SymbolTable`
and the :ref_guide:`Symbol types psyclone.psyir.symbols.html#module-psyclone.psyir.symbols`.

Symbol Interfaces
-----------------

Each symbol has a Symbol Interface with the information about how the
variable data is provided into the local context. The currently available
Interfaces are:


- .. autoclass:: psyclone.psyir.symbols.AutomaticInterface
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.DefaultModuleInterface
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.ImportInterface
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.ArgumentInterface
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.StaticInterface
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.CommonBlockInterface
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.UnresolvedInterface
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.UnknownInterface
    :no-index:

- .. autoclass:: psyclone.psyir.symbols.PreprocessorInterface
    :no-index:


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
			  is_constant=True,
                          initial_value=3)

declares a symbol named "something" of REAL_TYPE datatype where the
``is_constant`` and ``initial_value`` arguments will be passed to the
DataSymbol constructor.

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
``Schedule`` (e.g. for ``IfBlock``, ``Loop`` and ``WhileLoop``).

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


When the node being replaced is part of a named context (in Calls or
Operations) the name of the argument is conserved by default. For example


.. code-block:: fortran

    call named_subroutine(name1=1)

.. code-block:: python

    call.children[0].replace_with(Literal('2', INTEGER_TYPE))

will become:

.. code-block:: fortran

    call named_subroutine(name1=2)

This behaviour can be changed with the `keep_name_in_context` parameter.

.. code-block:: python

    call.children[0].replace_with(
        Literal('3', INTEGER_TYPE),
        keep_name_in_context=False
    )

will become:

.. code-block:: fortran

    call named_subroutine(3)


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

The `Call` node (and its sub-classes) support named arguments.

Named arguments can be set or modified via the `create()`,
`append_named_arg()`, `insert_named_arg()` or `replace_named_arg()`
methods.

If an argument is inserted directly (via the children list) then it is
assumed that this is not a named argument. If the top node of an
argument is replaced by removing and inserting a new node then it is
assumed that this argument is no longer a named argument. If it is
replaced with the `replace_with` method, it has a `keep_name_in_context`
argument to choose the desired behaviour (defaults to True).
If arguments are re-ordered then the names follow the re-ordering.

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
