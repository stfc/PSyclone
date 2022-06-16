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
.. Written by R. W. Ford, A. R. Porter, S. Siso and A. B. G. Chalk STFC Daresbury Lab

The PSyclone Internal Representation (PSyIR)
############################################

The PSyclone Intermediate Representation (PSyIR) is a language-independent
Intermediate Representation that PSyclone uses to represent the PSy (Parallel
System) and the Kernel (serial units of work) layers of an application that
can be constructed from scratch or produced from existing code using one of
the PSyIR front-ends. Its design is optimised to represent high-performance
parallel applications in an expressive, mutable and extensible way:

 - It is **expressive** because it is intended to be created and/or manipulated
   by HPC software experts directly when optimizing or porting the code.

 - It is **mutable** because it is intended to be programmatically manipulated
   (usually though PSyclone scripts) and maintain a coherent state (with
   valid relationships, links to symbols, links to dependencies) after each
   manipulation.

 - It is **extensible** because it is intended to be used as the core component
   of domain-specific systems which include additional abstract concepts
   or logic not captured in the generic representation.

To achieve these design goals we use a Normalised Heterogeneous AST
representation together with a Type System and a Symbol Table.
By **heterogeneous** we mean that we distinguish between AST nodes using
Python class inheritance system and each node has its particular (and
semantically relevant) navigation and behaviour methods. For instance the
``Assignment`` node has ``lhs`` and ``rhs`` properties to navigate to the
left-hand-side and right-hand-side operators of the Assignment. It also
means we can indentify a node using its type with
``isinstance(node, Assignment)``.
Nevertheless, we maintain a **normalised** core of node relationships and
functionality that allows us to build tree walkers, tree visitors and
dependency analysis tools without the need to consider the implementation
details of each individual sub-class.

The common functionality that all nodes must have is defined in the
PSyIR base class `Node`.
See the list of all PSyIR common methods in the 
:ref_guide:`Node reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.Node`.

More information about the type system and symbols and how PSyIR
can be transformed back to a particular language using the back-ends
(Writers) is provided in the following sections of this guide.


.. _newnodes-label:

How to create new PSyIR Nodes
=============================
In order to create a new PSyIR node, either for adding a new core PSyIR node
or to extend the functionality in one of the PSyclone APIs, it is mandatory
to perform the following steps: 

1. The new node must inherit from ``psyclone.psyir.nodes.Node`` or one of its
   sub-classes. Note that ``Node`` won't be accepted as a child anywhere in
   the tree. It may be appropriate to specialise one of the existing
   subclasses of Node, rather than Node itself.
   A good starting point would be to consider
   ``psyclone.psyir.nodes.Statement`` (which will be accepted inside any
   Schedule) or ``psyclone.psyir.nodes.DataNode`` (which will be accepted
   anywhere that the node can be evaluated to a data element). 

2. Set the ``_text_name`` and the ``_color_key`` class string attributes. These
   attributes will provide standard behaviour for the ``__str__`` and
   ``view()`` methods.

3. Set the ``_children_valid_format`` class string attribute and specialise
   the static method ``_validate_child(position, child)``. These define,
   textually and logically, what types of nodes will be accepted as children
   of the new node:

    - ``_children_valid_format`` is the textual representation that will be
      used in error messages. It is expressed using tokens with the same name
      as the PSyIR classes and the following symbols:

        - ``|``: or operand.

        - ``,``: concatenation operand.

        - ``[ expression ]*``: zero or more instances of the expression.

        - ``[ expression ]+``: one or more instances of the expression.

        - ``<LeafNode>``: NULL operand (no children accepted).

      For instance, an expression that accepts a statement as a first child and
      one or more DataNodes after it would be: ``Statement [, DataNode]+``.


    - ``_validate_child(position, child)`` returns a boolean which indicates
      whether the given child is a valid component for the given position.

.. note:: Note that the valid children of a node are described two times, once in
    ``_children_valid_format`` and another in ``_validate_child``, and it is
    up to the developer to keep them coherent in order to provide sensible
    error messages. Alternatively we could create an implementation where
    the textual representation is parsed and the validation method is
    generated automatically, hence avoiding the duplication. Issue #765
    explores this alternative.

4. If any of the attributes introduced by this method should not be
   shallow-copied when creating a duplicate of this PSyIR branch, specialise
   the ``_refine_copy`` method to perform the appropriate copy actions.

5. If any of the attributes in this node should be used to compute the equality
   of two nodes, specialise the ``__eq__`` member to perform the appropriate
   checks. The default ``__eq__`` behaviour is to check both instance types are
   exactly the same, and each of their children also pass the equality check.
   The only restriction on this implementation is that it must call the
   ``super().__eq__(other)`` as part of its implementation, to ensure any
   inherited equality checks are correctly checked. The default behaviour
   ignores annotations and comment attributes, as they should not affect the
   semantics of the PSyIR tree.

For example, if we want to create a node that can be found anywhere where a
statement is valid, and in turn it accepts one and only one DataNode as a
child, we would write something like:


    .. literalinclude:: code_snippets/newnode.py
        :language: python
        :lines: 47-60

This implementation already provides the basic PSyIR functionality and the
node can be integrated and used in the PSyIR tree:

::

    >>> mynode = MyNode(children=[Literal("1", INTEGER_TYPE)])

    >>> mynode.children.append(Literal("2", INEGER_TYPE))
    ...
    psyclone.errors.GenerationError: Generation Error: Item 'Literal' can't be
    child 1 of 'MyNodeName'. The valid format is: 'DataNode'.

    >>> schedule.addchild(mynode)

    >>> print(schedule.view())
    Schedule[]
        MyNodeName[]
                Literal[value:'1', Scalar<INTEGER, UNDEFINED>]

For a full list of methods available in any PSyIR node see the
:ref_guide:`Node reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.Node`.

.. note:: For convenience, the PSyIR children validation is performed
    with both: Node methods (e.g. ``node.addchild()``) and also list
    methods (e.g. ``node.children.extend([node1, node2])``).

    To achieve this, we sub-classed the Python list and redefined all
    methods that modify the list by calling first the PSyIR provided
    validation method and subsequently, if valid, calling the associated
    list method.

.. _nodesinfo-label:

The parent-child relationship
=============================

To facilitate the PSyIR tree navigation, the parent-child relationship between
nodes is represented with a double reference (providing ``node.parent`` and
``node.children`` navigational properties).

However, to maintain the consistency of the double reference, we don't
allow the node API to manually specify its ``parent`` reference. It is
always the responsibility of a parent node to update the ``parent``
reference of its children.  To make this possible for any operation
applied to the ``node.children`` list, we provide this functionality
in the same list subclass specialisation that does the child
validation checks explained in the previous section. Therefore, all
the following list operations will work as expected:

.. code-block:: python

    node.children.insert(node1)  # Will set node1.parent reference to node
    node.children.extend([node2, node3])  # Will set node2 and node3 parent
                                          # references to node
    del node.children[1]  # Will unset the parent reference of children[1]
    node.children = []  # Will unset the parent references of all its previous
                        # children
    node.detach()  # Will ask node.parent to free node, as node can't change
                   # the connection by itself

The only exception to the previous consistency rule is when a node constructor
is given the parent reference when building a PSyIR tree top-down. In this
case, the single-direction reference will be accepted temporarily, but a child
connection operation will need to be done eventually to satisfy the other
part of the connection. Any attempt to insert the new node as a child of
another node not specified in the constructor will fail as this would break
the consistency with the predefined parent reference. For example:

.. code-block:: python

    assignment = Assignment()
    rhs = Reference(symbol1, parent=assignment)  # Predefined parent reference
    lhs = Reference(symbol2, parent=assignment)  # Predefined parent reference
    assignment.children = [lhs, rhs]  # Finalise parent-child relationship

    node = Reference(symbol3, parent=assignment)
    lhs.addchild(node)  # Will produce a Generation error because the node
                        # constructor specified that its parent would be the
                        # 'assignment' node
    

Note that a node which already has a parent won't be accepted as a child of
another node, as this could break any previously existing parent-child
relationship.

.. code-block:: python

    node1.children.insert(child)  # Valid
    node2.children.insert(child)  # Will produce a GenerationError

Methods like ``node.detach()``, ``node.copy()`` and ``node.pop_all_children()``
can be used to move or replicate existing children into different nodes. 


Selected Node Descriptions
==========================

ScopingNode
-----------

A `ScopingNode` is an abstract class node that defines a scoping region,
this node and all its descendants have access to a shared set of symbols.
These symbols are described in the `SymbolTable`
(`psyclone.psyir.symbols.SymbolTable`) attached to this node.

There is a double-link between the `ScopingNode` (through the ``symbol_table``
property) and the `SymbolTable` (through the ``scope`` property) objects. To
maintain a consistent connection between both objects the only public methods
to update the connections are the ``attach`` and ``detach`` methods of
``SymbolTable`` (which takes care of both sides of the connection).

Also note that the constructor will not accept as a parameter a symbol table
that already belongs to another scope. The symbol table will need to be
detached or deep copied before it can be assigned to the new ScopingNode.

See the full API in the
:ref_guide:`ScopingNode reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.ScopingNode`.

.. _container-label:

Container
^^^^^^^^^

The `Container` node is a `ScopingNode` that contains one or more `Container`
and/or `Routine` nodes.
A `Container` can be used to capture a hierarchical grouping of
`Routine` nodes and a hierarchy of `Symbol` scopes i.e. a `Symbol`
specified in a `Container` is visible to all `Container` and
`Routine` nodes within it and their descendants.
See the full Container API in the
:ref_guide:`Container reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.Container`.

FileContainer
^^^^^^^^^^^^^

The `FileContainer` node is a subclass of the `Container` node and is
used to capture the concept of a file that contains one or more
`Container` and/or `Routine` nodes. Whilst this structure is the same
as for a `Container`, it is useful to distinguish between the two as
backends may need to deal differently with a `FileContainer` and a
`Container`.

A `FileContainer` is always created at the root of the PSyIR tree when
parsing Fortran code, as a Fortran file can contain one or more
program units (captured as `Containers` and/or `Routines`).  PSyIR
tree when parsing Fortran code, as Fortran code has the concept of a
program (captured as a `FileContainer`) that can contain one or more
program units (captured as `Containers` and/or `Routines`).  See the
full `FileContainer` API in the :ref_guide:`FileContainer reference guide
psyclone.psyir.nodes.html#psyclone.psyir.nodes.FileContainer`.

Schedule
^^^^^^^^

The `Schedule` is a `ScopingNode` that represents a sequence of statements.
See the full `Schedule` API in the :ref_guide:`Schedule reference guide
psyclone.psyir.nodes.html#psyclone.psyir.nodes.Schedule`.

Routine
^^^^^^^

The `Routine` node is a subclass of `Schedule` that represents any program
unit (subroutine, function or main program). As such it extends `Schedule`
through the addition of the `return_symbol` (required when representing a
function) and `is_program` properties.
It also adds the `create` helper method for constructing a valid
`Routine` instance. It is an important node in PSyclone because two of its
specialisations: `InvokeSchedule` and `KernelSchedule` (described below),
are used as the root nodes of PSy-layer invokes and kernel subroutines.
This makes them the starting points for any walking of the PSyIR tree in
PSyclone transformation scripts and a common target for the application of
transformations.

InvokeSchedule
^^^^^^^^^^^^^^

The `InvokeSchedule` is a PSyIR node that represents an invoke subroutine in
the PSy-layer. It specialises the `psyclone.psyir.nodes.Routine` functionality
with a reference to its associated `psyclone.psyGen.Invoke` object.

.. note:: This class will be renamed to `InvokeRoutine` in issue #909.


.. _kernel_schedule-label:

KernelSchedule
^^^^^^^^^^^^^^

The `KernelSchedule` is a PSyIR node that represents a Kernel
subroutine. As such it is a subclass of `psyclone.psyir.nodes.Routine`
with `return_type` set to `None` and `is_program` set to `False`.

.. note:: This class will be renamed to `KernelRoutine` in issue #909.

Control-Flow Nodes
------------------

The PSyIR has three control flow nodes: `IfBlock`, `Loop` and
`Call`. These nodes represent the canonical structure with which
conditional branching constructs, iteration constructs and accessing
other blocks of code are built. Additional language-specific syntax
for branching and iteration will be normalised to use these same
constructs.  For example, Fortran has the additional branching
constructs `ELSE IF` and `CASE`: when a Fortran code is translated
into the PSyIR, PSyclone will build a semantically equivalent
implementation using `IfBlocks`.  Similarly, Fortran also has the
`WHERE` construct and statement which are represented in the PSyIR
with a combination of `Loop` and `IfBlock` nodes. Such nodes in the
new tree structure are annotated with information to enable the
original language-specific syntax to be recreated if required (see
below).  See the full IfBlock API in the :ref_guide:`IfBlock reference
guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.IfBlock`. The
PSyIR also supports the concept of named arguments for `Call` nodes,
see the :ref:`named_arguments-label` section for more details.

.. note:: A Call node (like the CodeBlock) inherits from both
          Statement and DataNode because it can be found in Schedules
          or inside Expressions, however this has some shortcomings,
          see issue #1437.

Control-Flow Node annotation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

.. note:: A `Loop` may currently only be given the `was_single_stmt`
	  annotation if it also has the `was_where` annotation. (Thus
	  indicating that this `Loop` originated from a WHERE
	  *statement* in the original Fortran code.) The PSyIR
	  represents Fortran single-statement loops (often called
	  array notation) as arrays with ranges in the appropriate
	  indices.

Loop Node
^^^^^^^^^

The `Loop` node is the cannonical representation of a bounded loop, it
has the start, stop, step and loop_body of the loop as its children. The
node has the same semantics than the Fortran do construct: the boundary
values are inclusive (both are part of the iteration space) and the start,
stop and step expressions are evaluated just once at the beginning of the
loop.

For more details on the `Loop` node, see the full API in the
:ref_guide:`reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.Loop`.


Ranges
------

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
this argument would be represented by an `ArrayReference` node with the first
entry in its `shape` being an integer `Literal` (with value 1) and the
second entry being a `Range`. In this case the `Range` will have a
start value of `LBOUND(my_array, 1)`, a stop value of
`UBOUND(my_array, 1)` and a step of `Literal("1")`. Note that `LBOUND`
and `UBOUND` will be
instances of `BinaryOperation`. (For the particular code fragment
given above, the values are in fact known [1 and 5, respectively] and
could be obtained by querying the Symbol Table.)

See the full Range API in the
:ref_guide:`Range reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.Range`.

Operation Nodes
---------------

Arithmetic operations and various intrinsic/query functions are represented
in the PSyIR by sub-classes of the `Operation` node. The operations are
classified according to the number of operands:

- Those having one operand are represented by
  `psyclone.psyir.nodes.UnaryOperation` nodes,

- those having two operands are represented by
  `psyclone.psyir.nodes.BinaryOperation` nodes.

- and those having more than two or a variable number of operands are
  represented by `psyclone.psyir.nodes.NaryOperation` nodes.

See the documentation for each Operation class in the
:ref_guide:`Operation psyclone.psyir.nodes.html#psyclone.psyir.nodes.Operation`,
:ref_guide:`UnaryOperation psyclone.psyir.nodes.html#psyclone.psyir.nodes.UnaryOperation`,
:ref_guide:`BinaryOperation psyclone.psyir.nodes.html#psyclone.psyir.nodes.BinaryOperation` and
:ref_guide:`NaryOperation psyclone.psyir.nodes.html#psyclone.psyir.nodes.NaryOperation`
sections of the reference guide.

Note that where an intrinsic (such as
Fortran's `MAX`) can have a variable number of arguments, the class
used to represent it in the PSyIR is determined by the actual number
of arguments in a particular instance. e.g. `MAX(var1, var2)` would be
represented by a `psyclone.psyir.nodes.BinaryOperation` but `MAX(var1,
var2, var3)` would be represented by a
`psyclone.psyir.nodes.NaryOperation`.

The PSyIR supports the concept of named arguments for operation
nodes, see the :ref:`named_arguments-label` section for more details.

CodeBlock Node
--------------

The PSyIR CodeBlock node contains code that has no representation in
the PSyIR. It is useful as it allows the PSyIR to represent complex
code by using CodeBlocks to handle the parts which contain unsupported
language features. One approach would be to work towards capturing all
language features in the PSyIR, which would gradually remove the need
for CodeBlocks. However, the purpose of the PSyIR is to capture code
concepts that are relevant for performance, not all aspects of a code,
therefore it is likely that CodeBlocks will continue to be an
important part of the PSyIR.
See the full Codeblock API in the
:ref_guide:`CodeBlock reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.CodeBlock`.

The code represented by a CodeBlock is currently stored as a list of
fparser2 nodes. Therefore, a CodeBlock's input and output language is
limited to being Fortran. This means that only the fparser2 front-end
and Fortran back-end can be used when there are CodeBlocks within a
PSyIR tree. In theory, language interfaces could be written between
CodeBlocks and other PSyIR Nodes to support different back-ends but
this has not been implemented.

Currently PSyIR have a single CodeBlock node that can be found
in place of full Statements or being part of an expression that
evaluates to a DataNode. To make this possible CodeBlock is a subclass
of both: Statement and DataNode. However, in certain situations we
still need to differentiate which one it is, for instance the Fortran
back-end needs this information, as expressions do not need indentation
and a newline whereas statements do.
For this reason, CodeBlock has a ``structure`` method that indicates
whether the code contains one or more unrecognized language expressions
or one or more statements (which may themselves contain expressions).

The Fortran front-end populates the ``structure`` attribute using a
feature of the fparser2 node list that is if the first node in the
list is a statement then so are all the other nodes in the list and
that if the first node in the list is an expression then so are all
the other nodes in the list. This allows the ``structure`` method to
return a single value that represents all nodes in the list.
The structure of the PSyIR hierarchy is used to determine whether the
code in a CodeBlock contains expressions or statements. This is
achieved by looking at the parent PSyIR Node. If the parent Node is a
Schedule then the CodeBlock contains one or more statements, otherwise
it contains one or more expressions.

This logic works for existing PSyIR nodes and relies on any future PSyIR
nodes being constructed so this continues to be true. Another solution
would be to have two different nodes: StatementsCodeBlock which subclasses
Statement, and DataCodeBlock which subclasses DataNode. We have chosen the
first implementation for the simplicity of having a single PSyIR node instead
of two, but if things get more complicated using this implementation, the
second alternative could be considered again.

ArrayMixin
----------

``ArrayMixin`` is an abstract "mix-in" base class which implements
various methods that are specific to those nodes representing arrays
and array accesses.  It is subclassed by ``ArrayReference``,
``ArrayOfStructuresReference``, ``ArrayMember`` and
``ArrayOfStructuresMember``.

Reference Node
--------------

The PSyIR ``Reference`` Node represents a variable access. It keeps
a reference to a ``Symbol`` which will be stored in a symbol table.
See the full ``Reference`` API in the
:ref_guide:`Reference reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.Reference`.

ArrayReference Node
-------------------

The PSyIR ``ArrayReference`` Node represents an access to one or more
elements of an array variable. It keeps a reference to a Symbol which
will be stored in a symbol table. The indices used to access the array
element(s) are represented by the children of the node. The
``ArrayReference`` Node inherits from both the ``Reference`` and
``ArrayMixin`` classes.  See the full API in the :ref_guide:`ArrayReference
reference guide psyclone.psyir.nodes.html#psyclone.psyir.nodes.ArrayReference`.

Directive
---------
The PSyIR ``Directive`` Node represents a Directive, such as is used
in OpenMP or OpenACC. There are two subclasses, ``RegionDirective``
and ``StandaloneDirective``. ``RegionDirective`` nodes contain a
schedule as their first child, which contains the code segment covered
by the directive, for example a ``Loop`` for which an OpenMP parallel
do may be applied to.
Both ``RegionDirective`` and ``StandaloneDirective`` may also have
``Clause`` nodes as children, and can be accessed through the ``clauses``
member. See the full API in the :ref_guide:`Directive reference guide
psyclone.psyir.nodes.html#psyclone.psyir.nodes.Directive`.

.. _named_arguments-label:

Named arguments
---------------

The `Call` node and the three subclasses of the `Operation` node
(`UnaryOperation`, `BinaryOperation` and `NaryOperation`) all support
named arguments.

The argument names are provided by the `argument_names` property. This
property returns a list of names. The first entry in the list refers
to the first argument, the second entry in the list refers to the
second argument, etc. An argument name is stored as a string. If an
argument is not a named argument then the list entry will contain
`None`. For example, for the following call::

    call example(arg0, name1=arg1, name2=arg2)

the following list would be returned by the `argument_names` property::

    [None, "name1", "name2"]

It was decided to implement it this way, rather than adding a new
(`NamedArgument`) node, as 1) there is no increase in the number and
types of PSyIR nodes and 2) iterating over all children (the
arguments) of these nodes is kept simple.

The following methods support the setting and updating of named
arguments:  `create()`, `append_named_arg()`, `insert_named_arg()` and
`replace_named_arg()`.

However, this implementation raises consistency problems as it is
possible to insert, modify, move or delete children (argument) nodes
directly. This would make the argument names list inconsistent as the
names themselves are stored within the node.

To solve this problem, the argument names are stored internally in an
`_argument_names` list which not only keeps the argument names but
also keeps a reference (the `id`) to the associated child argument. An
internal `_reconcile()` method then checks whether the internal
`_argument_names` list and the actual arguments match and fixes any
inconsistencies.

The `_reconcile()` method is called before the `argument_names`
property returns its values, thereby ensuring that any access to
`argument_names` is always consistent.

The `_reconcile()` method looks through the arguments and tries to
match them with one of the stored id's. If there is no match it is
assumed that this is not a named argument. This approach has the
following behaviour: the argument names are kept if arguments are
re-ordered; an argument that has replaced a named argument will not be
a named argument; an inserted argument will not be a named argument,
and the name of a deleted named argument will be removed.

Making a copy of the `Call` node or one of the three subclasses of
Operation nodes (`UnaryOperation`, `BinaryOperation` or
`NaryOperation`) also causes problems with consistency between the
internal `_argument_names` list and the arguments. The reason for this
is that the arguments get copied and therefore have a different `id`,
whereas the `id`s in the internal `_argument_names` list are simply
copied. To solve this problem, the `copy()` method is specialised to
update the `id`s. A second issue is that the internal
`_argument_names` list may already be inconsistent when a copy is
made. Therefore the `_reconcile()` method is also called in the
specialisation of the `copy()` method.


References to Structures and Structure Members
----------------------------------------------

The PSyIR has support for representing references to symbols of
structure type and to members of such structures. Since the former
case is still a reference to a symbol held in a symbol table, it is
already captured by the ``Reference`` node. A reference that includes
an access to a member of a structure is described by a
``StructureReference`` which is a subclass of ``Reference``.  As such,
it has a ``symbol`` property which gives the ``Symbol`` that the
reference is to. The *member* of the structure being accessed is
described by a `Member`_ (or subclass) which is stored as the
first and only child of the ``StructureReference``. The full API is
given in the :ref_guide:`StructureReference section of the reference guide
psyclone.psyir.nodes.html#psyclone.psyir.nodes.StructureReference`.

Similarly, ``ArrayOfStructuresReference`` represents a reference to a
*member* of one or more elements of an array of structures. As such it
subclasses both ``ArrayMixin`` and ``StructureReference``.  As with the
latter, the first child describes the member being accessed and will
be an instance of (a subclass of) ``Member``. Subsequent
children (of which there must be at least one since this is an array
reference) then describe the array-index expressions of the reference
in the usual fashion for an ``ArrayReference``.  The full API is given
in the :ref_guide:`ArrayOfStructuresReference section of the reference guide
psyclone.psyir.nodes.html#psyclone.psyir.nodes.ArrayOfStructuresReference`.

Since *members* of structures are not represented by symbols in a symbol
table, references to them are *not* subclasses of ``Reference``. They are
instead represented by instances of ``Member`` (or subclasses
thereof). There are four of these:

============================= ===============================================
Class                         Type of Accessor Nested Inside
============================= ===============================================
Member                        No nested accessor (i.e. is a leaf)
ArrayMember                   One or more elements of an array
StructureMember               Member of a structure
ArrayOfStructuresMember       Member of one or more elements of an array of
                              structures
============================= ===============================================

These classes are briefly described below. For full details please follow the
appropriate links to the Reference Guide.

Member
^^^^^^

This node is used for accesses to members of a structure which do not contain
any further accesses nested inside. In a PSyIR tree, any instance of this node
type must therefore have no children and a ``StructureReference`` or
``StructureMember`` (or subclasses thereof) as parent. The full API is given
in the
:ref_guide:`Member section of the reference guide
psyclone.psyir.nodes.html#psyclone.psyir.nodes.Member`.

ArrayMember
^^^^^^^^^^^

This node represents an access to one or more elements of an array
within a structure. As such, it subclasses both
``Member`` and ``ArrayMixin``. Its children follow the same rules
as for an `ArrayReference Node`_. The full API is given in the
:ref_guide:`ArrayMember section of the reference guide
psyclone.psyir.nodes.html#psyclone.psyir.nodes.ArrayMember`.

StructureMember
^^^^^^^^^^^^^^^

This node represents an access to a member of a structure that is
itself a member of a structure. As such, it has a single child which subclasses
``Member`` and specifies which component is being accessed. The full API
is given in the
:ref_guide:`StructureMember section of the reference guide
psyclone.psyir.nodes.html#psyclone.psyir.nodes.StructureMember`.

ArrayOfStructuresMember
^^^^^^^^^^^^^^^^^^^^^^^

This node represents an access to a member of one or more elements of an array
of structures that is itself a member of a structure. Its first child must be a
subclass of ``Member``. Subsequent children represent the index expressions
for the array access. The full API is given in the
:ref_guide:`ArrayOfStructuresMember section of the reference guide psyclone.psyir.nodes.array_of_structures_member.html`.

Comments attached to PSyIR Nodes
================================

Since the PSyIR is designed to support source-to-source code generation, it is
desirable to keep the output code as readable as possible, and this includes
keeping or adding comments to the generated code.
Comments are not first-class nodes in the PSyIR because it is an abstract
syntax tree and it was preferable to hide the complexity of comment nodes from
the PSyIR transformations and other manipulations. Therefore, comments have
been implemented as string attributes (one for preceding and another for inline
comments) attached to particular nodes. And thus the location of comments on a
PSyIR tree will move together with their owning node.

The group of nodes that can contain comments does not have an exclusive
common ancestor, so they have been implemented with a Mixin class called
CommentableMixin. A node can keep track of comments if it inherits from this
class, for example:

.. code-block:: python

    from psyclone.psyir.nodes.commentable_mixin import CommentableMixin

    class MyNode(Node, CommentableMixin):
        ''' Example node '''

    mynode = MyNode()
    mynode.preceding_comment = "A preceding comment"
    mynode.inline_comment = "An inline comment"

From the language-level PSyIR nodes, Container, Routine and Statement have
the CommentableMixin trait.

Domain-Specific PSyIR
=====================

The discussion so far has been about generic language-level
PSyIR. This is located in the ``psyir`` directory and contains nodes,
symbols, transformations, front-ends and back-ends. None of this is
domain specific.

To obtain domain-specific concepts the language-level PSyIR can be
specialised or extended. All domains follow the PSyKAl separation of
concerns with the Algorithm-layer and the PSy-layer having its own
domain-specific concepts, this can be found in
``psyclone.domain.common.algorithm`` and ``psyclone.domain.common.psylayer``
respectively (some concepts are still on ``psyclone.psyGen`` for legacy
reasons but will be moved to the new locations over time).

PSy-layer concepts
------------------

* The `PSyLoop` is a `Loop` where the boundaries are given by the domain
  specific iteration space that the kernels are applied to. In turn it is
  sub-classed in all of the domains supported by PSyclone. This then allows
  the class to be configured with a list of valid loop 'types'. For instance,
  the GOcean sub-class, `GOLoop`, has "inner" and "outer" while the LFRic
  (dynamo0.3) sub-class, `DynLoop`, has "dofs", "colours", "colour", ""
  and "null". The default loop type (iterating over cells) is here
  indicated by the empty string. The concept of a "null" loop type is
  currently required because the dependency analysis that determines the
  placement of halo exchanges is handled within the `Loop` class. As a
  result, every `Kernel` call must be associated with a `Loop` node.
  However, the LFRic domain has support for kernels which operate on the
  'domain' and thus do not require a loop over cells or dofs in the
  generated PSy layer. Supporting a `DynLoop` of "null" type allows us
  to retain the dependence-analysis functionality within the `Loop`
  while not actually producing a loop in the generated code. When
  `#1148 <https://github.com/stfc/PSyclone/issues/1148>`_ is tackled,
  the dependence-analysis functionality will be removed from
  the `Loop` class and this concept of a "null" loop can be dropped.
* The `Kern`, which can be of type `CodedKern`, `InlinedKern` or `BuiltIn`
  are the singular units of computation that can be found inside a
  `PSyLoop`.
* The `HaloExchange` is a distributed-memory concept in the PSy-layer.
* The `GlobalSum` is a distributed-memory concept in the PSy-layer.


Other specializations
---------------------

In LFRic there are specialisations for
kernel-layer datatypes and symbols. For the algorithm layer in both
GOcean1.0 and LFRic there are specialisations for invokes and kernel
calls. This is discussed further in the following sections.




The LFRic PSyIR
===============

The LFRic PSyIR is a set of subclasses of the PSyIR which captures
LFRic-specific routines, datatypes and associated symbols. These
subclasses are work in progress and at the moment are limited to 1) a
subset of the datatypes passed into LFRic kernels by argument and by
use association and 2) LFRic calls (InvokeCall and KernCall) in the
LFRic algorithm-layer. Over time these will be expanded to support a)
all LFRic kernel datatypes, b) all LFRic PSyIR datatypes, c)
subroutines (KernRoutine etc), d) derived quantities e.g. iterator
variables and eventually e) higher level LFRic PSyIR concepts, which
will not be concerned with symbol tables and datatypes.

The Kernel-layer subclasses will be used to:

1) check that the data types, dimensions, intent etc. of a coded
   kernel's subroutine arguments conform to the expected datatypes,
   dimensions, intent etc as defined by the kernel metadata and
   associated LFRic rules.

2) represent coded kernels, which will make it easier to reason about
   the structure of a kernel. At the moment a coded kernel is
   translated into generic PSyIR. This generic PSyIR will be further
   translated into LFRic PSyIR using the expected datatypes as
   specified by the kernel metadata and associated LFRic rules.

3) replace the existing kernel stub generation implementation so that
   the PSyIR back ends can be used and PSyclone will rely less on
   ``f2pygen`` and ``fparser1``. At the moment ``kernel_interface``
   provides the same functionality as ``kern_stub_arg_list``, except
   that it uses the symbol table (which keeps datatypes and their
   declarations together).

4) generate the PSy-layer, replacing the existing
   ``kern_call_arg_list`` and ``gen_call`` routines.

The Algorithm-layer subclasses will be used to:

1) help with transforming the algorithm layer.

2) help with reasoning about the algorithm layer e.g. to check that
   the algorithm layer and kernel metadata match.

3) generate the LFRic Algorithm-layer PSyIR e.g. in psyclone-kern.

Algorithm-layer Classes
-----------------------

The LFRic PSyIR for the Algorithm layer is captured in
``domain/lfric/algorithm/psyir.py``. Three classes are currently
provided, one to capture an invoke call, ``LFRicAlgorithmInvokeCall``
and two to capture Builtin and (coded) Kernel calls within an invoke
call, ``LFRicBuiltinFunctor`` and ``LFRicKernelFunctor`` respectively.

Kernel-layer Classes
--------------------

The LFRic PSyIR for the Kernel layer is captured in
``domain/lfric/psyir.py``. The relevant classes are generated to avoid
boilerplate code and to make it simpler to change the LFRic
infrastructure classes in the future.

The idea is to declare different classes for the different
concepts. For example ``NumberOfDofsDataType()`` and
``NumberOfDofsDataSymbol()`` classes are created and these are
subclasses of ``DataType`` and ``DataSymbol`` respectively. In
``NumberOfDofsDataType`` the ``intrinsic`` and ``precision``
properties are pre-defined, as is the fact that it is a scalar, so
these do not need to be specified. All that is needed to create a
``undf`` symbol is a name and the function space it represents::

  UNDF_W3 = NumberOfUniqueDofsDataSymbol("undf_w3", "w3")

For arrays, (e.g. for ``FieldData``) the dimensions must also be
provided::

  UNDF_W3 = NumberOfUniqueDofsDataSymbol("undf_w3", "w3")
  FIELD1 = RealFieldDataDataSymbol("field1", [UNDF_W3], "w3")

At the moment, argument types and values are also not checked e.g. the
function space argument - see issue #926. There is also no consistency
checking between specified function spaces (e.g. that ``UNDF_W3`` is
for the same function space as ``FIELD1`` in the above example) - see
issue #927. Also, the function space attribute would be better if it
were a class, rather than using a string, see issue #934.

Currently entities which can have different intrinsic types
(e.g. ``FieldData``) are captured as different classes
(``RealFieldDataDataSymbol``, ``IntegerFieldDataDataSymbol``
etc). This could be modified if a single class turns out to be
preferable.

Kernel arguments
----------------
   
At the moment, kernel arguments are generated by the
``KernStubArgList`` or ``KernCallArgList`` classes. However, whilst
these classes generate the correct number of arguments in the correct
order, they have no knowledge of the datatypes that the arguments
correspond to and how the arguments relate to each other (they just
output strings).

The logic and declaration of kernel variables is handled separately by
the ``gen_stub`` method in ``DynKern`` and the ``gen_code`` method in
``DynInvoke``. In both cases these methods make use of the subclasses
of ``DynCollection`` to declare variables.

When using the symbol table in the LFRic PSyIR we naturally capture
arguments and datatypes together. The ``KernelInterface`` class is
aiming to replicate the ``KernStubArgList`` class and makes use of
the LFRic PSyIR. The idea is that the former will replace the latter
when it has the same or more functionality. At the moment, only
methods required to pass the tests have been implemented in
``KernelInterface`` so there is more to be done, but it is also not
clear what the limitations are for ``KernStubArgList``.

Eventually the definition of lfric datatypes should be moved to the
LFRic PSyIR, but at the moment there is a lot of information defined
in the ``DynCollection`` subclasses. This will need to be addressed
over time.

The GOcean PSyIR
================

GOcean makes use of algorithm-layer PSyIR specialisations.

Algorithm-layer Classes
-----------------------

The GOcean PSyIR for the Algorithm layer is captured in
``domain/common/algorithm/psyir.py``. Two classes are currently
provided, one to capture an invoke call, ``AlgorithmInvokeCall``
and the other to capture (coded) Kernel calls within an invoke
call, ``KernelFunctor``.
