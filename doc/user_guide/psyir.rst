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
.. Written by A. R. Porter, STFC Daresbury Lab
.. Modified by R. W. Ford, STFC Daresbury Lab
      
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
	  expectation that ``gen_code()`` and ``update()`` methods will
	  be removed.

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
subclassed into a kernel-layer ``KernelSchedule``. In addition to
``KernelSchedule``, Kernel-layer PSyIR nodes are: ``Loop``,
``IfBlock``, ``CodeBlock``, ``Assignment``, ``Range``, ``Reference``,
``Operation``, ``Literal``, ``Return`` and ``Container``. The
``Reference`` class is further subclassed into ``Array`` and the
``Operation`` class is further subclassed into ``UnaryOperation``,
``BinaryOperation`` and ``NaryOperation``.


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
runme_openmp.py>`_ is rendered as:

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
- ``Directive``:
   .. automethod:: psyclone.psyGen.Directive.dir_body()

These are the recommended methods to navigate the tree for analysis or
operations that depend on the Node type.

Additionally, the `walk` method (available in all nodes) is able to recurse
through the tree and return objects of a given type. This is useful when the
objective is to move down the tree to a specific node or list of nodes without
information about the exact location.

.. automethod:: psyclone.psyir.nodes.Node.walk


DataTypes
=========

The PSyIR supports scalar and array datatypes. These datatypes are
used when creating instances of DataSymbol and Literal.

Scalar DataTypes
----------------

A Scalar datatype consists of an intrinsic and a precision.

The intrinsic can be one of ``INTEGER``, ``REAL``, ``BOOLEAN`` and
``CHARACTER``.

The precision can be ``UNDEFINED``, ``SINGLE``, ``DOUBLE``, an integer
value specifying the precision in bytes, or a datasymbol (see Section
:ref:`symbol-label`) that contains precision information. Note that
``UNDEFINED``, ``SINGLE`` and ``DOUBLE`` allow the precision to be set
by the system so may be different for different architectures. For
example:

::

   > char_type = ScalarType(ScalarType.Intrinsic.CHARACTER,
   >                        ScalarType.Precision.UNDEFINED)
   >
   > int_type = ScalarType(ScalarType.Intrinsic.INTEGER,
   >                       ScalarType.Precision.SINGLE)
   >
   > bool_type = ScalarType(ScalarType.Intrinsic.BOOLEAN, 4)
   >
   > symbol = DataSymbol("rdef", int_type, constant_value=4)
   > scalar_type = ScalarType(ScalarType.Intrinsic.REAL, symbol)

For convenience PSyclone predefines a number of scalar datatypes:

``REAL_TYPE``, ``INTEGER_TYPE``, ``BOOLEAN_TYPE`` and
``CHARACTER_TYPE`` all have precision set to ``UNDEFINED``;

``REAL_SINGLE_TYPE``, ``REAL_DOUBLE_TYPE``, ``INTEGER_SINGLE_TYPE``
and ``INTEGER_DOUBLE_TYPE``;

``REAL4_TYPE``, ``REAL8_TYPE``, ``INTEGER4_TYPE`` and
``INTEGER8_TYPE``.

Array DataTypes
----------------

An Array datatype has a scalar datatype specifying the type of its
elements and a shape. The shape can have an arbitrary number of
dimensions. Each dimension captures what is known about its extent. It
is necessary to distinguish between four cases:

.. tabularcolumns:: |p{9cm}|L|

+--------------------------------------------+--------------------------------+
|Description                                 | Entry in ``shape`` list        |
+============================================+================================+
|An array has a static extent known at       | Integer ``Literal``            |
|compile time.                               |                                |
+--------------------------------------------+--------------------------------+
|An array has an extent defined by another   | ``Symbol``                     |
|symbol.                                     |                                |
+--------------------------------------------+--------------------------------+
|An array has a definite extent which is not | ``ArrayType.Extent.ATTRIBUTE`` |
|known at compile time but can be queried    |                                |
|at runtime.                                 |                                |
+--------------------------------------------+--------------------------------+
|It is not known whether an array has memory | ``ArrayType.Extent.DEFERRED``  |
|allocated to it in the current scoping unit.|                                |
+--------------------------------------------+--------------------------------+

The distinction between the last two cases is that in the former the
extents are known but are kept internally with the array (for example
an assumed shape array in Fortran) and in the latter the array has not
yet been allocated any memory (for example the declaration of an
allocatable array in Fortran) so the extents may have not been defined
yet.

For example:

::

   > array_type = ArrayType(REAL4_TYPE, [5, 10])
   >
   > n_var = DataSymbol("n", INTEGER_TYPE)
   > array_type = ArrayType(INTEGER_TYPE, [n_var, n_var])
   >
   > array_type = ArrayType(REAL8_TYPE, [ArrayType.Extent.ATTRIBUTE,
   >                                     ArrayType.Extent.ATTRIBUTE])
   >
   > array_type = ArrayType(LOGICAL_TYPE, [ArrayType.Extent.DEFERRED])

.. _symbol-label:

Symbols and Symbol Tables
=========================

Some PSyIR nodes have an associated Symbol Table
(`psyclone.psyir.symbols.SymbolTable`) which keeps a record of the
Symbols (`psyclone.psyir.symbols.Symbol`) specified and used within them.
The ``SymbolTable`` has the following interface:

.. autoclass:: psyclone.psyir.symbols.SymbolTable
    :members:

Where each element is a ``Symbol`` with an immutable name:

.. autoclass:: psyclone.psyir.symbols.Symbol
    :members:

There are several ``Symbol`` sub-classes to represent different
labeled entities in the PSyIR. At the moment the available symbols
are:

- .. autoclass:: psyclone.psyir.symbols.ContainerSymbol

- .. autoclass:: psyclone.psyir.symbols.DataSymbol

Creating PSyIR
==============

Symbol names
------------

PSyIR symbol names can be specified by a user. For example::

   > var_name = "my_name"
   > symbol_table = SymbolTable()
   > data = DataSymbol(var_name, REAL_TYPE)
   > symbol_table.add(data)
   > reference = Reference(data)

However, the ``SymbolTable`` ``add()`` method will raise an exception if a
user tries to add a symbol with the same name as a symbol already existing
in the symbol table. Therefore it is the responsibility of the user to
determine unique names when manually specifying them.

The ``SymbolTable`` class also provides the ``new_symbol_name()`` method (see
Section :ref:`symbol-label` for more details) to avoid this
problem. The method provides a name that is guaranteed to be distinct
from any existing names in the symbol table. The name returned is the
value of the ``PSYIR_ROOT_NAME`` variable specified in the ``DEFAULT``
section of the PSyclone config file, followed by an optional "_" and
an integer. For example, the following code::

  > from psyclone.psyir.symbols import DataSymbol, SymbolTable, REAL4_TYPE
  > symbol_table = SymbolTable()
  > for i in range(0, 3):
  >     var_name = symbol_table.new_symbol_name()
  >     symbol_table.add(DataSymbol(var_name, REAL4_TYPE))
  >     print (var_name)

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
readable names, the ``new_symbol_name()`` method allows the user to specify a
root name as an argument to the method which then takes the place of
the default root name. For example, the following code::
  
  > from psyclone.psyir.symbols import DataSymbol, SymbolTable, REAL_SINGLE_TYPE
  > symbol_table = SymbolTable()
  > for i in range(0, 3):
  >     var_name = symbol_table.new_symbol_name(root_name="something")
  >     symbol_table.add(DataSymbol(var_name, REAL_SINGLE_TYPE))
  >     print (var_name)

gives the following output::
  
  something
  something_0
  something_1

An example of using the ``new_symbol_name()`` method can be found in the
PSyclone ``examples/psyir`` directory.

Nodes
-----

PSyIR nodes are connected together via parent and child methods
provided by the ``Node`` baseclass.

These nodes can be created in isolation and then connected
together. For example::

    > ...
    > assignment = Assignment()
    > literal = Literal("0.0", REAL_TYPE)
    > reference = Reference(symbol)
    > literal.parent = assignment
    > reference.parent = assignment
    > assignment.children = [reference, literal]
    
However, as connections get more complicated, creating the correct
connections can become difficult to manage and error prone. Further,
in some cases children must be collected together within a
``Schedule`` (e.g. for ``IfBlock`` and for ``Loop``).

To simplify this complexity, each of the Kernel-layer nodes which
contain other nodes have a static ``create`` method which helps
construct the PSyIR using a bottom up approach. Using this method, the
above example then becomes::

    > ...
    > literal = Literal("0.0", REAL_TYPE)
    > reference = Reference(symbol)
    > assignment = Assignment.create(reference, literal)

A more complete example of using this approach can be found in the
PSyclone ``examples/psyir`` directory.
