.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019, Science and Technology Facilities Council.
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

The PSyclone Internal Representation (PSyIR)
============================================

The PSyIR is at the heart of PSyclone, representing code (at both the
PSy- and kernel-layer levels) in a language-agnostic form. A PSyIR may
be constructed from scratch (in Python) or by processing exising
source code using a frontend. Transformations act on the PSyIR and
ultimately the generated code is produced by one of the PSyIR's
backends.

PSyIR Nodes
-----------

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
^^^^^^^^^^^^^^^

PSy-layer PSyIR classes are primarily used to create the
PSy-layer. These tend to be relatively descriptive and do not specify
how a particular PSyclone frontend would implement them. With the
exception of ``Loop``, these classes are currently not compatible with
the PSyIR backends. The generic (non-api-specific) PSyIR nodes are:
``Schedule``, ``Directive``, ``GlobalSum``, ``HaloExchange``, ``Loop``
and ``Kern``. The ``Schedule`` class is further subclassed into
``InvokeSchedule``. The ``Directive`` class is subclassed into many
directives associated with OpenMP and OpenACC. The ``Kern`` class is
subclassed into ``CodedKern``, ``InlinedKern`` and ``BuiltinKern``.


Kernel-layer nodes
^^^^^^^^^^^^^^^^^^

Kernel-layer PSyIR classes are currently used to describe existing
code in a language independent way. Consequently these nodes are more
prescriptive and are independent of a particular PSyclone
frontend. These nodes are designed to be used with PSyIR backends. One
PSy-layer class (``Loop``), can also be used as a Kernel-layer
class. Kernel-layer PSyIR nodes are: ``Loop``, ``IfBlock``,
``CodeBlock``, ``Assignment``, ``Reference``, ``Operation``,
``Literal``, ``Return`` and ``Container``. The ``Operation`` class is
further subclassed into ``UnaryOperation``, ``BinaryOperation`` and
``NaryOperation``.


Text Representation
-------------------

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
---------------

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
   .. automethod:: psyclone.psyGen.Assignment.lhs()
   .. automethod:: psyclone.psyGen.Assignment.rhs()
- ``IfBlock``:
   .. automethod:: psyclone.psyGen.IfBlock.condition()
		
   .. automethod:: psyclone.psyGen.IfBlock.if_body()

   .. automethod:: psyclone.psyGen.IfBlock.else_body()
- ``Directive``:
   .. automethod:: psyclone.psyGen.Directive.dir_body()

These are the recommended methods to navigate the tree for analysis or
operations that depend on the Node type.

Additionally, the `walk` method (available in all nodes) is able to recurse
through the tree and return objects of a given type. This is useful when the
objective is to move down the tree to a specific node or list of nodes without
information about the exact location.

.. automethod:: psyclone.psyGen.Node.walk


.. _symbol-label:

Symbols and Symbol Tables
-------------------------

Some PSyIR nodes have an associated Symbol Table
(`psyclone.psyir.symbols.SymbolTable`) which keeps a record of the
Symbols (`psyclone.psyir.symbols.Symbol`) specified and used within them.
The ``SymbolTable`` has the following interface:

.. autoclass:: psyclone.psyir.symbols.SymbolTable
    :members:

Where each element is a `Symbol` with an immutable name:

.. autoclass:: psyclone.psyir.symbols.Symbol
    :members:

There are several `Symbol` sub-classes to represent different labeled entities
in the PSyIR. At the moment the available symbols are:

- .. autoclass:: psyclone.psyir.symbols.ContainerSymbol

- .. autoclass:: psyclone.psyir.symbols.DataSymbol

Creating or adding to PSyIR
---------------------------

PSyIR nodes are connected together via parent and child methods
provided by the ``Node`` baseclass.

These nodes can be created in isolation and then connected
together. For example::

    literal = Literal("0.0")
    reference = Reference("a")
    assignment = Assignment()
    literal.parent = assignment
    reference.parent = assignment
    assignment.children = [reference, assignment]

However, as connections get more complicated, creating the correct
connections can become difficult to manage and error prone. Further,
in some cases children must be collected together within a
``Schedule`` (e.g. for ``IfBlock`` and for ``Loop``).

To simplify this complexity, each of the Kernel-layer nodes which
contain other nodes have a ``create`` method which helps construct the
PSyIR. Using this method, the above example then becomes::
  
    literal = Literal("0.0")
    reference = Reference("a")
    assignment = Assignment.create(reference, literal)

A more complete example of using this approach can be found in the
PSyclone ``examples/psyir`` directory.
