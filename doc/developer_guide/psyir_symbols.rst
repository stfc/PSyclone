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

    from psyclone.psyir.symbols import Symbol, DataSymbol, RoutineSymbol, \
        ScalarType, ArrayType, REAL4_TYPE, REAL8_TYPE, INTEGER_TYPE, \
        BOOLEAN_TYPE
    from psyclone.psyir.nodes import Reference

PSyIR Types and Symbols
#######################

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
the Symbol Table provides the `find_or_create_tag` helper method that encapsulates
the described behaviour and declares symbols when needed.

.. automethod:: psyclone.psyir.symbols.SymbolTable.find_or_create_tag

By default the `get_symbol`, `new_symbol`, `add`, `lookup`,
`lookup_with_tag`, and `find_or_create_tag` methods in a symbol table will also
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

A third solution would be to have a single, non-hierarchical Symbol class
that has only a name and a symbol-type attribute. Then we could replace the
symbol_type attribute when we discover more information without modifying
the thinner Symbol class and therefore not affecting the references to it.

What is currently done is to specialise the symbol in place (so that
any references to it do not need to change). This is implemented by the
`specialise` method in the `Symbol` class. It takes a subclass of a
`Symbol` as an argument and modifies the instance so that it becomes
the subclass. For example:

.. doctest::

    >>> sym = Symbol("a")
    >>> # sym is an instance of the Symbol class
    >>> sym.specialise(RoutineSymbol)
    >>> # sym is now an instance of the RoutineSymbol class

Sometimes providing additional properties of the new sub-class is desirable,
and sometimes even mandatory (e.g. a `DataSymbol` must always have a datatype
and optionally a constant_value parameter). For this reason the specialise
method implementation provides the same interface as the constructor
of the symbol type in order to provide the same behaviour and default values
as the constructor. For instance, in the `DataSymbol` case the following
specialisations are possible:

.. doctest::

    >>> sym = Symbol("a")
    >>> # The following statement would fail because it doesn't have a datatype
    >>> # sym.specialise(DataSymbol)
    >>> # The following statement is valid and constant_value is set to None
    >>> sym.specialise(DataSymbol, datatype=INTEGER_TYPE)

    >>> sym2 = Symbol("b")
    >>> # The following statement would fail because the constant_value doesn't
    >>> # match the datatype of the symbol
    >>> # sym2.specialise(DataSymbol, datatype=INTEGER_TYPE, constant_value=3.14)
    >>> # The following statement is valid and constant_value is set to 3
    >>> sym2.specialise(DataSymbol, datatype=INTEGER_TYPE, constant_value=3)
