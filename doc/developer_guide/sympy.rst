.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
.. Authors: J. Henrichs, Bureau of Meteorology
            N. Nobre, STFC Daresbury Lab

.. highlight:: python

.. testsetup::

    from psyclone.core import Signature, VariablesAccessInfo
    from psyclone.psyir.frontend.fortran import FortranReader

    code = '''subroutine sub()
    integer :: i, j, k, a(10, 10)
    k = i + j
    k = j + i
    do i=1, k
         a(i,j) = 3
    enddo
    do i=5+k-4-k, 2*k-k-1
         a(i,j) = 3
    enddo

    i = (j+1) * k
    end subroutine sub
    '''
    psyir = FortranReader().psyir_from_source(code)
    # Take 'i+j' and set it as LHS for the example
    lhs = psyir.children[0].children[0].rhs
    # Take 'j+i' and set it as RHS for the example
    rhs = psyir.children[0].children[1].rhs

    # Take the two loops
    loop1 = psyir.children[0].children[2]
    loop2 = psyir.children[0].children[3]

    expr = psyir.children[0].children[4].rhs

.. _sympy:

SymPy
=====
PSyclone uses the symbolic maths package "SymPy" for comparing
expressions symbolically, e.g. a comparison like ``i+j > i+j-1``
will be evaluated to be true.

The SymPy package is wrapped in the PSyclone class ``SymbolicMaths``:

.. autoclass:: psyclone.core.SymbolicMaths
    :members:

This can be used for tests of nodes in the PSyIR. For example,
the NEMO loop fuse transformation checks that the loops to
be fused have the same loop boundaries using code like this:

.. testcode::

  from psyclone.core import SymbolicMaths
  from psyclone.psyir.backend.fortran import FortranWriter

  # Assume loop1 is ``do i=1, k`` and loop2 ``do i=5+k-4-k, 2*k-k-1``.

  writer = FortranWriter()
  sym_maths = SymbolicMaths.get()
  if sym_maths.equal(loop1.start_expr, loop2.start_expr):
      print(f"'{writer(loop1.start_expr)}' equals "
            f"'{writer(loop2.start_expr)}'")
  if not sym_maths.equal(loop1.stop_expr, loop2.stop_expr):
      print(f"'{writer(loop1.stop_expr)}' does not equal "
            f"'{ writer(loop2.stop_expr)}'")

.. testoutput::

    '1' equals '5 + k - 4 - k'
    'k' does not equal '2 * k - k - 1'

SymPyWriter - Converting PSyIR to SymPy
---------------------------------------
The methods of the SymbolicMaths class expect to be passed PSyIR nodes.
They convert these expressions first into strings before parsing
them as SymPy expressions. The conversion is done with the ``SymPyWriter``
class, and it is the task of the ``SymPyWriter`` to convert the PSyIR
into a form that can be understood by SymPy. Several Fortran constructs
need to be converted in order to work with SymPy. The SymPy writer mostly
uses the Fortran writer for creating a string for the PSyIR, but implements
the following features to allow the parsing of the expressions by SymPy:

Array Accesses
~~~~~~~~~~~~~~
Any array access is declared as a SymPy unknown function and any scalar
access as a SymPy symbol. These declarations are stored in a
dictionary, which is used by the parser of SymPy to ensure the correct
interpretation of any names found in the expression. Note that while
SymPy has the concept of ``Indexed`` expressions, they do not work well
when solving equations that requires the comparison of indices (which is
frequently needed for the dependency analysis). For example,
``M[x] - M[1] == 0`` does not result in the solution ``x=1`` when ``M``
is an indexed SymPy type. Using an unknown function on the other hand
handles this as expected.

.. _array_expressions:

Array Expressions
~~~~~~~~~~~~~~~~~
Each array index will be converted into three arguments for the corresponding
unknown SymPy function that represents the array access. For example, an array
expression ``a(i:j:k)`` will become
``a(i, j, k)``, and to then maintain the same number of arguments for
each use of an array/function, ``a(i)`` will become ``a(i,i,1)``, and
``b(i,j)`` becomes ``b(i,i,1,j,j,1)`` etc. Array expressions like ``a(:)``
will be using specific names for the lower and upper bound, defaulting
to ``sympy_lower`` and ``sympy_upper``. So the previous expression
becomes ``a(sympy_lower, sympy_upper, 1)``. Note that in case of a name
clash SymPyWriter will change the names of the boundaries to be unique.

Fortran-specific Syntax
~~~~~~~~~~~~~~~~~~~~~~~
No precision or kind information is added to a constant (e.g. a Fortran
value like ``2_4`` will be written just as ``2``). The intrinsic functions
``Max``, ``Min``, ``Mod`` are capitalised to be recognised by SymPy which
is case sensitive.

User-defined Types
~~~~~~~~~~~~~~~~~~
SymPy has no concept of user-defined types like ``a(i)%b`` in Fortran.
A structure reference like this is converted to a single new symbol
(scalar) or function (if an array index is involved). The default name
will be the name of the reference and members concatenated using ``_``,
e.g. ``a%b%c`` becomes ``a_b_c``, which will be declared as a new SymPy
symbol or function (if it is an array access). The SymPy writer uses a
symbol table to make sure it creates unique symbols.
It first adds all References in the expression to the symbol table, which
guarantees that no Reference to an existing symbol is renamed. In the case of
``a%b + a_b + b``, it would create ``a_b_1 + a_b + b``, using the name
``a_b_1`` for the structure reference to avoid the name clash with the
reference ``a_b``.

Any array indices are converted into arguments of this new function. So an
expression like ``a(i)%b%c(j,k)`` becomes ``a_b_c(i,i,1,j,j,1,k,k,1)``
(see :ref:`array_expressions`). The ``SymPyWriter`` creates a custom SymPy
function, which keeps a list of which reference/member contained how many
indices. In the example this would be ``[1, 0, 2]``, indicating that the
first reference had one index, the second one none (i.e. it is not an
array access), and the last reference had two indices. This allows the
function to properly re-create the Fortran string.


Documentation for SymPyWriter Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SymPyWriter provides the following functions:

.. autoclass:: psyclone.psyir.backend.sympy_writer.SymPyWriter
    :members:
    :special-members: __new__

SymPyReader - Converting SymPy to PSyIr
---------------------------------------
The ``SymPyReader`` converts a SymPy expression back to PSyIR. Together
with the SymPyWriter it allows to take a PSyIR expression, manipulate
it with SymPy, and creating a new PSyIR that can be used.
The SymPyReader is closely connected to the SymPyWriter: as explained
in :ref:`array_expressions` the SymPyWriter creates special symbols
to indicate the lower- and upper-bound of an array expression. In order
to convert these arguments back to the corresponding Fortran representation,
the SymPyReader needs to know the actual names (which might have been
changed from their default because of a name clash with a user variable).
The SymPyReader constructor therefore takes a SymPyWriter as argument,
and it is the responsibility of the user to make sure the provided
SymPyWriter instance is indeed the one used to create the SymPy expressions
in the first place.

An example of converting an expression `expr` from PSyIR to SymPy and back:

.. testcode::

    from sympy import expand
    from psyclone.psyir.backend.fortran import FortranWriter
    from psyclone.psyir.backend.sympy_writer import SymPyWriter
    from psyclone.psyir.frontend.sympy_reader import SymPyReader

    sympy_writer = SymPyWriter()
    sympy_expr = sympy_writer(expr)

    # Use SymPy to modify the expression ...
    new_expr = expand(sympy_expr)

    # Find the required symbol table in the original PSyIR
    symbol_table = expr.scope.symbol_table

    sympy_reader = SymPyReader(sympy_writer)
    # Convert the new sympy expression to PSyIR
    new_psyir_expr = sympy_reader.psyir_from_expression(new_expr, symbol_table)

    writer = FortranWriter()
    print(f"{writer(expr)} = {writer(new_psyir_expr)}")

.. testoutput::

     (j + 1) * k = j * k + k

Documentation for SymPyReader Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SymPyReader provides the following functions:

.. autoclass:: psyclone.psyir.frontend.sympy_reader.SymPyReader
    :members:
