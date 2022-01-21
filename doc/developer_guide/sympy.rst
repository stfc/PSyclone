.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
.. Written by J. Henrichs, Bureau of Meteorology

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
      print("'{0}' equals '{1}'".format(writer(loop1.start_expr),
                                        writer(loop2.start_expr)))
  if not sym_maths.equal(loop1.stop_expr, loop2.stop_expr):
      print("'{0}' does not equal '{1}'".format(writer(loop1.stop_expr),
                                                writer(loop2.stop_expr)))

.. testoutput::

    '1' equals '5 + k - 4 - k'
    'k' does not equal '2 * k - k - 1'


Handling of PSyIR Structures and Arrays
---------------------------------------
SymPy has no concept of structure references or array syntax like
``a(i)%b`` in Fortran. But this case is not handled especially, the
PSyIR is converted to Fortran syntax and is provided unmodified to SymPy.
SymPy interprets the ``%`` symbol
as modulo function, so the expression above is read as ``Mod(a(i), b)``.
This interpretation achieves the expected outcome when comparing structures
and array references.
For example, ``a(i+2*j-1)%b(k-i)`` and ``a(j*2-1+i)%b(-i+k)`` will be
considered to be equal:

1. Converting the two expressions to SymPy internally results in
   ``Mod(a(i+2*j-1), b(k-i))`` and ``Mod(a(j*2-1+i, b(-i+k))``.
2. Since nothing is known about the arguments of any of the ``Mod``
   functions, SymPy will first detect that the same function is called
   in both expression, and then continue to compare the arguments of
   this function.
3. The first arguments are ``a(i+2*j-1)`` and ``a(j*2-1+i)``.
   The name ``a`` is considered an unknown function. SymPy detects
   that both expressions appear to call the same function, and it
   will therefore compare the arguments.
4. SymPy compares ``i+2*j-1`` and ``j*2-1+i`` symbolically, and
   evaluate these expressions to be identical. Therefore, the
   two expressions ``a(...)`` are identical, so the first arguments
   of the ``Mod`` function are identical.
5. Similarly, it will then continue to evaluate the second argument
   of the ``Mod`` function (``b(...)``), and evaluate them to be
   identical.
6. Since all arguments of the ``Mod`` function are identical,
   SymPy will report these two functions to be the same, which
   is the expected outcome.

Converting PSyIR to Sympy - SymPyWriter
---------------------------------------
The method ``equal`` of the SymbolicMaths class expects two PSyIR
nodes. It converts these expression first into strings before parsing
them as SymPy expressions. The conversion is done with the SymPyWriter
class. As described in the previous section, a member of a structure
in Fortran becomes a stand alone symbol or function in sympy. The SymPy
writer will rename members to better indicate that they are members:
an expression like ``a%b%c`` will be written as ``a%a_b%a_b_c``, which
SymPy then parses as ``MOD(a, MOD(a_b, a_b_c))``. This convention
makes it easier to identify what the various expressions in SymPy are.

This handling of member variables can result in name clashes. Consider
the expression ``a%b + a_b + b``. The structure access will be using
two symbols ``a`` and ``a_b`` - but now there are two different symbols
with the same name. Note that the renaming of the member from ``b`` to
``a_b`` is not the reason for this - without renaming the same clash would
happen with the symbol ``b``.

The SymPy writer uses a symbol table to make sure it creates unique symbols.
It first adds all References in the expression to the symbol table, which
guarantees that no Reference to an existing symbol is renamed. The writer
then renames all members and makes sure it uses a unique name. In the case of
``a%b + a_b + b``, it would create ``a%a_b_1 + a_b + b``, using the name
``a_b_1`` for the member to avoid the name clash with the reference
``a_b`` - so an existing Symbol Reference will not be renamed, only
members.

The SymPy writer mostly uses the Fortran writer, but implements the
following, SymPy specific features:

1. It will declare any array access as a SymPy unknown function, and any
   scalar access as a SymPy symbol. These declarations are stored in a
   dictionary, which can be queried. This dictionary is parsed into the
   SymPy writer to ensure the correct interpretation of any names found
   in the expression. Declaring arrays as functions results in the correct
   behaviour of SymPy: in case of an unknown function SymPy will compare
   all arguments, which are the array indices.
2. It renames members as described above. So a structure reference like
   ``a%b`` (in Fortran syntax) will create two SymPy symbols: ``a`` and
   ``a_b`` (or a similar name if a name clash was detected).
3. No precision or kind information is added to a constant (e.g. a Fortran
   value like ``2_4`` will be written just as ``2``).
4. The intrinsic functions ``Max``, ``Min``, ``Mod`` are returned with a
   capitalised first letter. The Fortran writer would write them
   as ``MAX`` etc., which SymPy does not recognise and would then handle
   as unknown functions.

.. autoclass:: psyclone.psyir.backend.sympy_writer.SymPyWriter
    :members:

.. note::
    The SymPyWriter class provides the static function
    ``convert_to_sympy_expressions`` which hides the complexities of the
    conversion from PSyIR expressions to SymPy expressions. It is
    strongly recommended to only use this function when this functionality
    is needed.
