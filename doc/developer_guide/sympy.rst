.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2021, Science and Technology Facilities Council.
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


Internal Details
----------------
The method ``equal`` of the SymbolicMaths class expects two PSyIR
nodes. It converts these expression first into strings before parsing
them as SymPy expressions. The conversion is done with the SymPyWriter
class:

.. autoclass:: psyclone.psyir.backend.sympy_writer.SymPyWriter
    :members:

This class mostly uses the Fortran writer, but implements the following,
SymPy specific way of converting nodes:

1. No precision or kind information is added to a constant (e.g. a Fortran
   value like ``2_4`` will be written just as ``2``).
2. The intrinsic functions ``Max``, ``Min``, ``Mod`` are returned with a
   capitalised first letter only. The Fortran writer would write them
   as ``MAX`` etc., which SymPy does not recognise - it would handle them
   as unknown functions.

SymPy also has no concept of Fortran structure references or array syntax
like ``a(i)%b``. But this case is not handled especially, the Fortran
syntax is provided unmodified to SymPy. SymPy interprets the ``%`` symbol
as modulo function, so the expression above is read as ``Mod(a(i), b)``.
Furthermore, it will interpret ``a(i)`` as an unknown function ``a`` with the
symbol ``i`` as argument. Similarly,``b`` will be a symbol. This interpretation
achieves the expected outcome when comparing structures and array references.
For example, ``a(i+2*j-1)%b(k-i)`` and ``a(j*2-1+i)%b(-i+k)`` will be
considered to be equal:

1. Converting the two expressions to SymPy internally results in
   ``Mod(a(i+2*j-1), b(k-i))`` and ``Mod(a(j*2-1+i, b(-i+k))``.
2. Since nothing is known about the arguments of any of the ``Mod``
   function, SymPy will first detect that the same function is called
   in both expression, and then continue to compare the arguments of
   this function.
3. The first arguments are ``a(i+2*j-1)`` and ``a(j*2-1+i)``.
   The name ``a`` is considered an unknown function (since it's not
   a SymPy vector). SymPy detects that both expressions appear to call
   the same function, and it will therefore compare the arguments.
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

All Fortran names will be declared as SymPy symbols. Because
of the handling of structures, all members of a structure will be
declared individually, e.g. ``a%b`` will declare two SymPy symbols ``a``
and ``b``.
