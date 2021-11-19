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

.. _sympy:

SymPy
=====
PSyclone uses the symbolic maths package "SymPy" for comparing
expressions symbolically, e.g. a comparison like ``i+j > i+j-1``
will be evaluated to be true.

The SymPy package is wrapped in the PSyclone class SymbolicMaths:

.. autoclass:: psyclone.core.SymbolicMaths
    :members:

This can be used for tests of nodes in the PSyIR, for example
the dependency tools use::

        sym_maths = SymbolicMaths.get()

        first_index = all_indices[0]
        for index in all_indices[1:]:
            if not sym_maths.equal(first_index, index):
                print("Not equal")

Internal Details
----------------
The method ``equal`` of the SymbolicMaths class expects two PSyIR
nodes. It converts these expression first into strings before parsing
them as SymPy expressions. The conversion is done with the SymPyWriter
class:

.. autoclass:: psyclone.psyir.backend.sympy_writer.SymPyWriter
    :members:

This class mostly uses the Fortran writer, but implements the following,
SymPy specific way of convering nodes:

1. No precision or kind information is added to a constant (e.g. a Fortran
   value like ``2_4`` will be written just as ``2``).
2. The intrinsic functions ``Max``, ``Min``, ``Mod`` are return with a
   capitalised first lettter only. The Fortran writer would write them
   as ``MAX`` etc, which SymPy does not recognise - it would handle them
   as unknown functions.

SymPy also has no concept of Fortran structure references or array syntax
like ``a(i)%b``. But this case is not handled especially, the Fortran
syntax is provided to SymPy. Sympy interprets the ``%`` symbol as modulo
function, so the expression above is read as ``Mod(a(i), b)``. Furthermore,
it will interpret ``a(i)`` as an unknown function ``a`` with the symbol
``i`` as argument. Similary,``b`` will be a symbol. This interpretation
achieves the expected outcome when comparing structures and array references.
For example, ``a(i+2*j-1)%b(k-i)`` and ``a(j*2-1+i)%b(-i+k)`` will be
considered to be equal:

1. Converting the two expressions to SymPy internally results in
   ``Mod(a(i+2*j-1), b(k-i))`` and ``Mod(a(j*2-1+i, b(-i+k))``.
2. Since nothing is known about the arguments of any of the ``Mod``
   function, SymPy will first detect that the same function is called
   in both expression, and then continue to compare the arguments of
   this function.
3. Comparing the first argument is ``a(i+2*j-1)`` and ``a(j*2-1+i)``.
   The name ``a`` is considered an unknown function. SymPy detects
   that both expressions call the same function, and it will therefore
   compare the arguments.
4. Sympy compares ``i+2*j-1`` and ``j*2-1+i`` symbolically, and 
   evaluate these expressions to be identical. Therefore the 
   two expressions ``a(...)`` are identical, so the first argument
   of the ``Mod`` function are identical.
5. Similary, it will then continue to evaluate the second argument
   of the ``Mod`` function (``b(...)``), and evaluate them to be
   identical.
6. Since all arguments of the ``Mod`` function calls are identical,
   SymPy will report these two functions to be the same, which
   is the expected outcome.

