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
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab

.. _implementation:


Implementation
==============

The approach taken is the line-by-line method, where the order of
computation is reversed and each line of the tangent linear code is
transformed into its adjoint form.

This approach is implemented in PSyclone by parsing the tangent linear
code and transforming it into the PSyIR (the PSyclone Internal
Representation). Transformations have been written that tranform the
PSyIR representation into its adjoint form. These transformations are
then applied to the PSyIR representation. Once this is complete, the PSyIR
representation is then written back out as code.


Active variables
++++++++++++++++

When creating the adjoint of a tangent linear code the active
variables must be specified. The remaining variables are inactive (or
trajectory) variables. The active variables are the ones that are
transformed and reversed, whereas the inactive (trajectory) variables
remain unchanged.

.. Note:: it should be possisble to only need to specify global
	  variables (ones with a lifetime beyond the code i.e. passed
	  in via argument, modules etc.) as local variables will
	  inherit being active or inactive based on how they are
	  used. However, this logic has not yet been implemented so at
	  the moment all variables (local and global) must be
	  specified.

Statements
++++++++++

As the line-by-line method is used then there are rules that must be
followed for the different types of statements. This section goes
through the rules for each supported statement type.

Assignments
-----------

If a tangent linear assigment statement contains no active variables
then it is left unchanged when creating the adjoint code.

If a tangent linear assignment statement contains one or more active
variables then it must be in the following form:

.. code-block:: none

    A = x*A + y*B + z*C + ...

If this is not the case the the associated PSyclone transformation
will raise an exception, which will be reported to the user as an
error when running the psyad script.

If this assignment is shown in matrix form:

.. code-block:: none

    A   x y z   A
    B = 0 1 0 * B
    C   0 0 1   C

then the adjoint of the assignment is obtained by transposing the
matrix:

.. code-block:: none

    A   x 0 0   A
    B = y 1 0 * B
    C   z 0 1   C

which gives the following adjoint assignments

.. code-block:: none

    C^ = C^ + z*A^
    B^ = B^ + y*A^
    A^ = x*A^

where the ^ indicates that the variable now represents the adjoint value.

Notice that if `x=0` in the general form then the tangent linear code
writes to `A`, rather than updating it i.e.:

.. code-block:: none

    A = 0*A + y*B + y*C + ...

which is:

.. code-block:: none

    A = y*B + z*C + ...

and its adjoint will set `A^` to zero:

.. code-block:: none

    C^ = C^ + zA^
    B^ = B^ + yA^
    A^ = 0

Lastly, notice that if `x=y=z=0` then the original tangent linear code sets `A` to zero:

.. code-block:: none

    A = 0*A + 0*B + 0*C + ...

which is:

.. code-block:: none

    A = 0

and its adjoint sets A^ to zero

.. code-block:: none

    A^ = 0

.. Note:: in all cases `A^` should be written to after it has been
          read.

Rules
*****

Rather than creating a matrix and transposing it, it can be seen that
there are some relatively simple rules that can be followed in order
to create the adjoint of a tangent linear assignment. This is how the
PSyclone `AssigmentTrans` transformation is implemented. Let's look
again at the general form of a tangent linear statement:

.. code-block:: none

    A = x*A + y*B + z*C + ...

If each of the terms on the RHS of the statement are taken in turn
(i.e. `x*A`, then `y*B`, then `x*C`, ...) there are two cases:

1) the active variable in the RHS term is different to the active
   variable on the LHS of the assigment.
2) the active variable in the RHS term is the same as the active
   variable on the LHS of the assigment.

In case 1, the adjoint is simply the active variable on the RHS being
updated by its associated expression multiplied by the left hand
active variable. For example, take the case:

.. code-block:: none

    A = ... y*B ...

the adjoint for this term is:

.. code-block:: none

    B^ = B^ + y*A^

In case 2, the adjoint is simply the active variable being multiplied
by the associated term. For the case:

.. code-block:: none

    A = x*A ...

the adjoint for this term is:

.. code-block:: none

    A^ = x*A^

If there is no term for `A` on the RHS of the assignment then the
adjoint variable `A^` must be set to zero:

.. code-block:: none

    A^ = 0

Array accesses
**************

Active variables will typically be arrays that are accessed within a
loop. These can usually be treated in the same way as scalars
illustrated above.

However, in the case of stencils, accesses to different parts of an
array in the same statement should be treated as if they were a
different variable. For example:

.. code-block:: none

    A(i) = x*A(i) + y*A(i-1)

would become:

.. code-block:: none

    A^(i-1) = A^(i-1) + y*A^(i)
    A^(i) = x*A^(i)

.. warning:: The authors are not sure that this code is actually
   correct and it needs to be checked. It might be that all iterations
   of the first adjoint assignment should be performed beore all
   iterations of the second.

The creation of assignments that write outside of `i` is not allowed
in LFRic and so appropriate transformations will need to be applied to
restructure the code.

Transformation
**************

.. autoclass:: psyclone.psyad.transformations.AssignmentTrans
      :members: apply
