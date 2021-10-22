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
computation is reversed and each line of the tangent-linear code is
transformed into its adjoint form.

This approach is implemented in PSyclone by parsing the tangent-linear
code and transforming it into the PSyIR (the PSyclone Internal
Representation). A PSyIR visitor has been written that visits each
node in the PSyIR tree and transforms each node into its adjoint form. Once
this is complete, the PSyIR representation is then written back out as
code.


Active variables
++++++++++++++++

When creating the adjoint of a tangent-linear code the active
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

Assignment
----------

If a tangent-linear assigment statement contains no active variables
then it is left unchanged when creating the adjoint code.

If a tangent-linear assignment statement contains one or more active
variables then it must be in the following general form:

.. math::

   A = xA + \sum_{i=0}^{N-1} y_i B_i

where :math:`A` and :math:`B_i` are active variables, :math:`x` and
:math:`y_i` are expressions that do not contain any active variables
and there is no limit on the size of :math:`N`.

If this is not the case the associated PSyclone transformation will
raise an exception, which will be reported to the user as an error
when running the psyad script.

For illustration, consider the case where there are 3 active variables
(equivalent to :math:`N=2`). We can then write this case in the
following form:

.. math::

    A = xA + yB + zC

where :math:`A`, :math:`B` and :math:`C` are active variables and
:math:`x`, :math:`y` and :math:`z` are expressions that do not contain
active variables.

If the above example is shown in matrix form, we have:

.. math::

    \begin{bmatrix} A \\ B \\ C \end{bmatrix} = \begin{bmatrix} x & y & z \\ 0 & 1 & 0 \\ 0 & 0 & 1 \end{bmatrix} * \begin{bmatrix} A \\ B \\ C \end{bmatrix}

The adjoint of the assignment is obtained by transposing the matrix:

.. math::

    \begin{bmatrix} \hat{A} \\ \hat{B} \\ \hat{C} \end{bmatrix} = \begin{bmatrix} x & 0 & 0 \\ y & 1 & 0 \\ z & 0 & 1 \end{bmatrix} * \begin{bmatrix} \hat{A} \\ \hat{B} \\ \hat{C} \end{bmatrix}

where :math:`\hat{A}` denotes the adjoint of the original active variable :math:`A`. This gives the
following adjoint assignments:

.. math::

    \hat{C} = \hat{C} + z\hat{A} \\
    \hat{B} = \hat{B} + y\hat{A} \\
    \hat{A} = x\hat{A}

Notice that if the expression :math:`x` is :math:`0` then the
tangent-linear code writes to :math:`\hat{A}`, rather than updating it
i.e.:

.. math::

    A = 0.A + yB + yC

which is:

.. math::

    A = yB + zC

and its adjoint will set :math:`\hat{A}` to zero:

.. math::

    \hat{C} = \hat{C} + z\hat{A} \\
    \hat{B} = \hat{B} + y\hat{A} \\
    \hat{A} = 0

Finally, notice that if :math:`x=y=z=0` then the original
tangent-linear code sets :math:`A` to zero:

.. math::

    A = 0.A + 0.B + 0.C

which is:

.. math::

    A = 0

and its adjoint sets :math:`\hat{A}` to zero

.. math::

    \hat{A} = 0

.. note:: in all cases :math:`\hat{A}` should be written to after it has been
          read.

Rules
*****

Rather than creating a matrix and transposing it, it can be seen that
there are some relatively simple rules that can be followed in order
to create the adjoint of a tangent-linear assignment. This is how the
PSyAD `AssigmentTrans` transformation is implemented. Let's look
again at the previous example tangent-linear statement:

.. math::

    A = xA + yB + zC

If each of the terms on the right-hand-side (RHS) of the statement are taken in turn
(i.e. :math:`xA`, then :math:`yB`, then :math:`xC`) there are two cases to consider:

1) the active variable in the RHS term is different to the active
   variable on the left-hand-side (LHS) of the assigment.
2) the active variable in the RHS term is the same as the active
   variable on the LHS of the assigment.

In case 1, the adjoint is simply the active variable on the RHS being
updated with the product of its multiplier in the tangent-linear
expression with the left-hand active variable. For example, take the
case:

.. math::

    A = ... yB ...

the adjoint for this term is:

.. math::

    \hat{B} = \hat{B} + y\hat{A}

In case 2, the adjoint is simply the active variable being multiplied
by the associated term. For the case:

.. math::

    A = xA ...

the adjoint for this term is:

.. math::

    \hat{A} = x\hat{A}

If there is no term for :math:`A` on the RHS of the assignment then the
adjoint variable :math:`\hat{A}` must be set to zero:

.. math::

    \hat{A} = 0

Array accesses
**************

Active variables will typically be arrays that are accessed within a
loop. These can usually be treated in the same way as the scalars
illustrated above.

However, in the case of stencils, accesses to different parts of an
array in the same statement should be treated as if they were a
different variable. For example:

.. math::

    A(i) = xA(i) + yA(i-1)

would become:

.. math::

    \hat{A}(i-1) = \hat{A}(i-1) + y\hat{A}(i) \\
    \hat{A}(i) = x\hat{A}(i)

.. warning:: The authors are not sure that this code is actually
   correct and it needs to be checked. It might be that all iterations
   of the first adjoint assignment should be performed before all
   iterations of the second (i.e. in separate loops).

In LFRic, a kernel is forbidden from writing to data outside the
current column (e.g. to element :math:`i-1`) and therefore appropriate
transformations will need to be applied to restructure the code.

Limitations
***********

If an active variable is part of the denominator in a division then
the transformation will always raise an exception stating that this
assignment is in an invalid tangent-linear form. For example
:math:`A=x/B` where :math:`A` and :math:`B` are active
variables. However, if the active variable is within an even number of
divides then it is is, in fact, valid and should not result in an
exception. For example :math:`A=x(/y/B)` is equivalent to
:math:`A=(x/y)B`. Issue #1348 captures this current limitation.


Transformation
**************

.. autoclass:: psyclone.psyad.transformations.AssignmentTrans
      :members: apply

Sequence of Statements (PSyIR Schedule)
---------------------------------------

The PSyIR captures a sequence of statements as children of a
'Schedule' node. In PSyclone a sequence of statements in a tangent
linear code are transformed to to their adjoint form by implementing
the following rules:

1) Each statement is examined to see whether it contains any active
variables. A statement that contains one or more active variables is
classed as an ``active statement`` and a statement that does not
contain any active variables is classed as an ``inactive statement``.

2) Any inactive statements are left unchanged and immediately output
as PSyIR in the same order as they were found in the tangent linear
code. Therefore the resulting sequence of statements in the adjoint
code will contains all inactive statements before all active
statements.

3) The order of any active tangent-linear statements are then reversed
and the rules associated with each statement type are applied
individually to each statement and the resultant PSyIR returned.

.. note:: At the moment the only statements supported within a
          sequence of statements are assignments. If other types of
          statement are found then an exception will be raised.

.. warning:: The above rules are invalid if an inactive variable is
             modified and that inactive variable is read both before
             and after it is modified from within active
             statements. This case is not checked in this version, see
             issue #1458.
