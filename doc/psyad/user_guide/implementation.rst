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
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab

.. _implementation:


Implementation
==============

The approach taken to constructing the adjoint is the line-by-line
method, where the order of computation is reversed and each line of
the tangent-linear (TL) code is transformed into its adjoint form.

This approach is implemented in PSyclone by parsing the tangent-linear
code and transforming it into the PSyIR (the PSyclone Internal
Representation). A PSyIR visitor has been written that visits each
node in the PSyIR tree and transforms each node into its adjoint form. Once
this is complete, the PSyIR representation is then written back out as
code.

If the supplied tangent-linear code contains active variables that are
passed by argument then the intent of those arguments may change when
translating to their adjoint form. The new intents are determined as a
final step in the visitor for a PSyIR :ref:`Schedule <psyir_schedule>`
node: dependence analysis is used to identify the way in which each
argument is being accessed in the adjoint code and the ``intent`` is
updated appropriately.


Active Variables
++++++++++++++++

When creating the adjoint of a tangent-linear code the active
variables must be specified. The remaining variables are passive (or
trajectory) variables. The active variables are the ones that are
transformed and reversed, whereas the passive (trajectory) variables
remain unchanged.

.. Note:: it should be possible to only need to specify global
	  variables (ones with a lifetime beyond the code i.e. passed
	  in via argument, modules etc.) as local variables will
	  inherit being active or passive based on how they are
	  used. However, this logic has not yet been implemented so at
	  the moment all variables (local and global) must be
	  specified.

Statements
++++++++++

As the line-by-line method is used then there are rules that must be
followed for the different types of statements. This section goes
through the rules for each supported statement type.

.. _sec_assignment:

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

Array Accesses
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

When zero-ing active variables (see step 1 in the
:ref:`psyir_schedule` section) only variables that are scalars or
arrays and are of type REAL or INTEGER are currently supported. Issue
#1627 captures this limitation.

Transformation
**************

.. autoclass:: psyclone.psyad.transformations.AssignmentTrans
      :members: apply

.. _psyir_schedule:

Sequence of Statements (PSyIR Schedule)
---------------------------------------

The PSyIR captures a sequence of statements as children of a
'Schedule' node. In PSyclone a sequence of statements in a tangent
linear code are transformed to to their adjoint form by implementing
the following rules:

1) If there are any active variables that are local to the Schedule in
the tangent linear code then they may need to be zero'ed in the
adjoint form. The current implementation does not try to determine
which local active variables need to be zero'ed and instead zero's all
of them. This approach is always safe but may zero some variables when
it is not required. The current implementation sets arrays to zero, it
does not use array notation or loops.

2) Each statement is examined to see whether it contains any active
variables. A statement that contains one or more active variables is
classed as an ``active statement`` and a statement that does not
contain any active variables is classed as a ``passive statement``.

3) Any passive statements are left unchanged and immediately output
as PSyIR in the same order as they were found in the tangent linear
code. Therefore the resulting sequence of statements in the adjoint
code will contains all passive statements before all active
statements.

4) The order of any active tangent-linear statements are then reversed
and the rules associated with each statement type are applied
individually to each statement and the resultant PSyIR returned.

.. note:: At the moment the only statements supported within a
          sequence of statements are assignments and loops. If other
          types of statement are found then an exception will be
          raised.

.. warning:: The above rules are invalid if a passive variable is
             modified and that passive variable is read both before
             and after it is modified from within active statements or
             loops. This case is not checked in this version, see
             issue #1458.

Loop
----

The loop variable and any variables used in the loop bounds should be
passive. If they are not then an exception will be raised.

If all of the variables used within the body of the loop are passive
then this loop statement and its contents is considered to be passive and
treated in the same way as any other passive node, i.e. left unchanged
when creating the adjoint code.

If one or more of the variables used within the body of the loop are
active then the loop statement is considered to be active. In this case:

1) the order of the loop is reversed. This can be naively implemented
   by swapping the loop's upper and lower bounds and multiplying the
   loop step by :math:`-1`. However, if we consider the following
   loop: :math:`start=1`, :math:`stop=4`, :math:`step=2`, the
   iteration values would be :math:`1` and :math:`3`. If this loop is
   reversed in the naive way then we get the following loop:
   :math:`start=4`, :math:`stop=1`, :math:`step=-2`, which gives the
   iteration values :math:`4` and :math:`2`. Therefore it can be seen
   that the naive approach does not work correctly in this case. What
   is required is an offset correction which can be computed as:
   :math:`(stop-start) \mod(step)`. The adjoint loop start then
   becomes :math:`stop - ((stop-start) \mod(step))`. With this change
   the iteration values in the above example are :math:`3` and
   :math:`1` as expected.

2) the body of the loop comprises a schedule which will contain a
   sequence of statements. The rules associated with the schedule are
   followed and the loop body translated accordingly (see the
   following section).

.. note:: if PSyclone is able to determine that the loop step is
          :math:`1` or :math:`-1` then there is no need to compute an
          offset, as the offset will always be :math:`0`. As a step of
          :math:`1` (or :math:`-1`) is a common case PSyclone will
          therefore avoid generating any loop-bound offset code in
          this case.

If Statement
------------

When an if statement is found in the tangent-linear code, such as the
following Fortran snippet:

.. code-block:: fortran
   
   if (condition) then
     ! content1
   else
     ! content2
   end if

1) the logical structure of the if is left unchanged, i.e. the
   :code:`if (condition) then`, optional :code:`else` and :code:`end
   if`.

2) the sequence of statements within :code:`! content1` in the above
   example, are processed as described in the Section
   :ref:`psyir_schedule`.

3) the sequence of statements within :code:`! content2` in the above
   example, (if it exists, as the else part of an if is optional) are
   processed as described in the Section :ref:`psyir_schedule`.

The :code:`condition` of the :code:`if` should only contain passive
variables for this to be a valid tangent-linear code and PSyAD will
raise an exception if this is not the case.

.. _pre-processing:
  
Pre-processing
++++++++++++++

PSyAD implements an internal pre-processing phase where code
containing unsupported code structures or constructs is transformed
into code that can be processed. These structures/constructs are
detailed below.

Array Notation
--------------

Array notation in tangent-linear codes is translated into equivalent
loops in the pre-processing phase before the tangent-linear code is
transformed into its adjoint. This is performed as the rules that are
applied to transform a tangent-linear code into its adjoint are not
always correct when array notation is used. Only array notation that
contains active variables is translated into equivalent loops.

Intrinsics
----------

If an intrinsic function, such as ``matmul`` or ``transpose``, is
found in a tangent-linear code and it contains active variables then
it must be transformed such that it is replaced by equivalent Fortran
code. This is performed in the pre-processing phase.

If an unsupported intrinsic function is found then PSyAD will raise an
exception.

The only supported intrinsics at this time are ``dot_product`` and
``matmul``.

If a ``dot_product`` or ``matmul`` intrinsic is found in the
tangent-linear code it is first transformed into equivalent inline
code before the code is transformed to its adjoint form. The PSyIR
``DotProduct2CodeTrans`` or ``Matmul2CodeTrans`` transformations are
used to perform these manipulations. See the
:ref:`user_guide:available_trans` section of the user guide for more
information on these transformations.

.. note:: At the moment all ``dot_product`` and ``matmul`` instrinsics
	  are transformed irrespective of whether their arguments and
	  return values are (or contain) active variables or not.

.. note:: Note, the transformed tangent-linear code can contain new
          variables, some of which might be active. Any such active
          variables will need to be specified as active on the PSyAD
          command-line using the ``-a`` flag even though they do not
          (yet) exist in the tangent linear code. Eventually such
          variables will be detected automatically by PSyAD, see issue
          #1595.

Associativity
-------------

As described in the :ref:`sec_assignment` section, PSyAD expects
tangent-linear code to be written as a sum of products of inactive and
active variables. Therefore if code such as :math:`a(b+c)` is found
(where :math:`b` and :math:`c` are active) then it must be transformed
into a recognised form. This is achieved by expanding all such
expressions as part of the pre-processing phase. In this example, the
resulting code is :math:`a*b + a*c` which PSyAD can then take the
adjoint of.

Test Harness
++++++++++++

In addition to generating the adjoint of a tangent-linear kernel, PSyAD
is also able to :ref:`generate <test_harness_gen>` a test harness for
that kernel that verifies that the generated adjoint is mathematically
correct.

This test harness code must perform the following steps:

1) Initialise all of the kernel arguments and keep copies of them;
2) Call the tangent-linear kernel;
3) Compute the inner product of the results of the kernel;
4) Call the adjoint of the TL kernel, passing in the outputs of the TL
   kernel call;
5) Compute the inner product of the results of the adjoint kernel with
   the original inputs to the TL kernel;
6) Compare the two inner products for equality, allowing for machine
   precision.

Steps 1, 3, 5 and 6 are described in more detail below.

Initialisation
--------------

All arguments to the TL kernel are initialised with pseudo-random numbers
in the interval :math:`[0.0,1.0]` using the Fortran `random_number` intrinsic
function.

.. note:: this initialisation will not be correct when a kernel contains
	  indirection and is passed a mapping array. In such cases the mapping
	  array will need initialising with meaningful values. This is the
	  subject of Issue #1496.

Inner Products
--------------

The precision of the variables used to accumulate the inner-product values
is set to match that of the active variables in the supplied TL kernel.
(An exception is raised if active variables of different precision
are found.)

For simplicity, when computing the inner product in steps 3) and 5),
both active and passive kernel arguments are included (since the
latter will remain constant for both the TL and adjoint kernel calls
they can be included in the inner-product compuation without affecting the
correctness test). It is likely that this will require refinement in future,
e.g. for kernels that have non-numeric arguments.

Comparing the Inner Products
----------------------------

Performing the comparison of the two inner products while allowing for
machine precision is implemented as follows:

1) Find the smallest possible difference that can be represented by
   calling the Fortran `spacing` intrinsic on the largest absolute value of
   of the two inner products;

2) Compute the *relative* difference between the two values by dividing
   their absolute difference by this spacing;

3) If this relative difference is less than the overall test tolerance
   then the test has passed.

By using the largest of the two inner product results in step 1), the
resulting spacing value is guaranteed to be appropriate in the case where
there is an error and one of the inner products is zero or less than
`tiny(1.0)`.

By default, the overall test tolerance is set to `1500.0`. This is
currently set as a constant in the `psyclone.psyad.tl2ad` module but
will eventually be exposed as a configuration option (this is the
subject of issue #1346).  This value is the one arrived at over time
by the Met Office in the current adjoint-testing code. In that code,
the vector of variables can be of order 200M in length (since it
involves values at all points of the 3D mesh) and therefore there is
plenty of scope for numerical errors to accumulate. Whether this value
is appropriate for LFRic kernels is yet to be determined.
