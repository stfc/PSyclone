# Creating Fortran Code with PSyclone

This hands-on session uses the stand-alone game of life again.
It shows how to modify existing Fortran code by adding newly
created code.

## Overview

The task here is to create an alternative version of a loop. When
optimising code, it can be useful to have two different versions of
the same loop depending on loop length. For example, if the loop
length it long enough, vectorisation might give a speed benefit,
while if the loop is too short, vectorisation will have too much
start-up overhead, causing a slow-down. Or only if the loop length
is large enough it is worth to parallelise a loop.

Of course, two versions of the could be implemented in the original source
code, but that has the well-known disadvantages of having to maintain
two mostly identical source codes (e.g. a science change to the code
would need to be done in two versions of the loop). With PSyclone,
this code manipulation can be done later, meaning the science developer
only needs to maintain a single copy of the source code.

There are three steps that need to be done: first of all, create a
condition that will be used to distinguish which version of the loop
to use (in this case based on the number of loop iterations). Then create
an if-expression with one loop in the if-body (and no else-body yet).
Then add a duplicate version of the loop as else-body. Finally, apply
additional optimisations (here OpenMP parallelisation) to one of the
two loops.

## Details

Start with using the ``add_if.py`` template script. We modify the
``combine_mod.f90`` file (though the same code can be applied to any
other subroutine here). It already has code to select the right
routine (while atm there is only one routine in the file, explicitly
finding the right routine means that this transformation can even
be applied if e.g. due to module inlining additional subroutines
might have been added previously).

There are two ways of creating the if condition:

1. Create a string with the Fortran expression that you need, and
   parse this code to create PSyIR.

   You need to use the Fortran writer to convert the start- and stop-
   expressions of the loop (since they might be complex Fortran
   expressions, and not just a variable name). Then create a
   string with ``start_expression - stop_expression >= 99``,
   and use the ``FortranReader``'s ``psyir_from_expression``
   function to convert this string to PSyIR. You need to provide
   the symbol-table of the routine you are working on (so that all
   symbols match up appropriately).

2. Create the PSyIR using the various ``create`` functions of
   the PSyIR nodes.

   The tree view of the expression ``ystop-ustart >= 99`` in PSyIR is:

       BinaryOperation[operator:'GE']
        BinaryOperation[operator:'SUB']
            Reference[name:'ystop']
            Reference[name:'ystart']
        Literal[value:'99', Scalar<INTEGER, UNDEFINED>]

    Start assembling the nodes bottom up (i.e. first the ``SUB``
    binary operation, then the ``GE`` one). As opposed to the previous
    approach, you don't need to worry about converting the start-
    and stop-expressions, you just use a copy of the tree.

The ``add_if.py`` template supports both ways, and queries the
environment variable ``PARSE_STRING`` to decide which one to use. Just
set ``PARSE_STRING`` to a non-empty string to use string-parsing,
otherwise (also as default) it will use the PSyIR subtree creation method.

Next, create the ``if``-statement (this time only using the PSyIR
tree assembly, which feels more natural and faster than creating the full
if-statement with its two versions of the loop as string and parsing it).
We start by create the ``if``-statement with a copy of the loop (you cannot
attach the original loop node immediately to the ``if``-statement, since the
original node is still attached to the ``routine`` PSyIR.
And if you detached it first, you would then need to ensure that the if
statement is inserted at the right location). Then we replace the original
loop with the newly created ``if``-statement. This achieves two goals with
one command: we make sure that the ``if``-statement is inserted at the
correct location (since it replaces the original loop), and at the same
time it detaches the original loop node. Now this detached loop node
can be added to the ``if``-statement, where it becomes the else body.

Finally, apply OpenMP parallelisation to the version of the loop
with at least 100 loop iterations.

The code created in the end will be:

    if (ystop - ystart >= 99) then
      !$omp parallel do default(shared) private(i,j) schedule(auto)
      do j = ystart, ystop, 1
        do i = xstart, xstop, 1
          current(i,j) = current(i,j) - die(i,j) + born(i,j)
        enddo
      enddo
      !$omp end parallel do
    else
      do j = ystart, ystop, 1
        do i = xstart, xstop, 1
          current(i,j) = current(i,j) - die(i,j) + born(i,j)
        enddo
      enddo
    end if

If the implementation of the loop is changed (i.e. like a science update),
running the same script will still create the two versions of the loop without
any change to the script (or the requirement to duplicate the updated code
in two versions of the loop).
