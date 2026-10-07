# Inlining and Loop Tiling

PSyclone does support proper inlining, i.e. replacing a function call
with the actual source code of the called function. Inlining a function
will reduce the call overhead, and can enable additional compiler
optimisations. 

At this stage, PSyclone does not yet support inlining in the context of its
domain-specific language support (which is why we have used
module inlining previously). But when using PSyclone to transform
existing code, inlining can be used.

This example is again based on the Game of Life, as in section 3.2,
i.e. it does not use the GOcean domain.

We have used PSyclone to fuse loops previously. But in order to use
fusion in this version, we have to bring the loop next to each other,
and make sure that the loop boundaries are identical. This full
example is rather complex and it is not expected that it can
be finished in the typical time of a single hands-on session.


## Use the inlining transformation to bring all the loops together

All subroutines calls except output_field need to be inlined. Full
inlining requires that the subroutine is first 'module inlined' using
`KernelModuleInlineTrans`.
You can use the ``walk`` method to find all ``Call``s, and the name
of a routine can be accessed using ``call.routine.name``

Make sure to study the code created after inlining:

    do time = 1, time_steps, 1
      xstart = current%internal%xstart
      xstop = current%internal%xstop
      ystart = current%internal%ystart
      ystop = current%internal%ystop
      do j = ystart, ystop, 1
        do i = xstart, xstop, 1
          neighbours%data(i,j) = current%data(i - 1,j - 1) + current%data(i,j - 1) + current%data(i + 1,j - 1) + current%data(i - &
        &1,j) + current%data(i + 1,j) + current%data(i - 1,j + 1) + current%data(i,j + 1) + current%data(i + 1,j + 1)
        enddo
      enddo
      xstart_1 = current%internal%xstart
      xstop_1 = current%internal%xstop
      ystart_1 = current%internal%ystart
      ystop_1 = current%internal%ystop
      do j_1 = ystart_1, ystop_1, 1
        do i_1 = xstart_1, xstop_1, 1
          born%data(i_1,j_1) = 0.0
          if (current%data(i_1,j_1) == 0.0 .AND. neighbours%data(i_1,j_1) == 3.0) then
            born%data(i_1,j_1) = 1.0
          end if
        enddo
      enddo

Each subroutine used the same scalar variables to determine start and stop values
of the loops. Inlining will rename variables if there is a clash, so all the
assignments (even though they are identical) have been changed.

The immediate problem to do additional code optimisations is that still the
loops are not next to each other.

## Use the Move transformation to move the scalar assignments to the top

Before any loop fusion can be done, the loops must be next to each other,
which means that the scalar assignments must all be moved to the top. This
can be done with the Move transformation.

## Loop Fusion with a twist
Still, even though the loops are now next to each other, loop fusion still
does not work:

    Transformation Error: Error in LoopFuseTrans transformation. Loops do not have the same iteration space.
    do j = ystart, ystop, 1
      do i = xstart, xstop, 1
        neighbours%data(i,j) = current%data(i - 1,j - 1) + current%data(i,j - 1) + current%data(i + 1,j - 1) + current%data(i - 1,j) + current%data(i + 1,j) + current%data(i - 1,j + 1) + current%data(i,j + 1) + current%data(i + 1,j + 1)
      enddo
    enddo

    do j_1 = ystart_1, ystop_1, 1
      do i_1 = xstart_1, xstop_1, 1
        born%data(i_1,j_1) = 0.0

Reason is that the first loop uses ``ystart``, and the second loop uses ``ystart_1`` etc.
In this case, we know that ``ystart`` and ``ystart_1`` are the same, but PSyclone
will not automatically deduce this.

The easiest solution is to replace all the scalar values with the values they
are getting assigned. When moving the assignments, you can keep the variable name and
the assigned values, e.g. in a dictionary. As a reminder, an assignment node has
``lhs`` and ``rhs`` attributes.

Then loop over all References, which is the PSyIR node type of a variable access.
And if the accessed value is one of the variables assigned previously, replace
it with a ``copy()`` of the value.

After this, the loop will look like:

      do j = current%internal%ystart, current%internal%ystop, 1
        do i = current%internal%xstart, current%internal%xstop, 1
          neighbours%data(i,j) = current%data(i - 1,j - 1) + current%data(i,j - 1) + current%data(i + 1,j - 1) + current%data(i - &
    &1,j) + current%data(i + 1,j) + current%data(i - 1,j + 1) + current%data(i,j + 1) + current%data(i + 1,j + 1)
        enddo
      enddo
      do j_1 = current%internal%ystart, current%internal%ystop, 1
        do i_1 = current%internal%xstart, current%internal%xstop, 1
          born%data(i_1,j_1) = 0.0
          if (current%data(i_1,j_1) == 0.0 .AND. neighbours%data(i_1,j_1) == 3.0) then
            born%data(i_1,j_1) = 1.0
          end if
        enddo
      enddo

After this step, you can finally fuse the loop.

## Optional steps:

Apply OpenMP parallelisation, and then loop tiling. The order in the latter is important,
after loop tiling the dependency analysis of PSyclone will not be able to detect if
parallelisation is still valid due to the complex, 4-times nested loop structure.

