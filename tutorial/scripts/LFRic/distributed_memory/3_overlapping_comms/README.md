In this example you will replace the existing synchronous halo
exchanges with asynchronous ones and then move the start of the
asynchronous halo exchanges earlier in the schedule to provide overlap
between the halo exchange communication and computation. If there is
enough computational overlap and the underlying hardware and software
supports asynchronous communication this can potentially remove the
cost of halo exchanges.

## 1: The example

We will again use the same helmholtz code that we used in the previous
two examples. As already mentioned this is one of the most computationally
costly routines in the LFRic dynamical core.

## 2: The asynchronous halo exchange transformation

PSyclone provides a halo exchange transformation that transforms
synchronous halo exchanges to asynchronous ones.

Your task is to modify the supplied psyclone script i.e. schedule.py
(or create a separate script) to apply this transformation to the
example code.

Some hints are given below. You should be able to work out how to
apply this transformation, however if you are relatively new to Python
and/or get really stuck you can look in the pre_cooked directory at
some example solutions and make use of those.

To remind yourself where the halo exchanges are in the PSyIR
representation, run psyclone on the code again

> psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py ../code/helmholtz_solver_alg_mod.x90

The schedule containing the halo exchanges is stored as a tree in the
PSyIR. The indendation in the visualisation of the PSyiR indicates the
depth of the tree. Also notice the numbers in the output. These help
you determine the position of the particular nodes in the tree.

Take a look at the provided schedule.py file. The "schedule" variable
in the schedule.py script is the root of the tree (which is why
schedule.view() prints out a view of the whole schedule). You will
need to traverse this tree to get to the halo exchanges and then apply
the transformation to the appropriate nodes.

Traversing the PSyIR tree is discussed in the user guide
https://psyclone.readthedocs.io/en/latest/psyir.html (search for Tree
Navigation.

You will need to determine HaloExchange nodes from other types of
nodes. A HaloExchange node is a class which can be imported like this:

from psyclone.dynamo0p3 import DynHaloExchange

The transformation is also documented in our user guide
https://psyclone.readthedocs.io/en/latest/dynamo0p3.html (search for
Dynamo0p3AsyncHaloExchangeTrans).

If you prefer, a pdf of the user guide is also available in
<psyclone_home> and is called psyclone.pdf

Have fun.

## 3: Generic script

Switch on annexed dofs as you did in the previous example (or off you
you are already using them). For convenience, a copy of the
configuration file is provided in this directory with annexed dofs
switched on.

Does your script still work? If not, make it generic, so that it will
work with any number of halo exchanges placed at any location.

Well done, you've (at least partially) created a generic script that
could be applied to the whole code base of any code that uses this API.

Why might this script not be completely generic? What about the case
where a file contains multiple invokes?  Would your script modify halo
exchanges in all of these invokes? If not, what would you need to do
to fix this? If you're racing ahead you might like to change the
example to include more invokes in the algorithm layer and check that
your script works for all of them.

The script you have created is very powerful as it can be applied to
an arbitrarily large code base (which can change over time). Imagine
having to do this by hand and having to continue to add in changes as
the code evolves.

## 4: Specific script

You've written a generic script. If you have enough time now re-write
your example so that it is only applied to a specific halo
exchange. However, keep your original general code as you will need to
use this again in a bit (you might like to use a different script in
order to keep both).

Applying optimisations selectively is something that might be useful
when different types of optimisations are required in different
situations.

## 5: Moving halo exchange calls

Well done, you managed to transform all or some of the halo exchanges
between synchronous and asynchronous. However, there will be no
performance improvement as there is no overlap of computation and
communication. Let's now try to make use of any potential overlap.

PSyclone provides a move transformation that allows a node in the
PSyIR tree to be moved. However, it will only allow a node to be moved
to a different point at the same depth of the tree and will only allow
the node to be moved if it does not break any data dependence ordering
constraints. For example a halo exchange is placed before a loop that
requires the halo exchange. Therefore the move transformation will not
allow the node to be moved after the loop as that would result in
incorrect code. Feel free to try this out if you like as you work on
this tutorial :-)

Your task is to extend your current script that transforms synchronous
halo exchanges to asynchronous halo exchanges (or create a separate
script) in order to apply the move transformation to move any halo
exchange start nodes as early as possible in the schedule.

Some pointers are given below. You will hopefully be able to work out
how to apply this transformation, however if you are relatively new to
Python and/or get really stuck, or run out of time you can look in the
pre_cooked directory at some example solutions.

You will need to determine HaloExchange nodes from other types of
nodes. A HaloExchange node is a class which can be imported like this:

from psyclone.dynamo0p3 import DynHaloExchangeStart

The transformation is also documented in our user guide
https://psyclone.readthedocs.io/en/latest/transformations.html (search for
MoveTrans).

If you prefer, a pdf of the user guide is also available in
<psyclone_home> and is called psyclone.pdf

Have even more fun.

## 6. Generated code

Lastly, you might like to take a quick look at the generated code.

## Key points

* It is possible to use PSyclone transformations applied in a PSyclone
  script to transform synchronous halo exchanges into asynchronous
  halo exchanges and subsequently overlap communication and
  computation.

* The science code (algorithm and kernel code) does not change,
  therefore science developers do not need to be concerned with
  parallelism issues.

* The resultant code is guaranteed to be correct (if the rules being
  followed are correct!). This helps the HPC expert.

## Congratulations

You have finished this section of the tutorial.
