# PSyclone and LFRic distributed-memory tutorial: asynchronous halo exchanges #

In this example you will replace the existing synchronous halo
exchanges with asynchronous ones and then move the start of the
asynchronous halo exchanges earlier in the schedule to provide overlap
between the halo exchange communication and computation. If there is
enough computational overlap and the underlying hardware and software
supports asynchronous communication this can potentially remove, or at
least reduce, the cost of halo exchanges.

## The example ##

We will again use the same helmholtz code that we used in the previous
two sections of the tutorial. As already mentioned this is one of the
most computationally costly routines in the LFRic dynamical core.

## The asynchronous halo exchange transformation ##

PSyclone provides the `Dynamo0p3AsyncHaloExchangeTrans` halo exchange
transformation which transforms synchronous halo exchanges to
asynchronous ones.

Your task is to modify the supplied psyclone script (in the same
directory as this `README.md` file) i.e. `schedule.py` to apply the
asynchronous halo exchange transformation to each of the halo
exchanges in the schedule.

Advice is given below. With this advice you should be able to work out
how to apply this transformation, however if you are relatively new to
Python and/or get really stuck you can look in the `solutions`
directory at some example solutions and make use of those.

To remind yourself where the halo exchanges are in the PSyIR
representation, run psyclone on the code again. (As in previous
sections of the tutorial the command below and all subsequent commands
in this file assume that you are in the same directory as this
`README.md` file):

```bash
    $ psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py ../code/helmholtz_solver_alg_mod.x90
```

The schedule containing the halo exchanges is stored as a tree in the
PSyIR. The indendation in the visualisation of the PSyiR indicates the
depth of the tree. Also notice the numbers in the output. These help
you determine the position of the particular nodes in the tree.

Take a look at the provided `schedule.py` script file. The `schedule` variable
in the script is the root of the tree (which is why
`schedule.view()` provides a view of the whole schedule). You will
need to traverse this tree to get to the halo exchanges and then apply
the transformation to the appropriate nodes.

In order to use the transformation in the script we need to import it. The 
`Dynamo0p3AsyncHaloExchangeTrans` is found in `psyclone/transformations`

```python
from psyclone.transformations import Dynamo0p3AsyncHaloExchangeTrans
```

As transformations are objects we also need to create an instance of
`Dynamo0p3AsyncHaloExchangeTrans`.

```python
    async_hex_trans = Dynamo0p3AsyncHaloExchangeTrans()
```

If you would like to find out more about this transformation, it is
documented in our
[user guide](https://psyclone.readthedocs.io/en/latest/dynamo0p3.html)
(search for `Dynamo0p3AsyncHaloExchangeTrans`). Alternatively, if you
prefer, a pdf of the user guide is also available in <psyclone_home> and is
called psyclone.pdf

Traversing the PSyIR tree is discussed in the [user
guide](https://psyclone.readthedocs.io/en/latest/psyir.html) (search
for Tree Navigation) and in one of the [notebook
tutorials](../../../../notebooks/introduction.ipynb).

The simplest way to traverse the tree is to use the `walk` method,
which is accessible from all nodes in the PSyIR. The `walk` method
takes a class name as an argument (actually it can take more than one
class name but you don't need that) and will return all instances of
this class that it finds in the tree beneath the node from which it is
called in a list. If we use the walk method from the `schedule`
variable in the `schedule.py` script then all nodes will be visited.

```python
    nodes = schedule.walk(<ClassName>)
```

We only want to return HaloExchange nodes. In the LFRic API
HaloExchange nodes are instances of the `LFRicHaloExchange` class.

We therefore need to import the `LFRicHaloExchange` class which is found
in `psyclone/dynamo0p3`

```python
from psyclone.dynamo0p3 import LFRicHaloExchange
```

and use it in the walk method:

```python
    hex_nodes = schedule.walk(LFRicHaloExchange)
```

We now want to transform each of these halo exchange nodes. The
simplest way to do this is iterate over the `hex_nodes` list. Assuming
that our instance of the asynchronous halo exchange transformation is
called `async_hex_trans`:

```python
    for hex_node in hex_nodes:
        async_hex_trans.apply(hex_node)
```

Your script should look something like this:

```python
from psyclone.transformations import Dynamo0p3AsyncHaloExchangeTrans
from psyclone.dynamo0p3 import LFRicHaloExchange
def trans(psy):
    async_hex = Dynamo0p3AsyncHaloExchangeTrans()
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    for hex_node in schedule.walk(LFRicHaloExchange):
        async_hex.apply(hex_node)
    print(schedule.view())
```

You are now ready to transform the code. Run your script:

```bash
    $ psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py ../code/helmholtz_solver_alg_mod.x90
```

You will see that all halo exchanges have been converted to asynchronous halo exchanges.

```bash
InvokeSchedule[invoke='invoke_0', dm=True]
    0: Loop[type='dofs', field_space='any_space_1', it_space='dof', upper_bound='ndofs']
        Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
        Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
        Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
        Schedule[]
            0: BuiltIn setval_c(grad_p,0.0_r_def)
    1: HaloExchangeStart[field='grad_p', type='region', depth=1, check_dirty=False]
    2: HaloExchangeEnd[field='grad_p', type='region', depth=1, check_dirty=False]
    3: HaloExchangeStart[field='p', type='region', depth=1, check_dirty=True]
    4: HaloExchangeEnd[field='p', type='region', depth=1, check_dirty=True]
    ...
```

## 3: Generic script

If you have been running this example with `COMPUTE_ANNEXED_DOFS=true`
as you did in the previous example then set them to `false`. If you
are using this example with `COMPUTE_ANNEXED_DOFS=false` then set them
to true. For convenience, a copy of the configuration file is provided
in this directory with annexed dofs switched on. Remember that to
specify the config file you add `--config psyclone.cfg` to the
psyclone command.

The reason for doing this is that the number of halo exchanges and
their relative location in the PSyIR for this example changes when
`COMPUTE_ANNEXED_DOFS` is `true` or `false`.

Try running your script again. Does it still work? If not, make it
generic by following the `walk` approach suggested earlier, so that it
will work with any number of halo exchanges placed at any location.

Well done, you've (at least partially) created a generic script that
could be applied to the whole code base of any code that uses this API.

Why might this script not be completely generic? What about the case
where a file contains multiple invokes?  Would your script modify halo
exchanges in all of these invokes? If not, what would you need to do
to fix this?

The problem is that the script you are using is only applied to the first invoke:

```python
    invoke = psy.invokes.invoke_list[0]
````

We want to apply this for all invokes:

```python
    invokes = psy.invokes.invoke_list
    for invoke in invokes:
        ...
```

If you're racing ahead, you might like to change the example to
include more invokes in the algorithm layer and check that your script
works for all of them. If you do this remember to leave the code as it
was afterwards or modify a copy of the code in order to not affect
future sections of the tutorial.

The script you have created is very powerful as it can be applied to
an arbitrarily large code base (which can change over time). Imagine
having to do this by hand and having to continue to add in changes as
the code evolves.

## 4: Specific script

You've written a generic script. If you have enough time now re-write
your script so that it is only applied to a specific halo
exchange. However, keep your original generic script as you will need to
use this again in a bit (you might like to use a different script file
in order to keep both).

Applying optimisations selectively is something that might be useful
when different types of optimisations are required in different
situations.

## 5: Moving halo exchange calls

Well done, you managed to transform all or some of the halo exchanges
from synchronous to asynchronous. However, there will be no
performance improvement as there is no overlap of computation and
communication. Let's now try to make use of any potential overlap.

PSyclone provides a move transformation `MoveTrans` that allows a node
in the PSyIR tree to be moved. However, it will only allow a node to
be moved to a different point at the same depth of the tree and will
only allow the node to be moved if it does not break any data
dependence ordering constraints, otherwise it will raise a
`TransformationError` exception. For example a halo exchange is placed
before a loop that requires the halo exchange. Therefore the move
transformation will not allow the node to be moved after the loop as
that would result in incorrect code. Feel free to try this out if you
like as you work on this tutorial :-)

Your task is to extend your current script (that transforms
synchronous halo exchanges to asynchronous halo exchanges), in order
to apply the move transformation to move one of the halo exchange
start nodes to as early as possible in the schedule.

First, find the position of the halo exchange start that you want to
move. You should be able to see these positions by running your
generic asynchronous halo exchange script and viewing the PSyIR
output. For example:

```bash
InvokeSchedule[invoke='invoke_0', dm=True]
    0: Loop[type='dofs', field_space='any_space_1', it_space='dof', upper_bound='ndofs']
        Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
        Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
        Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
        Schedule[]
            0: BuiltIn setval_c(grad_p,0.0_r_def)
    1: HaloExchangeStart[field='grad_p', type='region', depth=1, check_dirty=False]
    2: HaloExchangeEnd[field='grad_p', type='region', depth=1, check_dirty=False]
    3: HaloExchangeStart[field='p', type='region', depth=1, check_dirty=True]
    4: HaloExchangeEnd[field='p', type='region', depth=1, check_dirty=True]
    ...
```

In the above example we could try to move the haloexchangestart at
position 1 so that it ends up before the loop currently at position
0. However, the move transformation will not allow this as this halo
exchange has been added there because the field `grad_p` is modified
in the first loop, so it must happen after the loop art position 0.

Therefore let's try the next haloexchangestart (at position 3 in our case).

First import the `MoveTrans` transformation:

```python
from psyclone.transformations import MoveTrans
```

Next create an instance of the transformation:

```python
    move_trans = MoveTrans()
```

Finally apply the transformation to the selected locations:

```python
    move_trans.apply(schedule[3], schedule[0])
```

The first argument of the `MoveTrans` transformation apply method is
the PSyIR node you want to move and its second argument is where you
want to place the node.

Note, clearly the above transformation needs to be applied after the
asynchronous halo exchange transformation.

Run your modified transformation script and see if you have managed to move the
halo exchange start node. If so, well done!

If you have time you might like to repeat this transformation in your
script to move more of the halo exchange starts as early as
possible in the schedule.

## Generic approach ##

Of course the approach we have taken so far is not generic so will
only work for this example code and codes with a very similar
structure.

It is obviously possible to make the script generic. Try to do this if
you have the time and expertise. If not, you might like to take a look
at the `overlap_generic.py` script in the `solutions` directory to see
one way to do this.

If you are planning to write your own generic script, the suggested
approach is to find each halo exchange start node, and for each of these
nodes try to bubble the node back in the tree by repeatedly applying
the `MoveTrans` transformation until it can't be moved any
further. As mentioned earlier, the `MoveTrans` transformation will raise a
`TransformationError` exception if it is invalid to move the node to the
selected location and this can be used as the termination condition.

## 6. Generated code

Lastly, you might like to take a quick look at the generated code to
check that it looks the way you would expect it to.

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
