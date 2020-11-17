In this section of the tutorial you will see where we are up to with
parallelising code with OpenACC directives using the LFRic API.

## Early days

Support for OpenACC is still being developed for the LFRic
API. Therefore this tutorial gives a basic introduction to what can be
done at the moment. The code generated is incorrect in some
situations. Where this is the case it will be pointed out as we go
along. A more robust working version of OpenACC is presented in
one of the PSyclone tutorial sections for the NEMO API.

# Example

We will be using the same example that was used in the distributed
memory part of the tutorial.  This example is extracted from the LFRic
model and is one of the most computationally costly sections of the
LFRic dynamical core. Note, that all of the executable code has been
removed apart from the part we are interested in, the invoke call.

## OpenACC Kernels

The simplest way to parallelise code using OpenACC is to use the
Kernels directive. This specifies to the compiler that there is a
region of code that you want to be parallel and that the compiler
should try to make it run in parallel. Lets add kernels directives
around the loops in the psy-layer.

The acc_parallel.py script is already set up to do this so just run it

> psyclone -s ./acc_parallel.py ../code/helmholtz_solver_alg_mod.x90 -oalg /dev/null -opsy psy.f90

In the psy-layer PSyIR representation that is output to the terminal
you should see that ACC Kernels directives have been added around all
of the loops.

Take a look at the generated code. You should see one of the current
issues we have in PSyclone when inserting OpenACC directives around
loops. The end of the acc kernels directives should be placed
immedidately after the loop, but they are placed after any set_clean
and set_dirty calls.

You might notice that we have square brackets around the loop argument
to the apply method. The reason for this is that the kernel expects a
list of nodes, not a single node. In the future it will be changed to
accept a single node as well as a list of nodes.

## OpenACC Routine

The code as it stands will not compile with an OpenACC compiler. The
reason for this is because OpenACC requires any subroutines called
within in OpenACC parallel region (within Kernels or Parallel
directives) to contain an OpenACC Routine directive. Let's add one in.

Create an OpenACC Routine transformation, adding it after to the
existing kernels_trans = ACCKernelsTrans() transformation

    routine_trans = ACCRoutineTrans()

Add the following code after the for loop containing the
kernels_trans.apply([loop])

        for kernel in schedule.coded_kernels():
            routine_trans.apply(kernel)

Now rerun psyclone

> psyclone -s ./acc_parallel.py ../code/helmholtz_solver_alg_mod.x90 -oalg /dev/null -opsy psy.f90

The PSyIR output to the screen looks the same, but take a look in this
directory. You should see 3 new files. These files are copies of the
kernel files with the routine transformation applied to them.

You should be able to find the !acc routine directive in each of
these files. Being able to modify kernels is a relatively new feature in
PSyclone and it makes use of the ability of the PSyIR to be able to
represent kernels. This will allow more complicated kernel
optimisations in the future which will be needed if we are going to
get good performance on GPUs.

Note, that if you do not remove these kernel files then subsequent
runs will not overwrite them, new files will be created with a unique
index each time.

Assuming the kernels themselves do not call any other subroutines we
should be able to run this code in parallel on a GPU, although it is
likely to run very slowly.

## OpenACC Loop

At the moment we are relying on the compiler to determine whether
loops in the psy-layer are parallel or not. Well we know this in
PSyclone so let's use the OpenACC Loop directive to tell the compiler
which loops are safe to parallelise. Similarly the OpenMP we can't
parallelise loops if there are dependencies between
iterations. Therefore the simplest solution is to colour these
loops. Let's also colour the loops as required.

Create a colour transformation and an OpenACC loop transformation,
adding them after the existing routine_trans = ACCRoutineTrans()
transformation

    ctrans = Dynamo0p3ColourTrans()
    loop_trans = ACCLoopTrans()

Add in the colouring script (the same as we added in the OpenMP section of the tutorial)

        for loop in schedule.loops():
            if loop.field_space.orig_name \
               not in FunctionSpace.VALID_DISCONTINUOUS_NAMES \
               and loop.iteration_space == "cell_column":
                ctrans.apply(loop)

This should appear immediately after the schedule = invoke.schedule line.

Change the kernels_trans loop from

        for loop in schedule.loops():
            kernels_trans.apply([loop])

to

        for loop in schedule.loops():
            if loop.loop_type != "colours":
                kernels_trans.apply([loop])
                loop_trans.apply(loop)

In case you're having problems, this is what your script should now look like:

    kernels_trans = ACCKernelsTrans()
    routine_trans = ACCRoutineTrans()
    ctrans = Dynamo0p3ColourTrans()
    loop_trans = ACCLoopTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        schedule = invoke.schedule

        for loop in schedule.loops():
            if loop.field_space.orig_name \
               not in FunctionSpace.VALID_DISCONTINUOUS_NAMES \
               and loop.iteration_space == "cell_column":
                ctrans.apply(loop)

        for loop in schedule.loops():
            if loop.loop_type != "colours":
                kernels_trans.apply([loop])
                loop_trans.apply(loop)

        for kernel in schedule.coded_kernels():
            routine_trans.apply(kernel)

    schedule.view()
    
    return psy

Rerun psyclone

> psyclone -s ./acc_parallel.py ../code/helmholtz_solver_alg_mod.x90 -oalg /dev/null -opsy psy.f90

Take a look at the generated psy-layer code and its PSyIR
representation. You should see that the loop directives include the
independent clause by default. The clauses can be changed but
independent is what we want. Now the compiler is being told explicitly
which loops are parallel.

## Data movement

In theory, data could be copied to and from the cpu and gpu every time
we enter and exit a kernels (or parallel) region. However, some
compilers support what is called managed memory. This supports keeping
data on the required device between OpenACC kernels (or parallel)
regions automatically.

If we are not able to rely on using managed memory then we need
another way to keep data on the device between these regions. The data
directive is one way to do this. However, in PSyclone we deal with
invoke calls one at a time and as a result are not aware of the full
structure of a code. It is therefore not possible to automatically add
data regions at the appropriate location(s).

An alternative is to use Enter Data and Exit Data directives. These
directives are designed to work with less structured data, such as
objects, but Enter Data can be used to move data to the GPU if it is
not already there. Therefore we can specify all the required GPU data
in an invoke within an Enter Data directive and it will only copy
accross data that is not already on the GPU.

Let's add one of these directives. As it is required for the whole
invoke we provide it with the (top level) schedule node.

Create the transformation by adding the following line of code after
the other lines that create transformations:

    enter_trans = ACCEnterDataTrans()

Next add the following line

        enter_trans.apply(schedule)

after the loop that modifies kernels, i.e. at the end of the loop over
invokes.

Take a look at the generated code.

> my_fav_editor psy.f90

You should see an enter data directive with a large number of copyin
variables. As there are structures, it tries to add both the name of
the structure and the contents of the structure, which is needed by
OpenACC. Notice the error in list ... we accidentally include the
constant value 0.0_r_def. Ignoring any bugs! it is much simpler, less
error prone and more maintainable to have the computer generate such a
list rather than the HPC expert.

## More efficient code

Whilst the code that has been generated should run on a GPU (barring
any of the identified errors), it will not be performant. GPU experts
have manually optimised an extracted LFRic kernel and have shown that
in order to get good performance the kernels need to be modified so
that the k-loop (loop over levels) can be parallelised with an OpenACC
vector directive and the kernels need to be inlined into the
psy-layer. It might also be beneficial to use locks rather than
colouring the loops. Work on supporting these optimisations in
PSyclone is ongoing.

## Finished

Well done, you have finished the LFRic OpenACC part of the tutorial.