# PSyclone for existing code - Tutorial 1 #

Welcome to the first part of the tutorial on using PSyclone with existing
Fortran code via its 'nemo' API. For this tutorial we will be
using a standalone, single-source-file mini-app (tra_adv.F90) based on
a tracer-advection routine that has been extracted from the full source of
the NEMO ocean model (www.nemo-ocean.eu). The original version of this
mini-app was kindly provided by Silvia Mocavero of CMCC.

## Prerequisites ##

The requirements for this section are as described in the practicals
[README.md](../../README.md#Requirements).

Check that PSyclone is installed and configured correctly by doing
(assuming that your current working directory is the one containing
this file):

```bash
    $ cd ../../../../examples/nemo/eg1
    $ make transform
    $ cd -
```

If everything is working correctly then the `make transform` command
should result in Fortran code being written to the terminal, ending with:

```fortran
  ...
  DEALLOCATE(rnfmsk)
  DEALLOCATE(upsmsk)
  DEALLOCATE(rnfmsk_z)
  DEALLOCATE(tsn)
  CALL timer_report
END PROGRAM tra_adv
```

When examining the PSyIR (the internal representation that PSyclone
uses to generate and translate code) and writing transformation
scripts, it may be useful to see the documentation of the various node
types. The best way to do this is to use the PSyclone Reference Guide
available on
[readthedocs](https://psyclone-ref.readthedocs.io/en/latest/).

## 1. Processing NEMO Fortran code with PSyclone ##

The configuration file distributed with PSyclone sets the default API
to be LFRic. In order to specify that we are processing existing code
(i.e. that is not written in a DSL) we must supply `-api nemo`:

```bash
    $ psyclone -api nemo tra_adv.F90
```

This command should result in PSyclone processing the supplied Fortran
and then re-generating it and writing it to stdout:

```bash
    Transformed algorithm code:
    None
    Generated psy layer code:
     PROGRAM tra_adv
      USE iso_c_binding, ONLY: C_INT64_T
      INTEGER, PARAMETER :: wp = 8
      ...
```

Note that there is no algorithm code because the input code does not
follow the PSyKAl separation of concerns (see the [User
Guide](https://psyclone.readthedocs.io/en/stable/introduction.html#introduction)
for a description of PSyKAl). Instead, PSyclone treats each subroutine
(or program) as though it is a manually-written PSy layer. (An
unfortunate consequence of PSyclone's initial PSyKAl-focused development
is that each subroutine is called an 'invoke'. This will be improved in
future releases.)

In order to compile the Fortran that is output by PSyclone, we need it
to be written to a file instead of stdout. This is achieved with the
`-opsy` flag so that doing:

```bash
    $ psyclone -api nemo -opsy psy.f90 tra_adv.F90
```

will create a new file, `psy.f90`, containing the generated Fortran
code. As it stands, this file does not contain standards-compliant
Fortran because it has a number of lines that are more than 132
characters in length. There are two possible solutions to this: tell
PSyclone that it must limit the length of output lines or tell your
Fortran compiler to allow non-standard line lengths. Since not all
Fortran compilers allow the line-length limit to be ignored, we
instruct PSyclone to limit the line lengths in the output Fortran via
the `-l output` flag:

```bash
    $ psyclone -api nemo -opsy psy.f90 -l output tra_adv.F90
```

(Note that if we also wanted PSyclone to validate that the *incoming*
code was standards compliant then we could specify `-l all` instead).

Compiling the generated code is then a matter of doing (assuming gfortran is
the Fortran compiler):

```bash
    $ gfortran -o tra_adv.exe psy.f90
```

The mini-app picks-up the domain size and number of iterations from
environment variables. The file `../domain_setup.sh` contains example
settings for bash and `../domain_setup.csh` is the equivalent if you are
using csh or tcsh. You can either cut-n-paste the commands into your
shell or do (for csh):

```bash
    $ source ../domain_setup.csh
```

or (for bash):

```bash
    $ . ../domain_setup.sh
```

Once the environment variables are set, you are ready to execute the
mini-app:

    $ ./tra_adv.exe 
    Tracer-advection Mini-app:
    Domain is  100x 100 grid points
    Performing   10 iterations
    Mini-app finished.

At this point we have succeeded in processing some Fortran code
with PSyclone, generating new code and then compiling and running the
result. However, since we have not applied any transformations, the
generated code is functionally identical to that which was input.  In
order to apply transformations, we have to understand the Internal
Representation that PSyclone constructs for the supplied Fortran - the
PSyIR.

## 2. Obtaining the PSyIR ##

In order to examine the PSyIR for the mini-app we will supply PSyclone
with a transformation script, `schedule_view_trans.py`. This is done
via the `-s` flag to PSyclone (note that the directory containing the
transformation script - `./` in this case - must be specified
otherwise PSyclone will be unable to find the script):

```bash
    $ psyclone -api nemo -l output -opsy psy.f90 -s ./schedule_view_trans.py ./tra_adv.F90
```

This should report that it has found one 'invoke' named `tra_adv` (the
name of the Fortran program) and proceed to display a text view of the
PSyIR of that invoke. This will be a lot of output, beginning with:

```bash
    Invokes found:
    tra_adv

    NemoInvokeSchedule[invoke='tra_adv']
        0: Call[name='get_environment_variable']
            Literal[value:'JPI', Scalar<CHARACTER, UNDEFINED>]
            Reference[name:'env']
        1: CodeBlock[[<class 'fparser.two.Fortran2003.Read_Stmt'>]]
        ...
```

(Note that if you have the `termcolor` Python package installed
[`pip install termcolor`] then the PSyIR will be displayed with colour
highlighting.) Since the tracer-advection mini-app consists of a single
program unit, it is represented in PSyclone by a single `Invoke`. The
content of the program (the sequence of executable statements) is
described by a `NemoInvokeSchedule` in the PSyIR (a subclass of `Schedule`).


## 3. Interpreting the PSyIR ##

The basic structure and means of navigating the PSyIR are covered in the
PSyIR part of the
[tutorial](../../../notebooks/psyir/psyir_example2.ipynb). In
summary, all nodes in the PSyIR have `parent` and `children`
properties and a `walk` method which may be used to find all nodes of
a given type (or types) below the current node. Various sub-classes of
Node also support semantic navigation. For instance, the Loop node has
`loop_body`, the If node has `condition`, `if_body` and `else_body`
and the Directive node has `directive_body`.

If you examine the PSyIR output for the mini-app, you will see that
the second child node of the `NemoInvokeSchedule` is a
`CodeBlock`. This is an important node type since it makes it possible
for the PSyIR to represent arbitrary Fortran code without requiring
that it be fully understood. Since PSyclone uses fparser2 to parse
Fortran, a `CodeBlock` stores the nodes of the underlying fparser2
parse tree that cannot be represented in the PSyIR. For more
information on fparser2 see the associated
[tutorial](../../../notebooks/fparser2/parsing_fortran.ipynb) or the
[User Guide](https://fparser.readthedocs.io/en/latest/).

We can see from the fparser2 node type printed in the description of
the `CodeBlock` that this particular node represents a Fortran `READ`
statement. This is not computationally significant and therefore is not
interesting from a performance point of view.

1. Modify the transformation script so that it breaks-out into the Python
   debugger once it has obtained the `Schedule` of the Invoke:

   ```python
   for invoke in psy.invokes.invoke_list:

       sched = invoke.schedule
       import pdb; pdb.set_trace()
   ```

   Re-running PSyclone:
    
       $ psyclone -api nemo -s ./schedule_view_trans.py tra_adv.F90

   will now launch the Python debugger at that point:

       -> if not sched:
       (Pdb) 

   You can now interactively explore the Schedule and try the `walk`
   method, e.g.:

   ```python
   (Pdb) sched.children
   [<psyclone.psyir.nodes.call.Call object at 0x7fa00a6a0d90>, <psyclone.psyir.nodes.codeblock.CodeBlock object at 0x7fa00a6a0be0>, ...]
   (Pdb) cblocks = sched.walk(CodeBlock)
   (Pdb) cblocks
   [<psyclone.psyir.nodes.codeblock.CodeBlock object at 0x7fee49247790>, <psyclone.psyir.nodes.codeblock.CodeBlock ...]
   ```

   (Once you are done you can *quit the debugger* by doing `Ctrl-d` or
   by entering the `quit()` command.)

2. Modify the transformation script so that it uses `walk` to search
   for all of the CodeBlocks in the Schedule and prints information
   about each of them.  Work out which lines of Fortran in the
   mini-app each corresponds to. (A CodeBlock has the `get_ast_nodes`
   property which will return a list of all of the fparser2 nodes
   that it contains.)

For now we note that since, by definition, PSyclone does not
understand the contents of a CodeBlock, it is not possible (with the
exception of profiling) to apply transformations to regions of code
that contain them.

From a computational-performance standpoint, the most important nodes
are [`Loop`](https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1psyir_1_1nodes_1_1loop_1_1Loop.html) and
[`InlinedKern`](https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1psyGen_1_1InlinedKern.html). The NEMO API makes heavy use of
`InlinedKern` rather than the `Kern` used in other PSyclone APIs. This is
because it works with source code that does not follow the PSyKAl separation
of concerns. Instead, in constructing the PSyIR, the input code is treated
conceptually as a manually-written PSy layer in which calls to kernels have
been in-lined. Therefore an `InlinedKern` corresponds to the body of a loop
in the original source code.

3. Modify the transformation script to obtain a list of all of the
   `InlinedKern` nodes:

   ```python
   sched = invoke.schedule
   kernels = sched.walk(InlinedKern)
   ```

4. Use the `view()` method of one of these `InlinedKern` nodes to
   examine its `Schedule`. Check that you are able to work out
   which Fortran loop body this corresponds to, e.g.:

   ```python
   print(kernels[0].view())
   ```

## 4. Conclusion

Congratulations, you have now completed this section of the tutorial.
At this point you should be able to run PSyclone on a Fortran source
file, use a transformation script to access the PSyIR of the code and
be able to understand the structure of the PSyIR and how it relates to
the original Fortran.
