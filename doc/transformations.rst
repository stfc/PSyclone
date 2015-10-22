Transformations
===============

As discussed in the previous section, transformations can be applied
to a schedule to modify it. Typically transformations will be used to
optimise the PSy layer for a particular architecture, however
transformations could be added for other reasons, such as to aid
debugging or for performance monitoring.

Finding
-------

Transformations can be imported directly, but the user needs to know
what transformations are available. A helper class **TransInfo** is
provided to show the available transformations

.. autoclass:: psyGen.TransInfo
    :members:

Available
---------

Most transformations are generic as the schedule structure is
independent of the API, however it often makes sense to specialise
these for a particular API by adding API-specific errors checks. Some
transformations are API-specific (or specific to a set of API's
e.g. dynamo). Currently these different types of transformation are
indicated by their names.

The generic transformations currently available are given below (a
number of these have specialisations which can be found in the
API-specific sections).

.. autoclass:: transformations.LoopFuseTrans
    :members:
    :noindex:

.. autoclass:: transformations.ColourTrans
    :members:
    :noindex:

.. autoclass:: transformations.OMPLoopTrans
    :members:
    :noindex:

.. autoclass:: transformations.OMPParallelTrans
    :members:
    :noindex:

.. autoclass:: transformations.OMPParallelLoopTrans
    :members:
    :noindex:

Applying
--------

Transformations can be applied either interactively or through a
script.

.. _sec_transformations_interactive:

Interactive
+++++++++++

To apply a transformation interactively we first parse and analyse the
code. This allows us to generate a "vanilla" PSy layer. For example ...
::

    from parse import parse
    from psyGen import PSyFactory

    # This example uses version 0.1 of the Dynamo API
    api = "dynamo0.1"

    # Parse the file containing the algorithm specification and
    # return the Abstract Syntax Tree and invokeInfo objects
    ast, invokeInfo = parse("dynamo.F90", api=api)

    # Create the PSy-layer object using the invokeInfo
    psy = PSyFactory(api).create(invokeInfo)

    # Optionally generate the vanilla PSy layer fortran
    print psy.gen

We then extract the particular schedule we are interested
in. For example ...
::

    # List the various invokes that the PSy layer contains
    print psy.invokes.names

    # Get the required invoke
    invoke = psy.invokes.get('invoke_0_v3_kernel_type')

    # Get the schedule associated with the required invoke
    schedule = invoke.schedule
    schedule.view()


Now we have the schedule we can create and apply a transformation to
it to create a new schedule and then replace the original schedule
with the new one. For example ...
::

    # Get the list of possible loop transformations
    from psyGen import TransInfo
    t = TransInfo()
    print t.list

    # Create an OpenMPLoop-transformation
    ol = t.get_trans_name('OMPParallelLoopTrans')

    # Apply it to the loop schedule of the selected invoke
    new_schedule,memento = ol.apply(schedule.children[0])
    new_schedule.view()

    # Replace the original loop schedule of the selected invoke
    # with the new, transformed schedule 
    invoke.schedule=new_schedule

    # Generate the Fortran code for the new PSy layer
    print psy.gen

More examples of use of the interactive application of transformations
can be found in the runme*.py files within the examples/dynamo/eg1 and
examples/dynamo/eg2 directories. Some simple examples of the use of
transformations are also given in the previous section.

Script
++++++

PSyclone provides a Python script (**generator.py**) that can be used from
the command line to generate PSy layer code and to modify algorithm
layer code appropriately. By default this script will generate
"vanilla" (unoptimised) PSy layer code. For example:
::

    > python generator.py algspec.f90
    > python generator.py -oalg alg.f90 -opsy psy.f90 -api dynamo0.3 algspec.f90

The generator.py script has an optional **-s** flag which allows the
user to specify a script file to modify the PSy layer as
required. Script files may be specified without a path. For
example:
::

    > python generator.py -s opt.py algspec.f90

In this case the Python search path **PYTHONPATH** will be used to try
to find the script file.

Alternatively, script files may be specified with a path. In this case
the file is expected to be found in the specified location. For
example ...
::

    > python generator.py -s ./opt.py algspec.f90
    > python generator.py -s ../scripts/opt.py algspec.f90
    > python generator.py -s /home/me/PSyclone/scripts/opt.py algspec.f90

PSyclone also provides the same functionality via a function (which is
what the **generator.py** script calls internally)

.. autofunction:: generator.generate
		  :noindex:

A valid script file must contain a **trans** function which accepts a **PSy**
object as an argument and returns a **PSy** object, i.e.:
::

    def trans(psy)
        ...
        return psy

It is up to the script what it does with the PSy object. The example
below does the same thing as the example in the
:ref:`sec_transformations_interactive` section.
::

    def trans(psy):
	from transformations import OMPParallelLoopTrans
        invoke = psy.invokes.get('invoke_0_v3_kernel_type')
        schedule = invoke.schedule
        ol = OMPParallelLoopTrans()
        new_schedule, _ = ol.apply(schedule.children[0])
        invoke.schedule = new_schedule
	return psy

Of course the script may apply as many transformations as is required
for a particular schedule and may apply transformations to all the
schedules (i.e. invokes) contained within the PSy layer.

An example of the use of transformations scripts can be found in the
examples/dynamo/eg3 directory. Please read the examples/dynamo/README
file first as it explains how to run the example.

