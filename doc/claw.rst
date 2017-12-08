.. Copyright (C) 2017, Science and Technology Facilities Council, UK

CLAW
====

CLAW is a Fortran source-to-source compiler written by V. Clement of
CSCS. It is normally driven by the use of CLAW pragmas added to source
code. PSyclone makes use of CLAW for the purpose of transforming
kernel code to make it suitable/performant for e.g. use with OpenACC.
For this purpose, PSyclone includes code that interfaces with CLAW and
enables it to be used from within transformation scripts.

Prerequisites
-------------

In order to use the CLAW functionality you will obviously need the CLAW
compiler installed. This in turn requires the OMNI compiler.
You will also need Jython (a Java implementation of Python).

Installation/Configuration
--------------------------


Using CLAW with PSyclone
------------------------

A key concept in PSyclone is the application of transformations to the
generated PSy layer. These are supplied by the user in the form of a
Python script which makes use of the various Transformation classes
implemented in PSyclone. These transformations affect only the PSy
layer - any user-supplied kernels (written in Fortran) are treated as
'black-boxes' and are left untouched. In order to modify the kernels
themselves, the user-supplied script must invoke CLAW. This may be
done through the ``claw`` module supplied with PSyclone, e.g.:

::
    # Parse the file containing the algorithm specification
    _, invokeInfo = parse("dynamo.F90", api=api)

    # Create the PSy-layer object using the invokeInfo
    psy = PSyFactory(api).create(invokeInfo)


    # Get the schedule associated with the required invoke
    invoke = psy.invokes.get('invoke_0_v3_kernel_type')
    schedule = invoke.schedule

    # Create an OpenMPLoop-transformation
    ol = t.get_trans_name('OMPParallelLoopTrans')

    # Apply it to the loop schedule of the selected invoke
    new_schedule,memento = ol.apply(schedule.children[0])

