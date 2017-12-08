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
PSy layer before the associated Fortran code is actually
generated. These transformations are supplied by the user in the form
of a Python script which makes use of the various Transformation
classes implemented in PSyclone to modify the Abstract Syntax Tree
(AST) of the PSy layer. Any user-supplied kernels (written in Fortran)
are treated as 'black-boxes' and are left untouched during this
process. In order to modify the kernels themselves, the user-supplied
script must invoke CLAW.

In contrast to PSyclone's native transformations which operate on the
PSy AST, CLAW operates on Fortran source code. In this case this will
be one or more of the kernels called from the PSy layer.  PSyclone
already has the location of these kernels since the meta-data they
contain must be parsed. This location information must now also be
passed to the transformation script in case it wishes to modify the
kernels.

Rather than re-name any transformed kernels, PSyclone instead creates
them in a user-specified location. This information must be passed
to PSyclone when it is invoked from the command line. An example script
that uses CLAW to transform kernels might look like:

::

    def trans(psy, kernel_search_path, out_dir):

        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        # Get an OpenMPLoop-transformation
        from psyclone.transformations import OMPParallelLoopTrans
        ol = OMPParallelLoopTrans()

        # Apply it to the first loop in the schedule
        new_schedule, memento = ol.apply(schedule.children[0])

	# Transform the associated kernel
	from psyclone import claw
	kern = schedule.children[0].children[0]
	kernel_list = [kern.name]
	claw_script = "some jython file"
        claw.trans(kernel_list, kernel_search_path, claw_script, out_dir)

        return psy
