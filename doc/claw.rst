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

* Install Omni (>=1.2.2) from http://omni-compiler.org/.
* Install Jython.
* Install Claw.
  cp claw-compiler/omni-cx2x/src/claw/python/ClawTransform.py <claw-install-root>/lib
* Configure `src/claw_config.py`.

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

Just as PSyclone's native transformations operate on the PSy AST, CLAW
also operates on the AST of a kernel. In this case however the AST is
in XcodeML/F form which is obtained by running the front-end of the
OMNI compiler. This process of obtaining an XcodeML/F kernel AST
(instead of the one produced by fparser) is performed 'under the hood'
of the PSyclone interface to CLAW.

Each transformed kernel is written to the same location as the generated
PSy-layer code. In order to distinguish these kernels from the originals,
they are given unique names (based on their original name and the name of
the PSy-layer routine that calls them) and the PSy AST updated
to use them. 

An example script that uses CLAW to transform kernels might look like:

::

    def trans(psy):
        from psyclone.transformations import OMPParallelLoopTrans
	from psyclone import claw

        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        # Get an OpenMPLoop-transformation
        ol = OMPParallelLoopTrans()

        # Apply it to the first loop in the schedule
        new_schedule, memento = ol.apply(schedule.children[0])

	# Transform the associated kernel for the one invoke
	kern = schedule.children[0].children[0]
	kernel_list = [kern.name]
	invoke_list = [invoke]
	claw_script = "some jython file"
        claw.trans(invoke_list, kernel_list, claw_script)

        return psy
