.. Copyright (C) 2017-2018, Science and Technology Facilities Council, UK

.. _claw:

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
compiler installed. This in turn requires the Omni compiler.
You will also need Jython (a Java implementation of Python).

Installation/Configuration
--------------------------

Omni Compiler
^^^^^^^^^^^^^

The Omni Fortran compiler is distributed as a sub-module within the
CLAW git repository. See the INSTALL.md file that comes with CLAW for
instructions on building and installing that version.

Alternatively (e.g. if you haven't git-cloned the CLAW repository),
the Omni Fortran compiler is available from http://omni-compiler.org/.
PSyclone has been tested with version 1.2.2 but should work with any
version that is more recent than this.

In order to build Omni you will first need to install yacc, byacc,
libxml2-dev, flex and the Java development kit (we have tested with
version 7). You will also need an MPI-wrapper for your C compiler. For
gcc this is provided by e.g. OpenMPI or MPICH. See README.md in the Omni
root directory for information on configuring and building.

Once Omni has been built and installed it will need to be available
on your PATH so that PSyclone can find it.

Jython
^^^^^^

Jython is a Java implementation of Python and is available from
http://www.jython.org/downloads.html. Once you have downloaded the
installer jar, installation is performed by doing:

::

    java -jar jython-installer-2.7.0.jar

You will then need to edit the PSyclone configuration file and specify
the location of the jython.jar file.

CLAW
^^^^

The version of the CLAW compiler used by PSyclone is available from
https://github.com/stfc/claw-compiler.  See the INSTALL.md file for
installation instructions.  Note that you will need to ensure that it
is built with Jython support by using the
"-DJYTHON_DIR=/path/to/jython" option to CMake. Once it is installed,
you will need to manually copy the Python interface module into the
`lib` directory of the Claw installation:

::

    cp claw-compiler/cx2t/src/claw/python/ClawTransform.py <claw-install-root>/lib

.. note:: The need to manually copy the `ClawTransform.py` file will be removed in a future release of the CLAW compiler.


Configure PSyclone
^^^^^^^^^^^^^^^^^^

In order for PSyclone to make use of CLAW, it must be told the
location of the Jython jar file (`jython.jar`) and the root directory
of the CLAW installation. This is achieved by editing the
`src/claw_config.py` configuration file and setting `JYTHON_JAR` and
`CLAW_INSTALL_ROOT` appropriately, e.g.:

::

  # Location of the Jython Jar
  JYTHON_JAR = "/home/myuser/MyInstalls/jython2.7.0/jython.jar"
  # Root directory of the CLAW installation
  CLAW_INSTALL_ROOT = "/home/myuser/MyInstalls"

.. note:: the actual location of the `claw_config.py` file after installing PSyclone via pip is still to be determined.

CLAW relies upon the front-end of the Omni compiler to generate the
XCodeML/F representation of any Fortran code that is to be
transformed.  In turn, the Omni compiler requires a `.xmod` file for
each Fortran module that is USE'd in the Fortran code being
analysed. A `.xmod` file is obtained by running the Omni front-end on
the corresponding Fortran module. It therefore follows that in order
to use CLAW to transform a kernel, we require `.xmod` files for all of
the Fortran modules on which it depends. The location of these files
is specified in `claw_config.py` through the `OMNI_MODULES_PATH`
dictionary. The keys of this dictionary are the names of supported
PSyclone APIs (e.g. "dynamo0.3"). The corresponding entries contain
the path to a directory where the `.xmod` files for the API may be
found, e.g.:

::

  # Dictionary (indexed by PSyclone API name) containing the location of
  # Omni-compiled Fortran modules use'd by any kernels to be transformed
  # by CLAW.
  OMNI_MODULES_PATH = {"dynamo0.3":
                       os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "tests", "test_files", "dynamo0p3",
                                    "infrastructure"),
                       "gocean1.0":
                       "/home/myuser/Projects/dl_esm_inf/finite_difference/"
                       "src"}

The creation of the `.xmod` files themselves is the responsibility of
the user. However, a suitable Makefile is provided for the 'stub'
LFRic infrastructure that is distributed with PSyclone (in
`PSyclone/src/psyclone/tests/test_files/dynamo0p3/infrastructure`).

.. note:: the stub LFRic infrastructure was originally introduced to allow for compilation of generated code within the PSyclone test suite, hence its location. It may be moved to a more appropriate location in future.


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
Omni compiler. This process of obtaining an XcodeML/F kernel AST
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

	# Use CLAW to transform the associated kernel
	kernel_list = [schedule.children[0].children[0]]
	claw_script = "some_jython_file.py"
        claw.trans(kernel_list, claw_script)

        return psy

As can be seen in the above example, the application of CLAW to one or
more kernels is performed through the `claw.trans()` routine:

.. autofunction:: psyclone.claw.trans
    :noindex:

Just as standard PSyclone transformations must be implemented within a
`trans()` function in a supplied Python script file, CLAW
transformations must be implemented within a `claw_trans()`
function. This too uses Python but is actually executed by Jython
since this gives access to the (Java) transformations provided by CLAW.
The `claw_trans` function must accept the XcodeML/F AST of the kernel to
be transformed and return an object of the same type, i.e.:
::

    def claw_trans(xast):
        ...
        return xast

Two examples are distributed with PSyclone in the
`examples/transformations/claw` directory. The essentials of one of
these is reproduced below:
::

    def claw_trans(xast):
        from claw.tatsu.primitive import Loop
        from claw.tatsu.xcodeml.abstraction import NestedDoStatement
        from claw.tatsu.xcodeml.xnode.common import Xcode

        do_loops = xast.matchAll(Xcode.F_DO_STATEMENT)

        # Perform a simple loop interchange (inner becomes outer)
        # using CLAW primitives
        nested_loop = NestedDoStatement(do_loops[0])
        Loop.reorder(nested_loop, ["jj", "ji"])

        return xast

This example queries the AST to find all DO loops and then applies a
CLAW primitive to the first one in order inter-change the inner and
outer loops. Note that, for brevity, no attempt has been made to
verify that the supplied kernel has the expected structure.

.. note:: We need a link to the CLAW documentation describing the available transforms. However, I'm not sure that such documentation exists yet.
