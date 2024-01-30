.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2021-2024, Science and Technology Facilities Council.
.. All rights reserved.
..
.. Redistribution and use in source and binary forms, with or without
.. modification, are permitted provided that the following conditions are met:
..
.. * Redistributions of source code must retain the above copyright notice, this
..   list of conditions and the following disclaimer.
..
.. * Redistributions in binary form must reproduce the above copyright notice,
..   this list of conditions and the following disclaimer in the documentation
..   and/or other materials provided with the distribution.
..
.. * Neither the name of the copyright holder nor the names of its
..   contributors may be used to endorse or promote products derived from
..   this software without specific prior written permission.
..
.. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
.. "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
.. LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
.. FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
.. COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
.. INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
.. BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
.. LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
.. CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.. LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
.. ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
.. POSSIBILITY OF SUCH DAMAGE.
.. -----------------------------------------------------------------------------
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab

.. _psyad_command:

The ``psyad`` Command
=====================

The simplest way to run PSyAD is to use the ``psyad`` command. If you
installed PSyclone using ``pip`` then this command should be available
on your PATH (see the PSyclone user guide for more
details). Alternatively it can be found in the ``<PSYCLONEHOME>/bin``
directory. The command takes a file containing a tangent-linear kernel
(either a generic Fortran subroutine or an LFRic kernel) and a
list of active variables as input and outputs its adjoint. This
section walks through the functionality of the command.

Running
-------

The ``psyad`` command is an executable script designed to be run from the
command line, e.g.:
::

  > psyad <args>

The optional ``-h`` argument gives a description of the options provided
by the command:

.. parsed-literal::
		
  >>> psyad -h
    usage: psyad [-h] [-oad OAD] [-v] [-t] [-api API] [-coord-arg COORD_ARG] [-panel-id-arg PANEL_ID_ARG] [-otest TEST_FILENAME] -a ACTIVE [ACTIVE ...] -- filename

    Run the PSyclone adjoint code generator on a tangent-linear kernel file

    positional arguments:
      filename              tangent-linear kernel source

    optional arguments:
      -h, --help            show this help message and exit
      -a ACTIVE [ACTIVE ...], --active ACTIVE [ACTIVE ...]
                            name of active variables
      -v, --verbose         increase the verbosity of the output
      -t, --gen-test        generate a standalone unit test for the adjoint code
      -api API              the PSyclone API that the TL kernel conforms to (if any)
      -coord-arg COORD_ARG  the position of the coordinate (chi) field in the
                            meta_args list of arguments in the kernel metadata
                            (LFRic only)
      -panel-id-arg PANEL_ID_ARG
                            the position of the panel-ID field in the meta_args
                            list of arguments in the kernel metadata (LFRic only)
      -otest TEST_FILENAME  filename for the unit test (implies -t)
      -oad OAD              filename for the transformed code

Basic Use
---------

The simplest way to use ``psyad`` is to provide it with an LFRic
tangent-linear kernel file and the names of the active variables
within the kernel file
::

    > psyad -api dynamo0.3 -a var1 var2 -- tl_kern.f90

If the kernel file or active variables are invalid for some reason,
the command should return with an appropriate error.

If the kernel file and active variables are valid then the adjoint
kernel code will be output to the terminal screen.

The ``--`` is required to separate the filename from the list of
active variables.

Alternatively the list of active variables may be supplied as the last
command-line argument (or the list of active variables may be followed
by another optional argument [see the next section])
::
   
   > psyad tl_kern.f90 -a var1 var2

.. note:: PSyAD does not support tangent-linear code written as a
          function and will raise an exception if a function is
          found. The suggested solution is to re-write the code as a
          subroutine. The reason for this limitation is that there is
          no natural way to translate such code to its adjoint form
          without making the adjoint a subroutine.

File Output
-----------

By default the adjoint kernel code is output to the terminal. This can
instead be output to a file by using the ``-oad <file>`` option. For
example, the following will output the adjoint kernel code to the file
'ad_kern.f90'.
::

    > psyad -oad ad_kern.f90 -a var1 var2 -- tl_kern.f90

As mentioned in the previous section, the ``--`` is not required if
the list of active variables is followed by an optional argument, so
an alternative form is
::
   
    > psyad -a var1 var2 -oad ad_kern.f90 tl_kern.f90


.. _test_harness_gen:

Test Harness
------------

``psyad`` also supports the optional generation of test-harness code which
can be compiled together with the original tangent-linear kernel and
generated adjoint kernel to test that the adjoint code is correct. The
harness code will be generated if the ``-t`` option is specified. The
following will output the test-harness code to the terminal screen
::

   > psyad -oad ad_kernel.f90 -a var1 var2 -t tl_kern.f90

The test-harness code can be written to file by using the ``-otest
<file>`` option. For example, the following will output the
test-harness code to the file 'harness.f90'
::

   > psyad -oad ad_kernel.f90 -a var1 var2 -t -otest harness.f90 tl_kern.f90

If the ``-otest <file>`` option is provided then the ``-t`` option is
assumed so is not required. Therefore the following is equivalent to
the previous command
::

   > psyad -oad ad_kernel.f90 -a var1 var2 -otest harness.f90 tl_kern.f90

.. note:: A test harness can only be generated if the supplied tangent-linear
	  kernel is in the form of a subroutine contained within a module.
	  PSyAD will report an error if this is not the case.

The test harness created for a generic Fortran subroutine is a standalone
program that must be linked with the TL and adjoint kernels to produce an
executable. Since LFRic kernels require properties provided by the LFRic
infrastructure (function spaces, dof maps, geometry etc.), the test harness
produced for such a kernel is in the form of an Algorithm subroutine
that must be called from within a full LFRic mini-app. Please see the
:ref:`psyad_examples` for more information.

.. _geom_kernel_args:

Kernel Arguments Containing Geometric Information
+++++++++++++++++++++++++++++++++++++++++++++++++

By default, the test harness code generated by PSyAD populates all real,
scalar and field arguments to a kernel with pseudo-random data. (Integer and
logical scalar arguments are currently set to `1` and `.False.`, respectively
- see issue #2087.) However, certain
LFRic kernels have arguments which carry information on the geometry of the
simulation mesh (either or both of panel IDs and mesh coordinates) and these
must be preserved if the kernel is to execute
correctly. PSyAD therefore supports the ``-panel-id-arg`` and ``-coord-arg``
flags which allow the user to specify that a particular kernel argument
corresponds to either the field of panel IDs or mesh coordinates, respectively.
Each of these flags must be followed by the position (indexed from 1) of the
corresponding argument in the list of ``meta_args`` in the kernel
:ref:`metadata <user_guide:dynamo0.3-api-kernel-metadata>`.

PSyAD will return an error if the specified kernel argument is not consistent
with the particular geometry field that it is supposed to represent.

Logging Output
--------------

To see more information about what the psyad script is doing
internally you can specify the ``-v`` option. For example
::

   > psyad -a var1 var2 -oad ad_kern.f90 -v tl_kern.f90
