.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2021, Science and Technology Facilities Council.
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
directory. The command takes an LFRic tangent-linear kernel file and a
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
    usage: psyad [-h] [-oad OAD] [-v] [-t] [-otest TEST_FILENAME] -a ACTIVE [ACTIVE ...] -- filename

    Run the PSyclone adjoint code generator on an LFRic tangent-linear kernel file

    positional arguments:
      filename       LFRic tangent-linear kernel source

    optional arguments:
      -h, --help            show this help message and exit
      -a ACTIVE [ACTIVE ...], --active ACTIVE [ACTIVE ...]
                            name of active variables
      -v, --verbose         increase the verbosity of the output
      -t, --gen-test        generate a standalone unit test for the adjoint code
      -otest TEST_FILENAME  filename for the unit test (implies -t)
      -oad OAD              filename for the transformed code

Basic Use
---------

The simplest way to use ``psyad`` is to provide it with an LFRic
tangent-linear kernel file and the names of the active variables
within the kernel file
::

    > psyad -a var1 var2 -- tl_kern.f90

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

Processing Information
----------------------

To see more information about what the psyad script is doing
internally you can specify the ``-v`` option. For example
::

   > psyad -a var1 var2 -oad ad_kern.f90 -v tl_kern.f90
