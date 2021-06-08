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

The psyad command
=================

The simplest way to run PSyAD is to use the ``psyad`` command. If you
installed PSyclone using ``pip`` then this command should be available
on your PATH (see the PSyclone user guide for more
details). Alternatively it can be found in the ``<PSYCLONEHOME>/bin``
directory. The command takes an LFRic tangent linear kernel file as
input and outputs its adjoint. This section walks through the
functionality of the command.

Running
-------

The ``psyad`` command is an executable script designed to be run from the
command line, e.g.:
::

  > psyad <args>

The optional ``-h`` argument gives a description of the options provided
by the command:

.. parsed-literal::
		
  > psyad -h
    usage: psyad [-h] [-v] [-oad OAD] filename

    Run the PSyclone adjoint code generator on an LFRic tangent-linear kernel file

    positional arguments:
      filename       LFRic tangent-linear kernel source

    optional arguments:
      -h, --help     show this help message and exit
      -v, --verbose  increase the verbosity of the output
      -oad OAD       filename for the transformed code

Basic Use
---------

The simplest way to use ``psyad`` is to provide it with an LFRic
tangent-linear kernel file::

    > psyad tl_kern.f90

If the kernel file is invalid for some reason, the command should
return with an appropriate error.

If the kernel file is valid then the adjoint kernel code will be
output to the terminal screen.

File output
-----------

By default the adjoint kernel code is
output to the terminal. This can instead be output to a file by using the
``-oad <file>`` option. For example, the
following will output the adjoint kernel code to the file 'ad_kern.f90'.
::

    > psyad -oad ad_kern.f90 tl_kern.f90

Processing information
----------------------

To see more information about what the psyad script is doing
internally you can specify the ``-v`` option. For example
::

   > psyad -oad ad_kern.f90 -v tl_kern.f90
