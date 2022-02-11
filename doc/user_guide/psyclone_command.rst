.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
.. Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
.. Modified by I. Kavcic, Met Office

.. _psyclone_command:

The psyclone command
====================

The simplest way to run PSyclone is to use the ``psyclone`` command. If
you installed PSyclone using ``pip`` then this command should be available
on your PATH (see :ref:`getting-going-env` for more
details). Alternatively it can be found in the ``<PSYCLONEHOME>/bin``
directory. The command takes an algorithm file as input and outputs
modified algorithm code and generated PSy code. This section walks
through its functionality.

Running
-------

The ``psyclone`` command is an executable script designed to be run from the
command line, e.g.:
::

  > psyclone <args>

The optional ``-h`` argument gives a description of the options provided
by the command:

.. parsed-literal::

  > psyclone -h

  usage: psyclone [-h] [-oalg OALG] [-opsy OPSY] [-okern OKERN] [-api API]
                  [-s SCRIPT] [-d DIRECTORY] [-I INCLUDE] [-l {off,all,output}]
                  [-dm] [-nodm] [--kernel-renaming {multiple,single}]
                  [--profile {invokes,kernels}] [--config CONFIG] [-v]
                  filename

  Run the PSyclone code generator on a particular file

  positional arguments:
    filename              algorithm-layer source code

  optional arguments:
    -h, --help            show this help message and exit
    -oalg OALG            filename of transformed algorithm code
    -opsy OPSY            filename of generated PSy code
    -okern OKERN          directory in which to put transformed kernels
    -api API              choose a particular api from ['dynamo0.3',
                          'gocean1.0', 'nemo'], default 'dynamo0.3'.
    -s SCRIPT, --script SCRIPT
                          filename of a PSyclone optimisation script
    -d DIRECTORY, --directory DIRECTORY
                          path to a root directory structure containing kernel
                          source code. Multiple roots can be specified by using
                          multiple -d arguments.
    -I INCLUDE, --include INCLUDE
                          path to Fortran INCLUDE files (nemo API only)
    -l {off,all,output}, --limit {off,all,output}
                          limit the Fortran line length to 132 characters
                          (default 'off'). Use 'on' to apply limit to both input
                          and output Fortran. Use 'output' to apply line-length
                          limit to output Fortran only.
    -dm, --dist_mem       generate distributed memory code
    -nodm, --no_dist_mem  do not generate distributed memory code
    --kernel-renaming {single,multiple}
                          Naming scheme to use when re-naming transformed
                          kernels.
    --profile {invokes,kernels}, -p {invokes,kernels}
                          Add profiling hooks for either 'kernels' or 'invokes'
    --config CONFIG       Config file with PSyclone specific options.
    -v, --version         Display version information (\ |release|\ )

Basic Use
---------

The simplest way to use ``psyclone`` is to provide it with an
algorithm file::

    > psyclone alg.f90

If the algorithm file is invalid for some reason, the command should
return with an appropriate error. For example, if we use the Python
``psyclone-kern`` script as an algorithm file we get the following::

    > psyclone <PSYCLONEHOME>/bin/psyclone-kern
    Parse Error: algorithm.py:parse_fp2: Syntax error in file '<PSYCLONEHOME>/bin/psyclone-kern':
    at line 1
    >>>#!/usr/bin/env python

If the algorithm file is valid then the modified algorithm code and
the generated PSy code will be output to the terminal screen.

Choosing the API
----------------

In the previous section we relied on PSyclone using the default
API. The default API, along with the supported APIs can be seen by
running the ``psyclone`` command with the ``-h`` option.

If you use a particular API frequently and it is not the default then
you can change the default by creating a copy of the default
``psyclone.cfg`` file and editing it. See :ref:`configuration` for
more details.

If your code uses an API that is different to the default then you can
specify this as an argument to the ``psyclone`` command.
::

    > psyclone -api gocean1.0 alg.f90

File output
-----------

By default the modified algorithm code and the generated PSy code are
output to the terminal. These can instead be output to files by using the
``-oalg <file>`` and ``-opsy <file>`` options, respectively. For example, the
following will output the generated PSy code to the file 'psy.f90' but
the algorithm code will be output to the terminal:
::

    > psyclone -opsy psy.f90 alg.f90

If PSyclone is being used to transform Kernels then the location to
write these to is specified using the ``-okern <directory>``
option. If this is not supplied then they are written to the current
working directory. By default, PSyclone will overwrite any kernel of
the same name in that directory. To change this behaviour, the user
can use the ``--no_kernel_clobber`` option. This causes PSyclone to
re-name any transformed kernel that would clash with any of those
already present in the output directory.

Algorithm files with no invokes
-------------------------------

If ``psyclone`` is provided with a file that contains no
``invoke`` calls then the command outputs a warning to ``stdout`` and
copies the input file to ``stdout``, or to the specified algorithm
file (if the ``-oalg <file>`` option is used). No PSy code will be
output. If a file is specified using the ``-opsy <file>`` option this file
will not be created.

.. code-block:: bash

    > psyclone -opsy psy.f90 -oalg alg_new.f90 empty_alg.f90
    Warning: 'Algorithm Error: Algorithm file contains no invoke() calls: refusing to
    generate empty PSy code'

Kernel search directory
-----------------------

When an algorithm file is parsed, the parser looks for the associated
kernel files. The way in which this is done requires that any
user-defined kernel routine (as opposed to :ref:`built-ins`) called
within an invoke must have an explicit use statement. For example, the
following code gives an error:

.. code-block:: bash

    > cat no_use.f90
    program no_use
      call invoke(testkern_type(a,b,c,d,e))
    end program no_use
    > psyclone -api gocean1.0 no_use.f90
    "Parse Error: kernel call 'testkern_type' must either be named in a use statement or be a recognised built-in (one of '[]' for this API)"

(If the chosen API has any :ref:`built-ins` defined then
these will be listed within the ``[]`` in the above error message.) If the
name of the kernel is provided in a use statement then the parser will
look for a file with the same name as the module in the use
statement. In the example below, the parser will look for a file
called "testkern.f90" or "testkern.F90":

.. code-block:: bash

    > cat use.f90
    program use
      use testkern, only : testkern_type
      call invoke(testkern_type(a,b,c,d,e))
    end program use

Therefore, for PSyclone to find kernel files, the module name of a
kernel file must be the same as its filename. By default the parser
looks for the kernel file in the same directory as the algorithm
file. If this file is not found then an error is reported.

.. code-block:: bash

    > psyclone use.f90 
    Kernel file 'testkern.[fF]90' not found in <location>

The ``-d`` option can be used to tell ``psyclone`` where to look for
kernel files by supplying it with a directory. The execution will recurse
from the specified directory path to look for the required file. There
must be only one instance of the specified file within (or below) the
specified directory:

.. code-block:: bash
		  
    > cd <PSYCLONEHOME>/src/psyclone
    > psyclone -d . use.f90 
    More than one match for kernel file 'testkern.[fF]90' found!
    > psyclone -d tests/test_files/dynamo0p3 -api dynamo0.3 use.f90 
    [code output]

.. note:: The ``-d`` option can be repeated to add as many search
    directories as is required, with the constraint that there must be
    only one instance of the specified file within (or below) the
    specified directories.

Transformation script
---------------------

By default the ``psyclone`` command will generate 'vanilla'
Algorithm-layer and PSy-layer code with unmodified kernels for the
gocean1.0 and lfric (dynamo0.3) APIs. For the nemo API, ``psyclone``
will not perform any transformations on the input code.

The -s option allows a Python script to be specified which can contain
PSyclone transformations to transform the code. This option is
discussed in more detail in the :ref:`sec_transformations_script`
section.

.. _fort_line_length:

Fortran line length
-------------------

By default the ``psyclone`` command will generate Fortran code with no
consideration of Fortran line-length limits. As the line-length limit
for free-format Fortran is 132 characters, the code that is output may
be non-conformant.

Line length is not an issue for many compilers as they
allow compiler flags to be set which allow lines longer than the
Fortran standard. However this is not the case for all compilers.

When either the ``-l all`` or ``-l output`` option is specified to
the ``psyclone`` command, the output will be line wrapped so that the
output lines are always within the 132 character limit.

The ``-l all`` additionally checks the parsed algorithm and kernel files for
conformance and raises an error if they do not conform.

Line wrapping is not performed by default. There are two reasons for
this. This first reason is that most compilers are able to cope with
long lines. The second reason is that the line wrapping implementation
could fail in certain pathological cases. The implementation and
limitations of line wrapping are discussed in the
:ref:`line-length-limitations` section.

Distributed memory
------------------

By default the ``psyclone`` command will generate distributed
memory (DM) code (i.e. parallelised using MPI). As with the choice of
API, this default may be configured by editing ``psyclone.cfg`` - see
:ref:`configuration`.  Alternatively, whether or not to generate DM
code can be specified as an argument to the ``psyclone`` command using
the ``-dm``/``--dist_mem`` or ``-nodm``/``--no_dist_mem`` flags,
respectively.

For details of PSyclone's support for generating DM code see
:ref:`distributed_memory`.

Automatic Profiling Instrumentation
-----------------------------------

The ``--profile`` option allows the user to instruct PSyclone to
automatically insert profiling calls within the generated PSy
code. Two options are provided, ``invokes`` and ``kernels``. The first of
these causes PSyclone to insert profiling-start and -stop calls at the
beginning and end of every generated invoke routine. The second puts
profiling calls around every kernel call (including the associated
loops). The generated code must be linked against the PSyclone
profiling interface and the profiling tool itself. The application
that calls the PSyclone-generated code is responsible for initialising
and finalising the profiling library that is being used.  For full
details on the use of this profiling functionality please see the
:ref:`profiling` section.

Outputting of Transformed Kernels
---------------------------------

When transforming kernels there are two use-cases to consider:

 1. a given kernel will be transformed only once and that version
    then used from multiple, different Invokes and Algorithms;
 2. a given kernel is used from multiple, different Invokes and
    Algorithms and is transformed differently, depending on the
    Invoke.

Whenever PSyclone is used to transform a kernel, the new kernel must
be re-named in order to avoid clashing with other possible calls to
the original. By default (``--kernel-renaming multiple``), PSyclone
generates a new, unique name for each kernel that is
transformed. Since PSyclone is run on one Algorithm file at a time, it
uses the chosen kernel output directory (``-okern``) to ensure that
names created by different invocations do not clash.  Therefore, when
building a single application, the same kernel output directory must
be used for each separate invocation of PSyclone.

Alternatively, in order to support use case 1, a user may specify
``--kernel-renaming single``: now, before transforming a kernel,
PSyclone will check the kernel output directory and if a transformed
version of that kernel is already present then that will be
used. Note, if the kernel file on disk does not match with what would
be generated then PSyclone will raise an exception.

Fortran INCLUDE Files
---------------------

For the NEMO API, if the source code to be processed by PSyclone
contains INCLUDE statements (other than those for libraries such as
MPI) then the location of any INCLUDE'd files must be supplied to
PSyclone via the ``-I`` or ``--include`` option. (This is necessary
because INCLUDE lines are a part of the Fortran language and must
therefore be parsed - they are not handled by any pre-processing
step.) Multiple locations may be specified by using multiple ``-I``
flags, e.g.::

    > psyclone api "nemo" -I /some/path -I /some/other/path alg.f90

If no include paths are specified then the directory containing the
source file currently being parsed is searched by default. If the
specified include file is not found then ideally the INCLUDE line
would be left unchanged. However, fparser currently treats any such
INCLUDE lines as comments which results in them being lost (fparser
issue #138). The workaround for this is to ensure that the location
of *all* INCLUDE files is supplied to PSyclone.

Attempting to specify ``-I``/``--include`` for any API other than NEMO
will be rejected by PSyclone.
