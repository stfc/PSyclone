.. _psyclone_script:

psyclone script
===============

The simplest way to run PSyclone is to use the ``psyclone`` script. If
you installed PSyclone using pip then this script should be available
on your PATH (see :ref:`getting_going_env` for more
details). Alternatively it can be found in the ``<PSYCLONEHOME>/bin``
directory. The script takes an algorithm file as input and outputs
modified algorithm code and generated PSy code. This section walks
through its functionality. The :ref:`api-label` section gives a more
concise overview.

Running
-------

The ``psyclone`` script is executable and designed to be run from the command
line, e.g.:
::

  > psyclone <args>

The optional ``-h`` argument gives a description of the options provided
by the script:

.. code-block:: bash
		
  > psyclone -h

  usage: psyclone [-h] [-oalg OALG] [-opsy OPSY] [-api API] [-s SCRIPT]
                  [-d DIRECTORY] [-l] [-dm] [-nodm]
		  [--profile {invokes,kernels}]
		  [--force-profile {invokes,kernels}] [-v] filename

  Run the PSyclone code generator on a particular file

  positional arguments:
    filename              algorithm-layer source code

  optional arguments:
    -h, --help            show this help message and exit
    -oalg OALG            filename of transformed algorithm code
    -opsy OPSY            filename of generated PSy code
    -api API              choose a particular api from ['gunghoproto',
                          'dynamo0.1', 'dynamo0.3', 'gocean0.1', 'gocean1.0'],
                          default dynamo0.3
    -s SCRIPT, --script SCRIPT
                          filename of a PSyclone optimisation script
    -d DIRECTORY, --directory DIRECTORY
                          path to root of directory structure containing kernel
                          source code
    -l, --limit           limit the fortran line length to 132 characters
    -dm, --dist_mem       generate distributed memory code
    -nodm, --no_dist_mem  do not generate distributed memory code
    --profile {invokes,kernels}, -p {invokes,kernels}
                        Add profiling hooks for either 'kernels' or 'invokes'
    --force-profile {invokes,kernels}
                        Add profiling hooks for either 'kernels' or 'invokes'
                        even if a transformation script is used. Use at your
                        own risk.
    -v, --version         Display version information (1.6.0)

Basic Use
---------

The simplest way to use ``psyclone`` is to provide it with an
algorithm file::

    > psyclone alg.f90

If the algorithm file is invalid for some reason, the script should
return with an appropriate error. For example, if we use the Python
``genkernelstub`` script as an algorithm file we get the following::

    > cd <PSYCLONEHOME>/bin
    > psyclone genkernelstub
    ...
    'Parse Error: Fatal error in external fparser tool'

If the algorithm file is valid then the modified algorithm code and
the generated PSy code will be output to the terminal screen.


Choosing the API
----------------

In the previous section we relied on PSyclone using the default
API. The default API, along with the supported API's can be seen by
running the ``psyclone`` script with the ``-h`` option.

If you use a particular API frequently and it is not the default then
you can change the default by creating a copy of the default
``psyclone.cfg`` file and editing it. See :ref:`configuration` for
more details.

If your code uses an API that is different to the default then you can
specify this as an argument to the ``psyclone`` script.
::

    > psyclone -api dynamo0.1 alg.f90

File output
-----------

By default the modified algorithm code and the generated PSy code are
output to the terminal. These can instead be output to files by using the
``-oalg <file>`` and ``-opsy <file>`` options, respectively. For example, the
following will output the generated psy code to the file 'psy.f90' but
the algorithm code will be output to the terminal:
::

    > psyclone -opsy psy.f90 alg.f90

Algorithm files with no invokes
-------------------------------

If the ``psyclone`` script is provided with a file that contains no
``invoke`` calls then the script outputs a warning to ``stdout`` and
copies the input file to ``stdout``, or to the specified algorithm
file (if the ``-oalg <file>`` option is used). No PSy code will be
output. If a file is specified using the ``-opsy <file>`` option this file
will not be created.

.. code-block:: bash

    > psyclone -opsy psy.f90 -oalg alg_new.f90 empty_alg.f90
    Warning: 'Algorithm Error: Algorithm file contains no invoke() calls: refusing to
    generate empty PSy code'

Kernel directory
----------------

When an algorithm file is parsed, the parser looks for the associated
kernel files. The way in which this is done requires that any kernel routine
called within an invoke must have an explicit use statement. For
example, the following code gives an error:

.. code-block:: bash

    > cat no_use.f90
    program no_use
      call invoke(testkern_type(a,b,c,d))
    end program no_use
    > psyclone no_use.f90
    "Parse Error: kernel call 'testkern_type' must be named in a use statement"

If the name of the kernel is provided in a use statement then the
parser will look for a file with the same name as the module in the
use statement. In the example below, the parser will look for a file
called "testkern.f90" or "testkern.F90":

.. code-block:: bash

    > cat use.f90
    program use
      use testkern, only : testkern_type
      call invoke(testkern_type(a,b,c,d))
    end program use

Therefore, for PSyclone to find Kernel files, the module name of a
kernel file must be the same as its filename.  By default the parser
looks for the kernel file in the same directory as the algorithm
file. If this file is not found then an error is reported.

.. code-block:: bash

    > psyclone use.f90 
    Kernel file 'testkern.[fF]90' not found in <location>

The ``-d`` option can be used to tell ``psyclone`` where to look for
Kernel files by supplying it with a directory. The script will recurse
from the specified directory path to look for the required file. There
must be only one instance of the specified file within (or below) the
specified directory:

.. code-block:: bash
		  
    > cd <PSYCLONEHOME>/psyclone/src
    > psyclone -d . use.f90 
    More than one match for kernel file 'testkern.[fF]90' found!
    > psyclone -d tests/test_files/dynamo0p3 -api dynamo0.3 use.f90 
    [code output]

.. note::
    The ``-d`` option is limited to a single directory. Therefore a
    current limitation in PSyclone is that all Kernel files
    required by an algorithm file must exist within a directory
    hierarchy where their file names are unique.

Transformation script
---------------------

By default the ``psyclone`` script will generate 'vanilla' PSy layer
code. The -s option allows a Python script to be specified which can
transform the PSy layer. This option is discussed in more detail in
the :ref:`sec_transformations_script` section.

.. _fort_line_length:

Fortran line length
-------------------

By default the ``psyclone`` script will generate fortran code with no
consideration of Fortran line-length limits. As the line-length limit
for free-format Fortran is 132 characters, the code that is output may
be non-conformant.

Line length is not an issue for many compilers as they
allow compiler flags to be set which allow lines longer than the
Fortran standard. However this is not the case for all compilers.

When the ``-l`` option is specified to the ``psyclone`` script, the output
will be line wrapped so that the output lines are always within
the 132 character limit.

The ``-l`` option also checks the parsed algorithm and kernel files for
conformance and raises an error if they do not conform.

Line wrapping is not performed by default. There are two reasons for
this. This first reason is that most compilers are able to cope with
long lines. The second reason is that the line wrapping implementation
could fail in certain pathological cases. The implementation and
limitations of line wrapping are discussed in the
:ref:`line-length-limitations` section.

Distributed memory
------------------

By default the ``psyclone`` script will generate distributed
memory (DM) code (i.e. parallelised using MPI). As with the choice of
API, this default may be configured by editing ``psyclone.cfg`` - see
:ref:`configuration`.  Alternatively, whether or not to generate DM
code can be specified as an argument to the ``psyclone`` script using
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
