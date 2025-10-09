.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2025, Science and Technology Facilities Council.
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

The ``psyclone`` command
========================

The ``psyclone`` command is an executable script designed to be run from the
command line, e.g.:
::

  > psyclone <args>

The optional ``-h`` argument gives a description of the options provided
by the command:

.. parsed-literal::

  > psyclone -h
    usage: psyclone [-h] [-v] [-c CONFIG] [-s SCRIPT] [--enable-cache] [-l {off,all,output}]
                    [-p {invokes,routines,kernels}]
                    [--backend {disable-validation,disable-indentation}] [-o OUTPUT_FILE]
                    [-api DSL] [-oalg OUTPUT_ALGORITHM_FILE] [-opsy OUTPUT_PSY_FILE]
                    [-okern OUTPUT_KERNEL_PATH] [-dm] [-nodm]
                    [--kernel-renaming {multiple,single}]
                    [--log-level {OFF,DEBUG,INFO,WARNING,ERROR,CRITICAL}] [--log-file LOG_FILE]
                    [--keep-comments] [--keep-directives] [-I INCLUDE] [-d DIRECTORY]
                    [--modman-file-ignore IGNORE_PATTERN] [--free-form | --fixed-form]
                    filename

    Transform a file using the PSyclone source-to-source Fortran compiler

    positional arguments:
      filename              input source code

    options:
      -h, --help            show this help message and exit
      -v, --version         display version information
      -c CONFIG, --config CONFIG
                            config file with PSyclone specific options
      -s SCRIPT, --script SCRIPT
                            filename of a PSyclone optimisation recipe
      --enable-cache        whether to enable caching of imported module dependencies (if
                            enabled, it will generate a .psycache file of each imported module in
                            the same location as the imported source file).
      -l {off,all,output}, --limit {off,all,output}
                            limit the Fortran line length to 132 characters (default 'off'). Use
                            'all' to apply limit to both input and output Fortran. Use 'output'
                            to apply line-length limit to output Fortran only.
      -p {invokes,routines,kernels}, --profile {invokes,routines,kernels}
                            add profiling hooks for 'kernels', 'invokes' or 'routines'
      --backend {disable-validation,disable-indentation}
                            options to control the PSyIR backend used for code generation. Use
                            'disable-validation' to disable the validation checks that are
                            performed by default. Use 'disable-indentation' to turn off all
                            indentation in the generated code.
      -o OUTPUT_FILE        (code-transformation mode) output file
      -api DSL, --psykal-dsl DSL
                            whether to use a PSyKAl DSL (one of ['lfric', 'gocean'])
      -oalg OUTPUT_ALGORITHM_FILE
                            (psykal mode) filename of transformed algorithm code
      -opsy OUTPUT_PSY_FILE
                            (psykal mode) filename of generated PSy-layer code
      -okern OUTPUT_KERNEL_PATH
                            (psykal mode) directory in which to put transformed kernels, default
                            is the current working directory
      -dm, --dist_mem       (psykal mode) generate distributed memory code
      -nodm, --no_dist_mem  (psykal mode) do not generate distributed memory code
      --kernel-renaming {multiple,single}
                            (psykal mode) naming scheme to use when re-naming transformed kernels
      --log-level {OFF,DEBUG,INFO,WARNING,ERROR,CRITICAL}
                            sets the level of the logging (defaults to OFF).
      --log-file LOG_FILE   sets the output file to use for logging (defaults to stderr).
      --keep-comments       keeps comments from the original code (defaults to False). Directives
                            are not kept with this option (use --keep-directives).
      --keep-directives     keeps directives from the original code (defaults to False).
      --free-form           forces PSyclone to parse this file as free format (default is to look
                            at the input file extension).
      --fixed-form          forces PSyclone to parse this file as fixed format (default is to
                            look at the input file extension).

    Directory management:
      -I INCLUDE, --include INCLUDE
                            path to Fortran INCLUDE or module files
      -d DIRECTORY, --directory DIRECTORY
                            (psykal mode) path to a root directory structure containing kernel
                            source code. Multiple roots can be specified by using multiple -d
                            arguments. These directories will be searchedrecursively.
      --modman-file-ignore IGNORE_PATTERN
                            Ignore files that contain the specified pattern.

Basic Use
---------

The simplest way to use ``psyclone`` is to provide a Fortran input source file:

.. code-block:: console

    psyclone input.f90

If the input file is valid Fortran, PSyclone will print the output Fortran
(in this case the same unmodified code but with normalised syntax) to stdout.
Otherwise it will print the errors detected while parsing the Fortran file.

Usually we want to redirect the output to a file that we can later
compile. We can do this with the `-o` flag:

.. code-block:: console

    psyclone input.f90 -o output.f90


Transformation script
---------------------

By default, the ``psyclone`` command will not apply any transformation (other
than canonicalising the code and generating a normalised syntax). To apply
transformations to the code, a recipe needs to be specified with the `-s` flag.
This option is discussed in more detail in the :ref:`sec_transformations_script`
section. With a transformation recipe the command looks like:

.. code-block:: console

    psyclone input.f90 -s transformation_recipe.py


Fortran INCLUDE Files and Modules
---------------------------------

If the source code to be processed by PSyclone contains INCLUDE statements
then the location of any INCLUDE'd files *must* be supplied to PSyclone via
the ``-I`` or ``--include`` option. (This is necessary because INCLUDE lines
are a part of the Fortran language and must therefore be parsed - they are not
handled by any pre-processing step.) Multiple locations may be specified by
using multiple ``-I`` flags, e.g.:

.. code-block:: console

    psyclone -I /some/path -I /some/other/path input.f90


If no include paths are specified then the directory containing the
source file currently being parsed is searched by default. If the
specified INCLUDE file is not found then PSyclone will abort with
an appropriate error. For example:

.. code-block:: console

    psyclone -I nonexisting test.f90
    PSyclone configuration error: Include path 'nonexisting' does not exist

The `-I` locations will also be used when a script requests to follow module
dependencies in order to obtain more information about the code symbols (see
:ref:`sec_script_globals`). But note that if the whole program has many
dependencies and the imports happen from multiple files, it can increase the
psyclone processing time considerably. In this case it is recommended to use
the `--enable-cache` flag. This will creates a `filename.psycache` file in the
same location as the original file for every import followed. The next time
the same import is requested, if the hashes match, the cached file will be used.

Currently, the PSyKAl-based APIs (LFRic and GOcean - see below) will ignore
(but preserve) INCLUDE statements in algorithm-layer code. However, INCLUDE
statements in kernels will, in general, cause the kernel parsing to fail
unless the file(s) referenced in such statements are in the same directory
as the kernel file. Once kernel parsing has been re-implemented to use
fparser2 (issue #239) and the PSyclone Intermediate Representation then the
behaviour will be the same as for generic code-transformations.

Since PSyclone does not attempt to be a full compiler, it does not require
that the code be available for any Fortran modules referred to by ``use``
statements. However, certain transformations *do* require that e.g. type
information be determined for all variables in the code being transformed.
In this case PSyclone *will* need to be able to find and process any
referenced modules. To do this it searches in the directories specified
by the ``-I``/``--include`` flags.

C Pre-processor #include Files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PSyclone currently only supports Fortran input. As such, if a file to
be processed contains CPP ``#include`` statements then it must first be
processed by a suitable pre-processor before being passed to PSyclone.
PSyclone will abort with an appropriate error if it encounters a
``#include`` in any code being processed (whether or not a PSykAL DSL is
in use).

.. _fort_line_length:

Fortran line length
-------------------

By default the ``psyclone`` command will generate Fortran code with no
consideration of Fortran line-length limits. As the line-length limit
for free-format Fortran is 132 characters, the code that is output may
be non-conformant.

Line length is not an issue for many compilers as they provide flags to
increase or disable Fortran standard line lengths limits. However this is
not the case for all compilers.

When either the ``-l all`` or ``-l output`` option is specified to
the ``psyclone`` command, the output will be line wrapped so that the
output lines are always within the 132 character limit.

The ``-l all`` additionally checks the input Fortran files for conformance
and raises an error if they do not conform.

Line wrapping is not performed by default. There are two reasons for
this. This first reason is that most compilers are able to cope with
long lines. The second reason is that the line wrapping implementation
could fail in certain pathological cases.

For very deeply-nested code structures, it can be that the amount of
indentation (white space) alone exceeds the 132-character limit. The
line-length limiter will simply remove all indentation on any such lines.

Finally, if all else fails, the code-generation part of PSyclone (the
"backend") can be instructed not to use any indentation at all. See the
:ref:`backend-options` section.


.. _fortran_source_format:

Fortran Format Option
---------------------

PSyclone supports both free and fixed format source input. By default,
PSyclone follows the gfortran specification for file extensions:

    - Free format extensions: .f90, .f95, .f03, .f08, .F90, .F95, .F03, .F08
    - Fixed format extensions: .f, .for, .fpp, .ftn, .F, .FOR, .FPP, .FTN

PSyclone also recognises the following extensions as free format to support
current uses cases: .x90, .xu90

PSyclone also provides the ``--free-form`` and ``-fixed-form`` to override the
default behaviour, and will use the specified option over the file extension.

If the file extension is not one of the ones listed above, and neither of the
``--free-form`` or ``--fixed-form`` flags is used then PSyclone defaults to
assuming the input source is free form Fortran.


.. _backend-options:

Backend Options
---------------

The final code generated by PSyclone is created by passing the PSyIR
tree to one of the 'backends' (see :ref:`psyir-backends` in
the Developer Guide for more details). The ``--backend`` flag permits
a user to tune the behaviour of this code generation in the ways
described below.

Validation Checks
^^^^^^^^^^^^^^^^^

The option ``disable-validation`` turns off the
validation checks performed when doing code generation. By default,
such validation is enabled as it is only at code-generation time that
certain constraints can be checked (since PSyclone does not mandate
the order in which code transformations are applied).  Occasionally,
these validation checks may raise false positives (due to incomplete
implementations), at which point it is useful to be able to disable
them.  The default behaviour may be changed by adding the
``BACKEND_CHECKS_ENABLED`` entry to the
:ref:`configuration file <config-default-section>`. Any
command-line setting always takes precedence though. It is
recommended that validation only be disabled as a last resort and for
as few input source files as possible.

Code Indentation
^^^^^^^^^^^^^^^^

The ``--backend disable-indentation`` command-line
flag disables all indentation in the code generated by the backend.
By default, indentation is used to make the generated code human
readable. However, in certain compilers, this can interact
with the line-length limiter to produce code that cannot be compiled.
Disabling all indentation can solve this problem.
The default behaviour may be changed by adding the 
``BACKEND_INDENTATION_DISABLED`` entry in the PSyclone
:ref:`configuration file <config-default-section>`. Note that any
command-line setting always takes precedence.

Automatic Profiling Instrumentation
-----------------------------------

The ``--profile`` option allows the user to instruct PSyclone to automatically
insert profiling calls in addition to the code transformations specified in
the recipe.  This flag accepts the options: ``routines``, ``invokes`` and
``kernels``. PSyclone will insert profiling-start and -stop calls at the
beginning and end of each routine, PSy-layer invoke or PSy-layer kernel call,
respectively. The generated code must be linked against the PSyclone profiling
interface and the profiling tool itself. The application that calls the
PSyclone-generated code is responsible for initialising and finalising the
profiling library that is being used (if necessary). For more details on the use
of this profiling functionality please see the :ref:`profiling` section.


Using PSyclone for PSyKAL DSLs
------------------------------

In addition to the default code-transformation mode, ``psyclone`` can also
be used to process Fortran files that implement PSyKAL DSLs (see
:ref:`introduction_to_psykal`). To do this you can choose a DSL API
with the ``-api`` or ``--psykal-dsl`` flag.

The main difference is that, instead of providing a single file to process, for
PSyKAl DSLs PSyclone expects an algorithm-layer file that describes the high-level
view of an algorithm. PSyclone will use this algorithm file and the metadata of the
kernels that it calls to generate a PSy(Parallel System)-layer code that connects
the Algorithm layer to the Kernels. In this mode of operation, any supplied
transformation recipe is applied to the PSy-layer.

By default, the ``psyclone`` command for PSyKAl APIs will generate distributed
memory (DM) code (unless otherwise specified in the :ref:`configuration` file).
Alternatively, whether or not to generate DM code can be specified as an
argument to the ``psyclone`` command using the ``-dm``/``--dist_mem`` or
``-nodm``/``--no_dist_mem`` flags, respectively.
For exampe the following command will generate GOcean PSyKAl code with DM:

.. code-block:: console

    psyclone -api gocean -dm algorithm.f90


See :ref:`psyclone usage for PSyKAl <psykal_usage>` section for more information
about how to use PSyKAl DSLs.

PSyKAl file output
^^^^^^^^^^^^^^^^^^

By default the modified algorithm code and the generated PSy code are
output to the terminal. These can instead be output to files by using the
``-oalg <file>`` and ``-opsy <file>`` options, respectively. For example, the
following will output the generated PSy code to the file 'psy.f90' but
the algorithm code will be output to the terminal:

.. code-block:: console

    psyclone -opsy psy.f90 algorithm.f90

If PSyclone is being used to transform Kernels then the location to
write these to is specified using the ``-okern <directory>``
option. If this is not supplied then they are written to the current
working directory. By default, PSyclone will overwrite any kernel of
the same name in that directory. To change this behaviour, the user
can use the ``--no_kernel_clobber`` option. This causes PSyclone to
re-name any transformed kernel that would clash with any of those
already present in the output directory.

Algorithm files with no invokes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^^^

When an algorithm file is parsed, the parser looks for the associated
kernel files. The way in which this is done requires that any
user-defined kernel routine (as opposed to :ref:`psykal-built-ins`) called
within an invoke must have an explicit use statement. For example, the
following code gives an error:

.. code-block:: bash

    > cat no_use.f90
    program no_use
      call invoke(testkern_type(a,b,c,d,e))
    end program no_use
    > psyclone -api gocean no_use.f90
    "Parse Error: kernel call 'testkern_type' must either be named in a use statement or be a recognised built-in (one of '[]' for this API)"

(If the chosen API has any :ref:`psykal-built-ins` defined then
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
    > psyclone -d tests/test_files/lfric -api lfric use.f90 
    [code output]

.. note:: The ``-d`` option can be repeated to add as many search
    directories as is required, with the constraint that there must be
    only one instance of the specified file within (or below) the
    specified directories.

Transforming PSyKAl Kernels
^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

Enabling the Logging Infrastructure
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PSyclone supports logging which can provide additional information
on what is happening inside PSyclone. This logging will also
control the behaviour of any logging calls inside a user script.

Logging output can be controlled through the ``--log-level`` option.
By default, logging is set to ``OFF``, which means
no logging output will be produced. There are 5 other levels as
detailed in the ``psyclone -h`` information.

By default the output from the logging goes into stderr.
To control the logging output, PSyclone provides the
``--log-file`` option. If this is set, the logging output will instead
be directed to the provided file.

Keeping Comments and Directives
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PSyclone can now keep comments and directives from the original code, with
some limitations:

  1. Comments that appear after all statements in a routine are not currently
     kept.
  2. Directives are kept as ``CodeBlock`` nodes in the PSyIR which means
     some transformations will be unavailable on regions containing these
     nodes. Also PSyclone will not know any details about these nodes
     (including that they contain directives) but this functionality will
     be improved over time.

Note that using the ``keep-comments`` option alone means that any comments
that PSyclone interprets as directives will be lost from the input.
