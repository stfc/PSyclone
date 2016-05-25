Generator script
================

The simplest way to run PSyclone is to use the generator.py
script. This script is located in the <PSYCLONEHOME>/src
directory. The script takes an algorithm file as input and outputs
modified algorithm code and generated PSy code. This section walks
through its functionality. The :ref:`api-label` section gives a more
concise overview.

Running
-------

The generator.py script is designed to be run from the command
line. It is typically invoked as an argument to the python
interpreter:
::

  > python <PSYCLONEHOME>/src/generator.py <args>

The optional -h argument gives a description of the options provided
by the script:
::

  > python <PSYCLONEHOME>/src/generator.py -h

  usage: generator.py [-h] [-oalg OALG] [-opsy OPSY] [-api API] [-s SCRIPT]
                      [-d DIRECTORY] [-l] [-dm] [-nodm]
                      filename

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

Basic Use
---------

The simplest way to use generator.py is to provide it with an
algorithm file.
::

    > python <PSYCLONEHOME>/src/generator.py alg.f90

If the algorithm file is invalid for some reason, the script should
return with an appropriate error. For example, if we use the Python
generator code itself as an algorithm file we get the following:
::

    > cd <PSYCLONEHOME>/src
    > python ./generator.py generator.py
    'Parse Error: Error, program, module or subroutine not found in ast'

.. warning::

    In the current version of PSyclone an unhelpful error ending with
    the following may occur
    ::

        AttributeError: 'Line' object has no attribute 'tofortran'

    This is due to the parser failing to parse the algorithm code and
    is very likely to be due to the algorithm code containing a syntax
    error.

If the algorithm file is valid then the modified algorithm code and
the generated PSy code will be output to the terminal screen.


Choosing the API
----------------

In the previous section we relied on PSyclone using the default
API. The default API, along with the supported API's can be seen by
running the generator.py script with the -h option.

If you use a particular API frequently and it is not the default then
you can change the default by editing the config.py file in the
<PSYCLONEHOME>/src directory.

If your code uses an API that is different to the default then you can
specify this as an argument to the generator.py script.
::

    > python <PSYCLONEHOME>/src/generator.py -api dynamo0.1 alg.f90

File output
-----------

By default the modified algorithm code and the generated PSy code are
output to the terminal. These can be output to a file by using the
-oalg <file> and -opsy <file> options respectively. For example, the
following will output the generated psy code to the file 'psy.f90' but
the algorithm code will be output to the terminal:
::

    > python <PSYCLONEHOME>/src/generator.py -opsy psy.f90 alg.f90

Algorithm files with no invokes
-------------------------------

If the generator script is provided with a file that contains no
``invoke`` calls then the script outputs a warning to ``stdout`` and
copies the input file to ``stdout``, or to the specified algorithm
file (if the -oalg <file> option is used). No PSy code will be
output. If a file is specified using the -opsy <file> option this file
will not be created.
::

    > python <PSYCLONEHOME>/src/generator.py -opsy psy.f90 -oalg alg_new.f90 empty_alg.f90
    Warning: 'Algorithm Error: Algorithm file contains no invoke() calls: refusing to
    generate empty PSy code'

Kernel directory
----------------

When an algorithm file is parsed, the parser looks for the associated
kernel files. The way this is done requires that any kernel routine
specified in an invoke must have an explicit use statement. For
example, the following code gives an error:
::

    > cat no_use.f90
    program no_use
      call invoke(testkern_type(a,b,c,d))
    end program no_use
    > python <PSYCLONEHOME>/src/generator.py no_use.f90
    "Parse Error: kernel call 'testkern_type' must be named in a use statement"

If the name of the kernel is provided in a use statement then the
parser will look for a file with the same name as the module in the
use statement. In the example below, the parser will look for a file
called "testkern.f90" or "testkern.F90":
::

    > cat use.f90
    program use
      use testkern, only : testkern_type
      call invoke(testkern_type(a,b,c,d))
    end program use

Therefore, for PSyclone to find Kernel files, the module name of a
kernel file must be the same as its filename.  By default the parser
looks for the kernel file in the same directory as the algorithm
file. If this file is not found then an error is reported.
::

    > python <PSYCLONEHOME>/src/generator.py use.f90 
    Kernel file 'testkern.[fF]90' not found in <location>

The -d option can be used to tell the generator.py script where to
look for Kernel files. The -d option tells the generator.py script
that the required Kernel code is somewhere within the specified
directory hierarchy. The script will recurse from the specified
directory path to look for the required file. There must be only one
instance of the specified file within the specified directory:
::

    > cd <PSYCLONEHOME>/src
    > python ./generator.py -d . use.f90 
    More than one match for kernel file 'testkern.[fF]90' found!
    > python ./generator.py -d tests/test_files/dynamo0p3 -api dynamo0.3 use.f90 
    [code output]

.. note::
    The -d option is limited to a single directory. Therefore a
    current limitation in PSyclone is that all required Kernel files
    required by an algorithm file must exist within a directory
    hierarchy where their file names are unique.

Transformation script
---------------------

By default the generator.py script will generate 'vanilla' PSy layer
code. The -s option allows a python script to be specified which can
transform the PSy layer. This option is discussed in more detail in
the :ref:`sec_transformations_script` section.

.. _fort_line_length:

Fortran line length
-------------------

By default the generator.py script will generate fortran code with no
consideration of fortran line length limits. As the line length limit
for free-format fortran is 132 characters, the code that is output may
be non-conformant.

Line length is not an issue for many compilers as they
allow compiler flags to be set which allow lines longer than the
fortran standard. However this is not the case for all compilers.

When the -l option is specified in the generator.py script, the output
will be line wrapped so that the output line lengths are always within
the 132 character limit.

The -l option also checks the parsed algorithm and kernel files for
conformance and raises an error if they do not conform.

Line wrapping is not performed by default. There are two reasons for
this. This first reason is that most compilers are able to cope with
long lines. The second reason is that the line wrapping implementation
could fail in certain pathological cases. The implementation and
limitations of line wrapping are discussed in the
:ref:`line-length-limitations` section.

Distributed memory
------------------

By default the generator.py script will generate distributed
memory (DM) code (i.e. parallelised using MPI). As with the choice of
API, this default may be configured by editing
<PSYCLONEHOME>/src/config.py.  Alternatively, whether or not to
generate DM code can be specified as an argument to the generator.py
script using the ``-dm``/``--dist_mem`` or ``-nodm``/``--no_dist_mem``
flags, respectively.

For details of PSyclone's support for generating DM code see
:ref:`distributed_memory`.
