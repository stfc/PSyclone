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
.. Authors: R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab
.. Modified by I. Kavcic, Met Office

.. _getting-going:

Getting Going
=============

.. _getting-going-download:

Installation
------------

The following instructions are intended for a PSyclone user who wants
to work with a released version of the code. If you are a developer or
wish to test a specific branch of PSyclone from the GitHub repository
please see the
:ref:`Installation section in the Developer Guide <dev-installation>`.

.. tab-set::

  .. tab-item:: From PyPI:

    For a system-wide installation use:

    .. code-block:: bash

      pip install psyclone

    For a user-local installation use:

    .. code-block:: bash

      pip install --user psyclone

    For a specific release (where ``X.Y.Z`` is the release version) use:

    .. code-block:: bash

      pip install psyclone==X.Y.Z


    For more information about using ``pip`` or encapsulating the installation
    in its own ``virtual environment`` we recommend reading the
    `Python Packaging User Guide <https://packaging.python.org/en/latest/tutorials/installing-packages/>`_.

  .. tab-item:: From Conda:

    PSyclone is available in the ``conda-forge`` Conda channel.

    To create a conda environment containing PSyclone use:

    .. code-block:: bash

      conda create -n psyclone-env -c conda-forge psyclone

    For more information about how to use Conda we recommend reading the `Conda
    documentation <https://docs.conda.io/projects/conda/en/stable/>`_.

  .. tab-item:: From Spack:

    To install psyclone to your loaded Spack installation use:

    .. code-block:: bash

      spack install psyclone


    For more information about how to use Spack we recommend reading the
    `Spack documentation <https://spack-tutorial.readthedocs.io/>`_.

  .. tab-item:: From Source:

    To download and install a specific PSyclone release (where ``X.Y.Z`` is the release version)
    from source, use:

    .. code-block:: bash

       wget https://github.com/stfc/PSyclone/archive/X.Y.Z.tar.gz
       tar zxf X.Y.Z.tar.gz
       cd PSyclone-X.Y.Z
       pip install .


.. _getting-going-install-loc:

Installation location
^^^^^^^^^^^^^^^^^^^^^

The PSyclone installation location will vary depending on the specific installation
method and options used. The ``psyclone`` command will typically already be
prepended to your ``PATH`` after following the instructions above, but sometimes
you will need to source the virtual environment or load the Spack module again
after restarting your terminal.

Once psyclone is in your `PATH` you can execute ``which psyclone`` to see
the installation directory. Some supporting files such as configuration,
examples and instrumentation libraries are installed under the ``share/psyclone``
directory relative to the psyclone installation. You can replace
``bin/psyclone`` in the string returned by ``which psyclone`` with
``share/psyclone`` to find their location.


.. _getting-going-configuration:

Configuration
-------------

Various aspects of PSyclone are controlled through a configuration
file, ``psyclone.cfg``.  The default version of this file is located in
the ``share/psyclone/psyclone.cfg`` file in the :ref:`getting-going-install-loc`.

.. warning::

   If PSyclone is installed in 'editable' mode (``-e`` flag to ``pip``),
   or in a non-standard location, then PSyclone will not be able to find the
   default configuration file. There are two solutions to this:

   1. copy a configuration file to the location specified above.

   2. set the ``PSYCLONE_CONFIG`` environment variable (or the ``--config``
   flag) to the full-path to the configuration file, e.g.:

   .. code-block:: bash

     export PSYCLONE_CONFIG=/some/path/PSyclone/config/psyclone.cfg


See :ref:`configuration` for more details about the settings contained
within the config file.


.. _getting-going-run:

Running PSyclone
----------------

You are now ready to run PSyclone. One way of doing this is to use the ``psyclone``
command. To list the available options run: ``psyclone -h``, it should output::

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

There is more detailed information about each flag in :ref:`psyclone_command` section,
but the main parameters are the input source file that we aim to transform, and a transformation
recipe that is provided with the ``-s`` flag.
In addition to these, note that psyclone can be used in two distinct modes:
the code-transformation mode (when no ``-api``/``--psykal-dsl`` flags are provided) or the
PSyKAl DSL mode (when a ``-api``/``--psykal-dsl`` flag is provided). The following sections provide
a brief introduction to each mode.

PSyclone for Code Transformation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When using PSyclone for transforming existing Fortran files, only an
input source file is required:

.. code-block:: console

    psyclone input_file.f90


However, we usually want to redirect the output to a file so that we can later
compile it. We can do this using the `-o` flag:

.. code-block:: console

    psyclone input_file.f90 -o output.f90


This should not transform the semantics of the code (only the syntax), and is
what we sometimes refer to as a "passthrough" run. This can be useful as an initial
correctness test when applying PSyclone to a new code.

However, PSyclone allows users to programmatically change the source code of the
processed file. This is achieved using transformation recipes which are python scripts
with a `trans` function defined. For example:

.. code-block:: python

    def trans(psyir):
        ''' Add OpenMP Parallel Loop directives.

        :param psyir: the PSyIR of the provided file.
        :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

        '''
        omp_trans = TransInfo().get_trans_name('OMPParallelLoopTrans')

        for loop in psyir.walk(Loop):
            try:
                omp_trans.apply(loop)
            except TransformationError as err:
                print(f"Loop not paralellised because: {err.value}")


.. warning::

   Before PSyclone 3.0 the transformation scripts took a PSy object as argument:

   .. code-block:: python

       def trans(psy):
           ''' Add OpenMP Parallel Loop directives.

           :param psy: the PSy object that PSyclone has constructed for the
                       'invoke'(s) found in the Algorithm file.
           :type psy: :py:class:`psyclone.lfric.LFRicPSy`

           '''
           for invoke in psy.invokes.invoke_list:
              invoke.schedule

   This is deprecated and will stop working in PSyclone releases post version 3.0


And can be applied using the `-s` flag:

.. code-block:: console

    psyclone input_file.f90 -s trans_script.py -o output.f90


To see more complete examples of PSyclone for code transformation, see the
``examples/nemo`` folder in the PSyclone repository.

PSyclone for PSyKAl DSLs
^^^^^^^^^^^^^^^^^^^^^^^^

As indicated above, the ``psyclone`` command can also be used to process PSyKAl
DSLs (``--psykal-dsl`` flag). In this case the command takes as input the Fortran
source file containing the algorithm specification (in terms
of calls to ``invoke()``). It parses this, finds the necessary kernel
source files and produces two Fortran files. The first contains the
:ref:`middle, PSy-layer <PSy-layer>` and the second a re-write of the
:ref:`algorithm code <algorithm-layer>` to use that layer. These files
are named according to the user-supplied arguments (options ``-opsy``
and ``-oalg`` respectively). If those arguments are not supplied then the script
writes the re-written Fortran Algorithm layer to the terminal. For details of the other
command-line arguments please see the :ref:`psyclone_command` Section.

Examples are provided in the ``examples/lfric`` and ``examples/gocean`` directories
of the PSyclone repository. Alternatively, if you have installed PSyclone using
``pip`` then they may be found in the ``share/psyclone`` directory under your
PSyclone installation (see `which psyclone` for the location of the
PSyclone installation).
In this case you should copy the whole ``examples`` directory to some
convenient location before attempting to carry out the following instructions.

In this case we are going to use one of the LFRic examples:

.. code-block:: console

    cd <EGS_HOME>/examples/lfric/eg1
    psyclone --psykal-dsl lfric -d ../code -nodm -oalg alg.f90 \
        -opsy psy.f90 ./single_invoke.x90


You should see two new files created, called ``alg.f90`` (containing
the re-written algorithm layer) and ``psy.f90`` (containing the
generated PSy- or middle-layer). Since this is an LFRic example the
Fortran source code has dependencies on the LFRic system and
therefore cannot be compiled stand-alone.

The PSy-layer that PSyclone creates is constructed using the PSyclone Internal
Representation (:ref:`PSyIR <psyir-ug>`). Accessing this is demonstrated
by the ``print_psyir_trans.py`` script in the second LFRic example:

.. code-block:: console

    cd <EGS_HOME>/examples/lfric/eg2
    psyclone --psykal-dsl lfric -d ../code -s ./print_psyir_trans.py \
        -opsy psy.f90 -oalg alg.f90 ./multi_invoke_mod.x90

Take a look at the ``print_psyir_trans.py`` script for more information. *Hint*;
you can insert a single line in that script in order to break into the Python
interpreter during execution: ``import pdb; pdb.set_trace()``. This then enables
interactive exploration of the PSyIR if you are interested.

.. TODO #2627
  Alternatively, you can play with some interactive examples
  on `Binder <https://github.com/stfc/PSyclone#try-it-on-binder>`_.
