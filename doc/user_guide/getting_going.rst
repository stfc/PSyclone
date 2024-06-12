.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
.. Authors: R. W. Ford, A. R. Porter, N. Nobre and S. Siso, STFC Daresbury Lab
.. Modified by I. Kavcic, Met Office

.. _getting-going:

Getting Going
=============

.. _getting-going-install:

Installing PSyclone
-------------------

The following instructions are intended for PSyclone users who want to work
with a released version of PSyclone. If you are a developer or wish to test
a specific branch from the GitHub repository please see the
:ref:`dev_guide:dev-installation` section in the
`Developer Guide <https://psyclone-dev.readthedocs.io/>`_.

We recommend installing the latest release of psyclone from
`PyPI <https://pypi.org/project/PSyclone/>`_ by using:

.. code-block:: console

    pip install psyclone
   
You can also install a previous release from PyPI with:

.. code-block:: console

   pip install psyclone==X.Y.Z

where ``X.Y.Z`` is the specific PSyclone release version (e.g. |release|).

Or if you want an isolated installation in a `python virtual
environment <https://docs.python.org/3/library/venv.html>`_, you can do:

.. code-block:: console

    python -m venv <virtual_env_name>
    source <virtual_env_name>/bin/activate
    pip install psyclone

.. _getting-going-configuration:

Configuration
-------------

Various aspects of PSyclone are configured through a configuration file,
``psyclone.cfg``. The default version of this file is located in the installation
directory ``<prefix>/shared/psyclone/``. See :ref:`configuration` for details of
the settings contained within the config file.

.. warning::

   If PSyclone is installed to a non-standard location the configuration file
   will not be found when executing PSyclone. There are two solutions to this:
   1. copy the configuration file to a location where PSyclone will find it (see
   :ref:`configuration`) or 2. set the ``PSYCLONE_CONFIG`` environment variable
   to the full-path to the configuration file, e.g.:

   .. code-block:: console

       export PSYCLONE_CONFIG=/some/path/PSyclone/config/psyclone.cfg


.. _getting-going-run:

Running PSyclone
----------------

You are now ready to run PSyclone. One way of doing this is to use the ``psyclone``
command. To list the available options run: `psyclone -h`, it should output::

   usage: psyclone [-h] [--version] [--config CONFIG] [-s SCRIPT] [-I INCLUDE]
                   [-l {off,all,output}] [--profile {invokes,routines,kernels}]
				   [--backend {enable-validation,disable-validation}] [-o OUTPUT_FILE]
				   [-api DSL] [-oalg OUTPUT_ALGORITHM_FILE] [-opsy OUTPUT_PSY_FILE]
                   [-okern OUTPUT_KERNEL_PATH] [-d DIRECTORY] [-dm] [-nodm]
                   [--kernel-renaming {multiple,single}]
                   filename

   Transform a file using the PSyclone source-to-source Fortran compiler

   positional arguments:
     filename              input source code

   options:
     -h, --help            show this help message and exit
     --version, -v         display version information
     --config CONFIG, -c CONFIG
                           config file with PSyclone specific options.
     -s SCRIPT, --script SCRIPT
                           filename of a PSyclone optimisation recipe
     -I INCLUDE, --include INCLUDE
                           path to Fortran INCLUDE or module files
     -l {off,all,output}, --limit {off,all,output}
                           limit the Fortran line length to 132 characters (default 'off').
                           Use 'all' to apply limit to both input and output Fortran. Use
                           'output' to apply line-length limit to output Fortran only.
     --profile {invokes,routines,kernels}, -p {invokes,routines,kernels}
                           add profiling hooks for 'kernels', 'invokes' or 'routines'.
     --backend {enable-validation,disable-validation}
                           options to control the PSyIR backend used for code generation.
                           Use 'disable-validation' to disable the validation checks that
                           are performed by default.
     -o OUTPUT_FILE        (code-transformation mode) output file
     -api DSL, -psykal-dsl DSL
                           whether to use particular PSyKAl DSL API from ['lfric', 'gocean'].
     -oalg OUTPUT_ALGORITHM_FILE
                           (psykal mode) filename of transformed algorithm code
     -opsy OUTPUT_PSY_FILE
                           (psykal mode) filename of generated PSy-layer code
     -okern OUTPUT_KERNEL_PATH
                           (psykal mode) directory in which to put transformed kernels, default
                           is the current working directory.
     -d DIRECTORY, --directory DIRECTORY
                           (psykal mode) path to a root directory structure containing kernel
                           source code. Multiple roots can be specified by using multiple -d
                           arguments.
     -dm, --dist_mem       (psykal mode) generate distributed memory code
     -nodm, --no_dist_mem  (psykal mode) do not generate distributed memory code
     --kernel-renaming {multiple,single}
                           (psykal mode) naming scheme to use when re-naming transformed kernels

There is more detailed information about each flag in the :ref:`psyclone_command`, but the main
parameters are the input source file that we aim to transform, and a transformation recipe that
is provided with the `-s` flag.
In addition to these, note that ``psyclone`` can be used in two disctinct modes:
the code-transformation mode (when no `-api` or `-psykal-dsl` flags are provided) or the
PSyKAl DSL mode (when `-api` or `-psykal-dsl` are provided). The following sections provide
a brief introduction to each mode.

PSyclone for code-transformations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



PSyclone for PSyKAl DSLs
^^^^^^^^^^^^^^^^^^^^^^^^

In this case the command takes as input the Fortran source
file containing the algorithm specification (in terms
of calls to ``invoke()``). It parses this, finds the necessary kernel
source files and produces two Fortran files. The first contains the
:ref:`PSy, middle layer <PSy-layer>` and the second a re-write of the
:ref:`algorithm code <algorithm-layer>` to use that layer. These files
are named according to the user-supplied arguments (options ``-oalg``
and ``-opsy``). If those arguments are not supplied then the script writes
the generated/re-written Fortran to the terminal. For details of the other
command-line arguments please see the :ref:`psyclone_command` Section.

Examples are provided in the ``examples`` directory of the PSyclone Git
repository - if you have cloned the repository then ``EGS_HOME`` in
what follows is the root ``PSyclone`` directory. Alternatively, if you
have installed PSyclone using ``pip`` then they may be found in the
``share/psyclone`` directory under your Python installation (see
:ref:`above <getting-going-env-loc>` for location of PSyclone installation.
In this case you should copy the whole ``examples`` directory to some
convenient location (hereafter called ``EGS_HOME``) before attempting to
carry out the following instructions. Depending on your precise setup, you
may also need to set ``PSYCLONE_CONFIG`` to the full-path to the PSyclone
configuration file (see :ref:`getting-going-configuration`).

There are seven subdirectories, three of which (``lfric``, ``gocean``
and ``nemo``) correspond to the different APIs/domains that are
supported by PSyclone. (Note, that we are currently in the process of
renaming the ``lfric`` API to ``lfric``.)  In this case we are
going to use one of the LFRic examples::

   > cd <EGS_HOME>/examples/lfric/eg1
   > psyclone -api lfric -d ../code -nodm -oalg alg.f90 \
       -opsy psy.f90 ./single_invoke.x90


You should see two new files created, called ``alg.f90`` (containing
the re-written algorithm layer) and ``psy.f90`` (containing the
generated PSy- or middle-layer). Since this is an LFRic example the
Fortran source code has dependencies on the LFRic system and
therefore cannot be compiled stand-alone.

The PSy-layer that PSyclone creates is constructed using the PSyclone Internal
Representation (:ref:`PSyIR <psyir-ug>`). Accessing this is demonstrated
by the ``print_psyir_trans.py`` script in the second LFRic example::

  > cd <EGS_HOME>/examples/lfric/eg2
  > psyclone -api lfric -d ../code -s ./print_psyir_trans.py \
      -opsy psy.f90 -oalg alg.f90 ./multi_invoke_mod.x90

Take a look at the ``print_psyir_trans.py`` script for more information. *Hint*;
you can insert a single line in that script in order to break into the Python
interpreter during execution: ``import pdb; pdb.set_trace()``. This then enables
interactive exploration of the PSyIR if you are interested. Alternatively,
you can play with some interactive examples on `Binder <https://github.com/stfc/PSyclone#try-it-on-binder>`_.
