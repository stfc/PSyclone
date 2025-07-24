.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2025, Science and Technology Facilities Council
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
.. Written by: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
..             A. B. G. Chalk and N. Nobre, STFC Daresbury Lab
..             I. Kavcic, Met Office
..             J. Dendy, Met Office
.. 
.. _sec_transformations_script:

PSyclone User Scripts
=====================

The standard way to transform a codebase using psyclone is through the
:ref:`psyclone_command` tool, which has an optional ``-s <SCRIPT_NAME>``
flag that allows users to specify a transformation user script to
programmatically modify the input code::

    > psyclone -s optimise.py input_source.f90

In this case, the current directory is prepended to the Python search path
``PYTHONPATH`` which will then be used to try to find the script file. Thus,
the search begins in the current directory and continues over any pre-existing
directories in the search path, failing if the file cannot be found.

Alternatively, script files may be specified with a path. In this case
the file must exist in the specified location. This location is then added to
the Python search path ``PYTHONPATH`` as before. For example::

    > psyclone -s ./optimise.py input_source.f90
    > psyclone -s ../scripts/optimise.py input_source.f90
    > psyclone -s /home/me/PSyclone/scripts/optimise.py input_source.f90

A valid PSyclone user script file must contain a ``trans`` function which accepts
a :ref:`PSyIR node<psyir-ug>` representing the root of the psy-layer
code (as a FileContainer):

.. code-block:: python

    def trans(psyir):
        # Modify psyir tree

The example below adds an OpenMP directive to a specific PSyKAL kernel:

.. code-block:: python

    def trans(psyir):
        from psyclone.transformations import OMPParallelLoopTrans
        from psyclone.psyir.node import Routine
        for subroutine in psyir.walk(Routine):
            if subroutine.name == 'invoke_0_v3_kernel_type':
                ol = OMPParallelLoopTrans()
                ol.apply(subroutine.children[0])


The script may apply as many transformations as is required for the intended
optimisation, and may also apply transformations to all the routines (i.e. invokes
and/or kernels) contained within the provided tree.
The :ref:`examples section<examples>` provides a list of psyclone user scripts
and associated usage instructions for multiple applications.


.. _sec_script_globals:

Script Global Variables
-----------------------

In addition to the ``trans`` function, there are special global variables that can be set
to control some of the behaviours of the front-end (before the optimisation function
is applied). These are:

.. code-block:: python

    # List of all files that psyclone will skip processing
    FILES_TO_SKIP = ["broken_file1.f90", "broken_file2.f90"]

    # Whether to chase the imported modules to improve symbol information (it can
    # also be a list of module filenames to limit the chasing to only specific
    # modules). This has to be used in combination with '-I' command flag in order
    # to point to the module location directory. We also strongly recommend using
    # the '--enable-cache' flag to reduce the performance overhead.
    RESOLVE_IMPORTS = ["relevant_module1.f90", "relevant_module2.f90"]

    def trans(psyir):
        # Modify psyir tree


PSyKAl algorithm code transformations
-------------------------------------

When using PSyKAl, the ``trans`` functions is used to transform the PSy-layer (the
layer in charge of the Parallel-System and Loops traversal orders), however, a
second optional transformation entry point ``trans_alg`` can be provided to
directly transform the Algorithm-layer (this is currently only implemented for
GOcean, but in the future it will also affect the LFRic DSL).

.. code-block:: python

   def trans_alg(psyir):
       # Modify algorithm psyir tree

As with the ``trans``` function it is up to the script what it does with
the algorithm PSyIR. Note that the ``trans_alg`` transformation is applied to
the algorithm layer before the PSy-layer is generated so any changes
applied to the algorithm layer will be reflected in the PSy-layer PSyIR tree
object that is passed to the ``trans`` function.

For example, if the ``trans_alg`` function in the script merged two
``invoke`` calls into one then the PSyIR node passed to the
``trans`` function of the script would only contain one Routine
associated with the merged invoke.

An example of the use of a script making use of the ``trans_alg``
function can be found in ``examples/gocean/eg7``.
