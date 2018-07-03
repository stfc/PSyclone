.. _api-label:

API
===

.. program:: psyclone

psyclone

    .. cmdoption:: -h
    .. cmdoption:: -oalg <filename>
    .. cmdoption:: -opsy <filename>
    .. cmdoption:: -api <api>
    .. cmdoption:: -s <script>
    .. cmdoption:: -d <directory>
    .. cmdoption:: -l
    .. cmdoption:: -dm/-nodm
    .. cmdoption:: --profile/--force-profile <invokes,kernels>

Command line version of the generator. -h prints out the command
line options. If -oalg or -opsy are not provided then the
generated code is printed to stdout, otherwise they are output to
the specified file name. -api specifies the particular api to
use. -s allows a script to be called which can modify (typically
optimise) the PSy layer. -d specifies a directory to recursively
search to find the associated kernel files. -l limits the maximum
line length of the fortran output to 132 characters. -l uses a
relatively simple algorithm which in pathological cases may
produce incorrect output, so it is recommended to only use this
option if necessary. -dm/-nodm enables/disables the generation of
distributed-memory code (if the selected API supports this). --profile
enables the automatic insertion of profiling calls at either the
invoke or kernel level. `psyclone` uses the
:func:`generator.generate` function to generate the code. Please
see the :ref:`psyclone_script` documentation for more details.

For example::

    > psyclone algspec.f90
    > psyclone -oalg alg.f90 -opsy psy.f90 -api dynamo0.3 algspec.f90
    > psyclone -d ../kernel -s opt.py algspec.f90
    > psyclone -s ../scripts/opt.py -l algspec.f90

The generator module
--------------------

.. automodule:: psyclone.generator

.. autofunction:: generate

The parse module
----------------

.. automodule:: psyclone.parse
 
.. autofunction:: parse

The transformations module
--------------------------

.. automodule:: psyclone.transformations
    :members:

The psyGen module
-----------------

.. automodule:: psyclone.psyGen

.. autoclass:: PSy
    :members:

The algGen module
-----------------

.. automodule:: psyclone.algGen

.. autoclass:: Alg
    :members:

The line_length module
----------------------

.. automodule:: psyclone.line_length

.. autoclass:: FortLineLength
    :members:

