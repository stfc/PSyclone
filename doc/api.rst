.. _api-label:

API
===

.. program:: generator

generator.py

    .. cmdoption:: -h
    .. cmdoption:: -oalg <filename>
    .. cmdoption:: -opsy <filename>
    .. cmdoption:: -api <api>
    .. cmdoption:: -s <script>
    .. cmdoption:: -d <directory>
    .. cmdoption:: -r <filename>

    Command line version of the generator. -h prints out the command
    line options. If -oalg or -opsy are not provided then the
    generated code is printed to stdout, otherwise they are output to
    the specified file name. -api specifies the particular api to
    use. -s allows a script to be called which can modify (typically
    optimise) the PSy layer. -d specifies a directory to recursively
    search to find the associated kernel files. Uses the
    :func:`generator.generate` function to generate the code. Please
    see the run documentation for more details.

    For example::

    > python generator.py algspec.f90
    > python generator.py -oalg alg.f90 -opsy psy.f90 -api dynamo0.3 algspec.f90
    > python generator.py -d ../kernel -s opt.py algspec.f90
    > python generator.py -s ../scripts/opt.py algspec.f90

.. automodule:: generator

.. autofunction:: generate

The parse module
----------------

.. automodule:: parse
 
.. autofunction:: parse

The transformations module
--------------------------

.. automodule:: transformations
    :members:

The psyGen module
-----------------

.. automodule:: psyGen

.. autoclass:: PSy
    :members:

The algGen module
-----------------

.. automodule:: algGen

.. autoclass:: Alg
    :members:
