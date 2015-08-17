.. _api-label:

API
===

.. program:: generator

generator.py

    .. cmdoption:: -oalg <filename>
    .. cmdoption:: -opsy <filename>
    .. cmdoption:: <filename>

    Command line version of the generator. If -oalg or -opsy are not
    provided then the generated code is printed to stdout. Uses the
    :func:`generator.generate` function to generate the code. Please
    see the run documentation for more details.

    For example::

    > python generator.py algspec.f90

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
