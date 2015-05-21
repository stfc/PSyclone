.. _api-label:

API
===

.. program:: generator

generator.py

    .. cmdoption:: -oalg <filename>
    .. cmdoption:: -opsy <filename>
    .. cmdoption:: <filename>

    Command line version of the generator. If -oalg or -opsy or not provided then the generated code is printed to stdout. Uses the :func:`generator.generate` function to generate the code. Please see the run documentation for more details.

    For example::

    > python generator.py algspec.f90

.. automodule:: generator

.. autofunction:: generate

.. automodule:: parse
 
.. autofunction:: parse

.. automodule:: transformations

.. autoclass:: LoopFuseTrans

.. autoclass:: SwapTrans

.. autoclass:: OpenMPLoop

.. autoclass :: ColourTrans

.. automodule:: psyGen

.. autoclass:: PSy
    :members:

.. automodule:: algGen

.. autoclass:: Alg
    :members:
