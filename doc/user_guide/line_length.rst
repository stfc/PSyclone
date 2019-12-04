.. _line-length:

Line length
===========

By default PSyclone will generate fortran code with no consideration
of fortran line length limits.  As the line length limit for
free-form fortran is 132 characters, the code that is output may be
non-conformant.

Line length is not an issue for many compilers as they allow compiler
flags to be set which allow lines longer than the fortran
standard. However this is not the case for all compilers.

PSyclone therefore supports the wrapping of lines within the 132
character limit. The next two sections discuss how this is done when
scripting and when working interactively respectively.

Script
------

The `psyclone` script provides the -l option to wrap lines. Please
see the :ref:`fort_line_length` section for more details.

Interactive
-----------

When using PSyclone interactively the line lengths of the input
algorithm and Kernel files can be checked by setting the
:func:`psyclone.parse.algorithm.parse` function's "line_length"
argument to "True".  ::

    >>> from psyclone.parse.algorithm import parse
    >>> ast, info = parse("argspec.F90", line_length=True)

Similarly the "line_length" argument can be set to "True" if calling the
:func:`generator.generate` function. This function simply passes this
argument on to the :func:`psyclone.parse.algorithm.parse` function.
::

    >>> from psyclone.generator import generate
    >>> alg, psy = generate("argspec.F90", line_length=True)

Line wrapping is performed as a post processing step, i.e. after the
code has been generated. This is done by an instance of the
:class:`line_length.FortLineLength` class. For example:
::

    >>> from psyclone.generator import generate
    >>> from psyclone.line_length import FortLineLength
    >>> psy, alg = generate("algspec.f90", line_length=True)
    >>> line_length = FortLineLength()
    >>> psy_str = line_length.process(str(psy))
    >>> print psy_str
    >>> alg_str = line_length.process(str(alg))
    >>> print alg_str

.. _line-length-limitations:

Limitations
-----------

The :class:`line_length.FortLineLength` class is only partially aware
of fortran syntax. This awareness is required so that appropriate
continuation characters can be used (for example ``&`` at the end of a
line and ``!$omp&`` at the start of a line for OpenMP directives, ``&`` at
the end of a line for statements and ``&`` at the end of a line and ``&``
at the beginning of a line for strings).

Whilst statements only require an ``&`` at the end of the line when line
wrapping with free-form fortran they may optionally also have an ``&``
at the beginning of the subsequent line. In contrast, when splitting a
string over multiple lines an ``&`` is required at both
locations. Therefore an instance of the
:class:`line_length.FortLineLength` class will always add ``&`` at the
beginning of a continuation line for a statement, in case the line is
split within a string.

One known situation that could cause an instance of the
:class:`line_length.FortLineLength` class to fail is when an inline
comment is used at the end of a line to make it longer than the 132
character limit. Whilst PSyclone does not generate such code for the
PSy-layer, this might occur in Algorithm-layer code, even if the
Algorithm-layer code conforms to the 132 line length limit. The reason
for this is that PSyclone's internal parser concatenates lines
together, thus a long line correctly split with continuation characters
in the Algorithm-layer becomes a line that needs to be split by an
instance of the :class:`line_length.FortLineLength` class.
