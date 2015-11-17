Line length
===========

By default PSyclone will generate fortran code with no consideration
of fortran line length limits.  As the line length limit for
free-format fortran is 132 characters, the code that is output may be
non-conformant.

Line length is not an issue for many compilers as they allow compiler
flags to be set which allow lines longer than the fortran
standard. However this is not the case for all compilers.

PSyclone therefore supports the wrapping of lines within the 132
character limit. The next two sections discuss how this is done when
scripting and when working interactively respectively.

Script
------

The generate.py script provides the -l option to wrap lines. Please
see the :ref:`fort_line_length` section for more details.

Interactive
-----------

When using PSyclone interactively the line length of the input
algorithm and Kernel files can be checked by setting the
:func:`parse.parse` function's "line_length" argument to "True".
::

    >>> from parse import parse
    >>> ast, info = parse("argspec.F90", line_length=True)

Similarly the "line_length" argument can be set to "True" if calling the
:func:`generator.generate` function. This function simply passes this
argument on to the :func:`parse.parse` function.
::

    >>> from generator import generate
    >>> alg, psy = generate("argspec.F90", line_length=True)

Line wrapping is performed as a post processing step, i.e. after the
code has been generated. This is done by an instance of the
:class:`line_length.FortLineLength` class. For example:
::

    >>> from generator import generate
    >>> from line_length import FortLineLength
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
of fortran syntax. It has been designed to work in the cases that are
anticipated to produce long lines when generating code using PSyclone,
i.e. call and subroutine arguments, use statements, directives and
declarations.

If other types of line are too long (e.g. an assignment) then an
exception will be raised. This situation is not expected as the code
generator should not produce assignment lines that are longer than
132 characters.

Other known situations that could cause an instance of the
:class:`line_length.FortLineLength` class to fail are

#. When an inline comment is used at the end of line to make it too long. However, PSyclone does not generate such code.

#. When a long line includes a string. The :class:`line_length.FortLineLength` class is not aware of strings and therefore will not produce correct code if it attempts to line break within a string. Again it is believed that PSyclone will currently not generate long lines with strings in them.
