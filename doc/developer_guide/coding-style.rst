.. -----------------------------------------------------------------------------
.. SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
..                         Facilities Council
.. SPDX-License-Identifier: BSD-3-Clause
.. See the full LICENSE file in the project root for details.
.. -----------------------------------------------------------------------------

.. _coding-style:

Coding and Documentation Style
******************************

Directory Layout and File Names
###############################

The main directories in the PSyclone repository are:

.. list-table:: Repository directory layout
   :header-rows: 1
   :widths: 35 65

   * - Path
     - Description
   * - ``bin/``
     - Top-level driver scripts for PSyclone and the PSyclone kernel tool
   * - ``doc/``
     - Documentation source using Sphinx
   * - ``examples/``
     - Multiple examples of PSyclone usage and transformation scripts
   * - ``src/psyclone``
     - The Python source code
   * - ``src/psyclone/psyir/``
     - Classes related to the PSyclone Intermediate Representation (PSyIR)
   * - ``src/psyclone/domain/<API_NAME>``
     - Classes specific to the supported PSyKAl DSLs
   * - ``src/psyclone/tests/``
     - Unit and functional tests using pytest
   * - ``tutorial/practicals``
     - Hands-on exercises using a local installation of PSyclone

In general, any file in PSyclone should contain one main class (helper classes or
functions specific to that class are of course possible), following the naming
conventions recommended in PEP8.
The corresponding file name should be derived from the class name by replacing
all upper case letters with lower case, and adding a '_' to separate words. So the
file containing the class `ExtractTrans` should be called `extract_trans.py`.
Sets of large amounts of classes with very inter-related semantics (e.g. a domain
metadata, a domain set of built-ins) can also go under a single file if that helps

Every file under `src/psyclone/psyir/` should contain an equivalent file wiht pytest
unit-tests under `src/psyclone/psyir/tests/` with the same directory layout and the
`_test.py` postfix. For example, the unit tests of `src/psyclone/psyir/nodes/node.py`
are located in `src/psyclone/tests/psyir/nodes/node_test.py`.


Coding Style
############

Any new PSyclone code must conform to Python's PEP8 specification and be fully
covered by unit tests. Conformance can be tested with the following tools:

- The flake8 linter ( `flake8 .`)
- pytest (`pytest -n auto src/psyclone/`)
- Code coverage (`pytest --cov-report term-missing --cov psyclone`)

Installation of the tools is described in :ref:`dev-installation`.

We also recommend judicious use of `pylint`. In some cases pylint errors and
warnings can be suppressed using the pylint markup code::

    # pylint: disable=too-many-branches
    def _upper_bound(self):
        ....

It is up to the developer to decide if code should be refactored to
avoid a warning. For example, a warning about too many local
variables in a function might be better suppressed instead of 
removing one variable, or refactoring the code to create two functions.

Additional rules that apply:

  #) All setter and getter functions (that do not do any significant work)
     must be declared as property and setter to allow them to be used
     without parentheses::

        @property
        def ast_end(self):
            '''
            :returns: a reference to the last node in the fparser2 parse tree \
                      that represents a child of this PSyIR node or None.
            :rtype: sub-class of :py:class:`fparser.two.utils.Base`
            '''
            return self._ast_end

        @ast.setter
        def ast(self, ast):
            '''
            Set a reference to the fparser2 node associated with this Node.
                   :param ast: fparser2 node associated with this Node.
            :type ast: :py:class:`fparser.two.utils.Base`
            '''
            self._ast = ast

  #) Any function must contain an interface description, see
     :ref:`interface_description` for full details..
  #) Any new line of code must be covered by at least one test case,
     see :ref:`test_suite` and especially :ref:`test_coverage`.

  #) Importing other modules should be done at the top of a file, and
     the import statements must be sorted alphabetically. If a
     module cannot be import at the top of a file due to a circular
     dependency, a comment must be added to the local import statement,
     and the corresponding pylint warning must be disabled, e.g.::

         def my_function():
             ...
             # Avoid circular import
             # pylint: disable=import-outside-toplevel
             from psyclone.core.access_type import AccessType


License notice
--------------

Every file should start with the following header.
 
.. code-block: python

    # -----------------------------------------------------------------------------
    # SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
    #                         Facilities Council
    # SPDX-License-Identifier: BSD-3-Clause
    # See the full LICENSE file in the project root for details.
    # -----------------------------------------------------------------------------


Docstrings
----------

The docstring to any new or modified routine in PSyclone must be fully
documented with typehints, description, parameters, returns and raises sections.
If the method is simple and the docstring description and returns value essentially
say the same, the description can be omitted. An example of a method docstring is
shown below:

.. code-block: python

    def some_function(
        filename: str,
        kernel_path: str,
        node: Union[Node, None] = None
    ) -> Node:
        '''The description starts with a capital letter and must have proper
        punctuation. If it's useful to reference other code, this should use
        Sphinx notation, for example use :func:`parse.algorithm.parse` to
        reference functions, or :py:class:`psyclone.psyir.nodes.Node` to reference
        classes. Do not use backslash to escape each new lines. The description
        must be followed by an empty line before the parameters start (and also
        before the returns and raises sections if these exist).

        :param filename: start lower case, but add full stop.
        :param kernel_path: no empty line between different parameters.
            If you need more than one line, continue the following lines with
            an indentation, otherwise sphinx will not layout the text correctly.
        :param node: the parameter type should be declared using Python 3.9
            compatible typehint, i.e. ``Union[X, Y]`` rather than ``X | Y``.
            Type hints should be specified according to PEP 484
            (https://peps.python.org/pep-0483/).

        :returns: description of the returned value. The return type must always
            be specified as a typehint, with -> None used for functions with
            no return value.

        :raises IOError: lower case start with punctuation at the end.
        :raises GenerationError: list the same exception more than once if
            it can be raised by different errors.
        :raises GenerationError: same exception, raised by a different error.

        '''

Notes about typehints:


  #) If a type hint refers to a class that is defined only after its use
     in a given module, use ``from __future__ import annotations``.

  #) If a class is imported only for typehint signatures, not possible due to circular
     imports then its import should be protected by a ``if TYPE_CHECKING:`` block,
     and its actualy use should be put in double quotes, e.g.::

        if TYPE_CHECKING:
            from psyclone.somepath.class import SomeClass

        def my_func(my_arg: "SomeClass") -> None:
        ...


Exceptions
----------

When raising an exception, every effort must be made to ensure that
the associated message is clear and provides as much information as
possible. However, it should *not* contain the name of the current
routine/class etc. as this is provided in the Python stack trace.
Where it makes sense to do so, the past tense should be used,
e.g. "expected a str but got an object of type 'blah'."

In the event that code that is handling an exception then needs to
raise a new exception, the `raise XXX from YYY` form must be used.
(This ensures that contextual information about the source of the error is
retained.). For example::

    try:
        something()
    except KeyError as err:
        raise InternalError("Useful message here") from err


Logging
-------

PSyclone supports the Python ``logging`` module, to help report useful
information throughout the code. By default the logging infrastructure
is switched off, but can be enabled either through the PSyclone command-line
options or programmatically with:

.. code-block:: python

    import logging
    logger = logging.getLogger("psyclone")
    logger.setLevel(logging.LEVEL)

where ``logging.LEVEL`` is the required level of those defined in the standard
logging library.

To use logging in a module, you can add:

.. code-block:: python

    import logging
    logger = logging.getLogger(__name__)
    logging.info("Example of a logging message.")

The logger should be initialised in the module scope.

Logging has 5 levels of information: ``debug``, ``info``,
``warning``, ``error``, and ``critical``. The use of these levels is left to the
developers and reviewers decision.

Additional contextual information (such as class/function information) should
also be placed into logging messages when generated in a module containing
large numbers of classes or functions.

Note that the ``warning`` log level should not be used to give important
warnings to users, but only to give warning information for developers.
For warnings aimed at users, instead use ``warnings.warn`` from the Python
standard ``warnings`` import, i.e.:

.. code-block:: python

   warnings.warn(self._deprecation_warning, DeprecationWarning, 2)

Care must be taken when testing logging calls added to PSyclone. See
:ref:`logging_testing` for details.


.. _interface_description:


Documentation Style
###################

When writing documentation, each reference to a PSyclone class or function
should be set in italics (i.e. enclosed by single backticks) except in headings.
The first time a class or function is mentioned, use the full Python path, e.g.:
`psyclone.core.signature.Signature`. After that just use the
class name (again in italics).
File names and shell commands should be set in double back-ticks (\`\`).

Any code examples provided in the documentation should be using the
right syntax to allow for syntax highlighting. The option
``.. highlight:: LANGUAGE`` is used to define the language to be used
for all text following the directive (until the end of the current file).
It is recommended to have at most one ``highlight`` directive in
a file and to avoid switching default languages several times within a file.
Having a default language set in a file allows use of the very convenient
shortcut notation "::". Note that the "::" will be replaced with a
single ":" if the previous character is not a space::

    .. highlight:: fortran

    Code example::

        program this_is_highlighted_using_fortran_syntax

    And if you don't want any ":" in the previous line, just
    add a white space before the "::" ::

        program there_is_no_colon_in_the_previous_description

If some examples in a file need a different highlighting syntax,
use the ``.. code-block:: LANGUAGE`` directive, which will only
change the syntax for the following code example::

    .. highlight:: fortran

    Code example::

        program this_is_highlighted_using_fortran_syntax

    This is a python example:

    .. code-block:: python

        trans = self.get_transform()


It is also possible to use ``code-block`` inside a Python docstring that
is pulled into a document::


    class GOConstLoopBoundsTrans(Transformation):
        ''' Switch on (or off) the use of constant loop bounds within
        a GOInvokeSchedule. In the absence of constant loop bounds, PSyclone will
        generate loops where the bounds are obtained by de-referencing a field
        object, e.g.:

        .. code-block:: fortran

          DO j = my_field%grid%internal%ystart, my_field%grid%internal%ystop

        Some compilers are able to produce more efficient code if they are
        ...

The code block will be highlighted as Fortran code.
