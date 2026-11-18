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

Prefer one main class per file. Helper classes or functions and closely related
classes (e.g. domain metadata or available built-ins) may be grouped in the same
file when this improves clarity. Use PEP8 naming conventions: for example, the
class ``ExtractTrans`` belongs in ``extract_trans.py``.

All files under ``src/psyclone/`` should have corresponding pytest tests under
``src/psyclone/tests/``, mirroring the directory layout and using the ``_test.py``
suffix. For example, tests for ``src/psyclone/psyir/nodes/node.py`` belong in
``src/psyclone/tests/psyir/nodes/node_test.py``.


Coding Style
############

Any new or modified code must conform to Python's PEP8 specification and be fully
covered by unit tests. Tests must assert the intended behaviour, including relevant
error cases; executing a line alone does not establish correctness.

Conformance can be tested with the following tools:

- The flake8 linter ( `flake8 .`)
- pytest (`pytest -n auto src/psyclone/`)
- Code coverage (`pytest --cov-report term-missing --cov psyclone`)

Installation of the tools is described in :ref:`dev-installation`.

Additionally, we recommend using `pylint` judiciously. Avoid refactoring solely to
satisfy a complexity warning when doing so would make the code harder to understand.

License notice
--------------

Project source and documentation files that support comments should carry
an SPDX header near the start, after any required shebang or encoding line.
Use the comment syntax of the file: ``#`` for Python, ``!`` for free-form
Fortran, and ``..`` for reStructuredText.

.. code-block:: python

    # -----------------------------------------------------------------------------
    # SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
    #                         Facilities Council
    # SPDX-License-Identifier: BSD-3-Clause
    # See the full LICENSE file in the project root for details.
    # -----------------------------------------------------------------------------


Imports
-------

Place imports at the top of the module, after the module docstring.
Put any ``from __future__`` imports first, followed by standard-library,
third-party, and PSyclone imports, separating these groups with blank lines.
Sort imports alphabetically within each group. Place any ``TYPE_CHECKING``
block after the runtime imports (see :ref:`style_type_hints`).

If a circular dependency requires a local import, explain why and suppress
only the relevant pylint warning, for example::

    def my_function() -> None:
        '''Perform an operation requiring access information.'''
        # Avoid a circular import with the module defining AccessType.
        # pylint: disable-next=import-outside-toplevel
        from psyclone.core.access_type import AccessType

        ...

.. _docstring_description:

Docstrings
----------

Every new or modified production code must have type annotations and a docstring
that describes its interface. When modifying an existing routine, update its
docstring and annotations to meet these requirements, even for a small fix.

Test functions and nested methods must have concise docstrings describing their
purpose; but type annotations and full interface documentation are optional
for these functions.

- Start the description with a capital letter and use proper punctuation.
  For a simple method whose description would repeat its return-value
  documentation, the description may be omitted.
- Document each parameter with ``:param name:``. Put types in the Python
  signature rather than duplicating them in ``:type:`` or ``:rtype:`` fields.
- Document returned values with ``:returns:`` where applicable. Annotate
  functions that return no value with ``-> None``; they do not need a
  ``:returns:`` field.
- Use ``:raises ExceptionType:`` to describe relevant error conditions.
  Repeat the exception type when different conditions need separate entries.
  Omit this section when there are no relevant exceptions to document.
- Start field descriptions with a lower-case letter and end with a full stop.
- Indent continuation lines and do not escape newlines with backslashes.
- Separate the description, parameters, return and raises sections with a
  blank line, but do not insert blank lines between each individual parameter
  and raises entry.
- Use Sphinx roles for references to code, as described in
  :ref:`style_documentation`.

For example:

.. code-block:: python

    def example(arg1: str, arg2: int) -> None:
        ''' Describe example method.

        :param arg1: describe first argument.
        :param arg2: describe second argument.

        :returns: describe what the method returns.

        :raises TypeError: condition that raises this error.
        :raises ValueError: condition that raises this error.

        '''

.. _style_type_hints:

Type annotations
----------------

..
  TODO #3682: Update with https://github.com/stfc/PSyclone/pull/3681#discussion_r4601781496
Use Python 3.9-compatible annotations, following PEP 484
(https://peps.python.org/pep-0484/). Use ``Union[X, Y]`` from ``typing``
rather than ``X | Y``. Use ``Optional[X]`` or ``Union[X, None]`` for a value
that may be ``None``.

If an annotation refers to a class defined later in the module, use
``from __future__ import annotations``.

When an import is needed only for annotations and would cause a circular
import at runtime, put it in an ``if TYPE_CHECKING:`` block. Quote the
annotation unless the module uses ``from __future__ import annotations``::

    from typing import TYPE_CHECKING

    if TYPE_CHECKING:
        from psyclone.psyir.nodes import Node

    def visit(node: "Node") -> None:
        '''Visit a PSyIR node.

        :param node: the node to visit.

        '''
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
This retains contextual information about the source of the error. For example::

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
    logger.info("Example of a logging message.")

The logger should be initialised in the module scope.

Logging has 5 levels of information: ``debug``, ``info``,
``warning``, ``error``, and ``critical``. The use of these levels is left to the
developers' and reviewers' judgement.

Additional contextual information (such as class/function information) should
also be placed into logging messages when generated in a module containing
large numbers of classes or functions.

Note that the ``warning`` log level should not be used to give important
warnings to users, but only to give warning information for developers.
For warnings aimed at users, instead use ``warnings.warn`` from the Python
standard ``warnings`` import, i.e.:

.. code-block:: python

   import warnings

   warnings.warn(self._deprecation_warning, DeprecationWarning, stacklevel=2)

Care must be taken when testing logging calls added to PSyclone. See
:ref:`logging_testing` for details.


.. _style_documentation:

Documentation Style
###################

Use Sphinx Python-domain roles to link references to PSyclone APIs in both
prose and docstrings: ``:py:class:`` for classes, ``:py:func:`` for functions,
and ``:py:meth:`` for methods. Use the full Python path on first mention,
for example ``:py:class:`psyclone.core.signature.Signature```.
On subsequent mentions, prefix the target with ``~`` to display only its
final component: ``:py:class:`~psyclone.core.signature.Signature```.
Use double backticks for file names, shell commands, and other inline code.

Use ``.. code-block:: LANGUAGE`` for syntax-highlighted examples. This also
works inside docstrings included in the documentation::

    .. code-block:: fortran

        do j = 1, size(field)
            field(j) = 0.0
        end do

