# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

from typing import Union
from psyclone.psyir.nodes import Node


def some_function(filename: str, kernel_path: str,
                  node: Union[Node, None] = None) -> Node:
    '''The description starts with a capital letter and must have
    proper punctuation. Use for example :func:`parse.algorithm.parse`
    to reference functions, or :py:class:`psyclone.psyir.nodes.Node` for
    references to other PSyclone classes. The description must be followed
    by an empty line before the parameters start, but it is not necessary
    to escape each new line with a backslash here.

    :param filename: start lower case, but add full stop.
    :param kernel_path: no empty line between different parameters.
        If you need more than one line, continue the following lines with
        an indentation, otherwise sphinx will not layout the text correctly.
    :param node: the parameter type should be declared using Python 3.9
        compatible type hints, i.e. ``Union[X, Y]`` rather than ``X | Y``.
        Type hints should be specified according to PEP 484
        (https://peps.python.org/pep-0483/). Notice the empty line between
        parameter and return documentation. ``Optional[Node]`` would also be a
        valid type hint for this node, and the Sphinx docstring may
        automatically adjust it to ``Optional[Node]``.

    :returns: a new node in the PSyIR. The return type must always be
        specified as a typehint, with -> None used for functions with
        no return value. An empty line separates the return
        documentation and the exceptions.

    :raises IOError: lower case start with punctuation at the end.
    :raises GenerationError: list the same exception more than once if
        it can be raised by different errors.
    :raises GenerationError: same exception, raised by a different error.

    '''
