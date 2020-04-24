def some_function(filename, kernel_path, node=None):
    '''The description starts with a capital letter and must have
    proper punctuation. Use for example :func:`parse.algorithm.parse`
    to reference functions, or :py:class:`psyclone.psyir.nodes.Node` for
    references to other PSyclone classes. The description must be followed
    by an empty line before the parameters start, but it is not necessary
    to escape each new line with a backslash here.

    :param str filename: start lower case, but add full stop. This\
                         line also shows the type declarations part of\
                         the parameter declaration, which can be used for\
                         any standard Python data type like str, bool, \
                         int, float.
    :param str kernel_path: no empty line between different parameters.\
                            If you need more than one line, add a backslash\
                            to the end of each line, otherwise sphinx\
                            will not layout the text correctly.
    :param node: the parameter type can also be declared in a stand-alone\
                 line which follows immediately after the corresponding\
                 :param: line. The actual type should only contain the\
                 type, no filler words like 'return type is integer'.\
                 Notice the empty line between parameter and return\
                 documentation.
    :type node: :py:class:`psyclone.psyir.nodes.Node`

    :return: a new node in the PSyIR. The return type must always be\
             specified in a separate line with an :rtype: entry. An empty\
             line separates the return documentation and the exceptions.
    :rtype: :py:class:`psyclone.psyir.nodes.Node`

    :raises IOError: lower case start with punctuation at the end.
    :raises GenerationError: list the same exception more than once if\
                             it can be raised by different errors.
    :raises GenerationError: same exception, raised by a different error.

    For example:

    >>> from psyclone.generator import generate
    >>> alg, psy = generate("algspec.f90")
    >>> alg, psy = generate("algspec.f90", kernel_path="src/kernels")

    '''
