# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module provides various error classes used in PSyclone'''


class LazyString:
    '''Utility that defers any computation associated with computing a
    string until the string is required. This is particularly useful
    for exceptions, where the string will typically not need to be
    computed unless the program is about to stop.

    :param function func: a function that computes a string.

    :raises TypeError: if the func argument is not a function.

    '''
    def __init__(self, func):
        if not hasattr(func, '__call__'):
            raise TypeError(
                f"The func argument for the LazyString class should be a "
                f"function, but found '{type(func).__name__}'.")
        self._func = func

    def __str__(self):
        '''
        :raises TypeError: if the function stored in self._func does \
            not return a string.
        '''
        result = self._func()
        if not isinstance(result, str):
            raise TypeError(
                f"The function supplied to the LazyString class should return "
                f"a string, but found '{type(result).__name__}'.")
        return result


class PSycloneError(Exception):
    ''' Provides a PSyclone specific error class as a generic parent class for
    all PSyclone exceptions.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = LazyString(lambda: f"PSyclone Error: {value}")

    def __repr__(self):
        return type(self).__name__ + "()"

    def __str__(self):
        return str(self.value)


class UnresolvedDependencyError(PSycloneError):
    ''' Provides a PSyclone specific error class for errors detected when
    resolving dependencies in the code.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "UnresolvedDependencyError: "+str(value)


class GenerationError(PSycloneError):
    ''' Provides a PSyclone specific error class for errors found during PSy
    code generation.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "Generation Error: "+str(value)


class FieldNotFoundError(PSycloneError):
    ''' Provides a PSyclone-specific error class when a field with the
    requested property/ies is not found.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "Field not found error: "+str(value)


class InternalError(PSycloneError):
    '''
    PSyclone-specific exception for use when an internal error occurs (i.e.
    something that 'should not happen').

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "PSyclone internal error: "+str(value)


class DocParseError(PSycloneError):
    '''
    PSyclone-specific exception for use when an error is found in a docstring
    while parsing in the docstring_parser.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "Docstring parsing error: "+str(value)


# For Sphinx AutoAPI documentation generation
__all__ = ["LazyString", "PSycloneError", "GenerationError",
           "FieldNotFoundError", "InternalError"]
