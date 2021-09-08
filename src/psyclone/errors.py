# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic and A. J. Voysey, Met Office
#         J. Henrichs, Bureau of Meteorology
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
                "The func argument for the LazyString class should be a "
                "function, but found '{0}'.".format(type(func).__name__))
        self._func = func

    def __str__(self):
        '''
        :raises TypeError: if the function stored in self._func does \
            not return a string.
        '''
        result = self._func()
        if not isinstance(result, str):
            raise TypeError(
                "The function supplied to the LazyString class should return "
                "a string, but found '{0}'.".format(type(result).__name__))
        return result


class PSycloneError(Exception):
    ''' Provides a PSyclone specific error class as a generic parent class for
    all PSyclone exceptions.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = LazyString(lambda: "PSyclone Error: {0}".format(value))

    def __repr__(self):
        return type(self).__name__ + "()"

    def __str__(self):
        return str(self.value)


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


# For Sphinx AutoAPI documentation generation
__all__ = ["LazyString", "PSycloneError", "GenerationError",
           "FieldNotFoundError", "InternalError"]
