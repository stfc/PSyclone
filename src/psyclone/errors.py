# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides various error classes used in PSyclone'''


class GenerationError(Exception):
    ''' Provides a PSyclone specific error class for errors found during PSy
    code generation.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Generation Error: "+value

    def __str__(self):
        return str(self.value)


class FieldNotFoundError(Exception):
    ''' Provides a PSyclone-specific error class when a field with the
    requested property/ies is not found.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Field not found error: "+value

    def __str__(self):
        return str(self.value)


class InternalError(Exception):
    '''
    PSyclone-specific exception for use when an internal error occurs (i.e.
    something that 'should not happen').

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "PSyclone internal error: "+value

    def __str__(self):
        return str(self.value)
