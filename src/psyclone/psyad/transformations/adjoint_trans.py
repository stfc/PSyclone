# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''This module contains an abstract parent class for adjoint
transformations.

'''
from psyclone.psyGen import Transformation
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.visitor import PSyIRVisitor
from psyclone.psyir.symbols import DataSymbol

# AdjointTransformation is purposefully abstract. It does not
# implement the validate or apply methods and instead provides
# initialisation that all subclasses can benefit from. Therefore we
# disable the pylint warning here.
# pylint: disable=abstract-method

# We make use of the form of super that works with both Python2 and
# Python3 here so disable the pylint warning about using a Python3
# specific version of super.
# pylint: disable=super-with-arguments


class AdjointTransformation(Transformation):
    '''An abstract class for Adjoint transformations. Requires a list of
    active variables to be passed when creating an instance of the
    class. Also supports an optional writer argument.

    :param active_variables: a list of names of the active variables.
    :type active_variables: list of \
        :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param writer: the writer to use when outputting error \
        information. Defaults to FortranWriter.
    :type writer: subclass of \
        :py:class:`psyclone.psyir.backend.visitor.PSyIRVisitor`

    :raises TypeError: if the active_variables or writer arguments are \
        of the wrong type.

    '''
    def __init__(self, active_variables, writer=FortranWriter()):
        super(AdjointTransformation, self).__init__()

        if not isinstance(active_variables, list):
            raise TypeError(
                "The active variables argument should be a list, but found "
                "'{0}'.".format(type(active_variables).__name__))

        if not active_variables:
            raise TypeError("There should be at least one active variable.")

        for active_variable in active_variables:
            if not isinstance(active_variable, DataSymbol):
                raise TypeError(
                    "Active variables should be of type DataSymbol, but "
                    "found '{0}'.".format(type(active_variable).__name__))

        if not isinstance(writer, PSyIRVisitor):
            raise TypeError(
                "The writer argument should be a PSyIRVisitor but found "
                "'{0}'.".format(type(writer).__name__))

        # A list of active variables.
        self._active_variables = active_variables
        # The writer to use when outputting error information.
        self._writer = writer


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["AdjointTransformation"]
