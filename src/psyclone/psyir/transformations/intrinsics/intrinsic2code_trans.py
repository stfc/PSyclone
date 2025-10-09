# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab
# Modified: A. B. G. Chalk, STFC Daresbury Lab

'''Module providing an abstract class which provides some generic
functionality required by transformations of PSyIR intrinsic
(such as MIN and MAX) to code.

'''
import abc
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Assignment, IntrinsicCall
from psyclone.psyir.symbols import ArrayType, ScalarType
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class Intrinsic2CodeTrans(Transformation, metaclass=abc.ABCMeta):
    '''Provides support for transformations from PSyIR IntrinsicCall
    nodes to equivalent PSyIR code in a PSyIR tree. Such
    transformations can be useful when the intrinsic is not supported
    by a particular backend or if it is more efficient to have
    explicit code.

    '''
    def __init__(self):
        super().__init__()
        self._intrinsic = None

    def __str__(self):
        return (f"Convert the PSyIR '{self._intrinsic.name}' "
                f"intrinsic to equivalent PSyIR code.")

    def validate(self, node, options=None, **kwargs):
        '''Perform various checks to ensure that it is valid to apply
        an intrinsic transformation to the supplied Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the node argument is not the
            expected type.
        :raises TransformationError: if the IntrinsicCall node does
            not have an Assignment Node as an ancestor.

        '''
        # Validate any keyword argument options
        if not options:
            self.validate_options(**kwargs)

        # Check that the node is one of the expected types.
        if not isinstance(node, IntrinsicCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node must "
                f"be an 'IntrinsicCall', but found '{type(node).__name__}'.")
        if node.intrinsic != self._intrinsic:
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied "
                f"IntrinsicCall must be a '{self._intrinsic.name}' but found: "
                f"'{node.intrinsic.name}'.")
        # Check that there is an Assignment node that is an ancestor
        # of this Operation.
        if not node.ancestor(Assignment):
            raise TransformationError(
                f"Error in {self.name} transformation. This transformation "
                f"requires the operator to be part of an assignment "
                f"statement, but no such assignment was found.")

    def _validate_scalar_arg(self, node, options=None):
        '''
        Check that the argument to the intrinsic is a scalar of known type.

        :param node: the target intrinsic call.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: any options for the transformation.
        :type options: dict[str, Any]

        :raises TransformationError: if the supplied SIGN call operates on
            an argument of array type or unsupported/unresolved type.

        '''
        result_type = node.arguments[0].datatype
        if isinstance(result_type, ArrayType):
            raise TransformationError(
                f"Transformation {self.name} cannot be applied to SIGN calls "
                f"which have an array as argument but "
                f"'{node.arguments[0].debug_string()}' is of array type. It "
                f"may be possible to use the ArrayAssignment2LoopsTrans "
                f"to convert this to a scalar argument.")
        if not isinstance(result_type, ScalarType):
            raise TransformationError(
                f"Transformation {self.name} cannot be applied to "
                f"'{node.debug_string()} because the type of the "
                f"argument '{node.arguments[0].debug_string()}' is "
                f"{result_type}")

    @abc.abstractmethod
    def apply(self, node, options=None, **kwargs):
        '''
        Apply the Intrinsic2CodeTrans transformation.

        :param node: the target intrinsic call.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: any options for the transformation.
        :type options: dict[str, Any]
        '''


# For AutoAPI auto-documentation generation.
__all__ = ["Intrinsic2CodeTrans"]
