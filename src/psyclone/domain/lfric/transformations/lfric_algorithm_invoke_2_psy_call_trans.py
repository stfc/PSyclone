# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab.

''' Transform a PSyclone LFRic algorithm-layer-specific invoke call into a call
to the corresponding PSy-layer routine.

'''

from psyclone.core import SymbolicMaths
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Call, ArrayReference, Reference, Literal
from psyclone.psyir.symbols import (RoutineSymbol, ContainerSymbol,
                                    ImportInterface)
from psyclone.domain.lfric.algorithm import (LFRicAlgorithmInvokeCall,
                                             LFRicBuiltinFunctor)
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations import TransformationError


class LFRicAlgorithmInvoke2PSyCallTrans(Transformation):
    '''
    Transforms an LFRicAlgorithmInvokeCall into a standard Call to a generated
    PSy-layer routine.

    '''
    def validate(self, node, options=None):
        '''Validate the node argument.

        :param node: a PSyIR node capturing an LFRicinvoke call.
        :type node: \
            :py:class:`psyclone.domain.lfric.algorithm.LFRicAlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied call argument is \
            not a PSyIR AlgorithmInvokeCall node.

        '''
        if not isinstance(node, LFRicAlgorithmInvokeCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be an `AlgorithmInvokeCall` node but found "
                f"'{type(node).__name__}'.")

    def apply(self, node, options=None):
        ''' Apply the transformation to the supplied node.

        :param node: a PSyIR algorithm invoke call node.
        :type node: \
            :py:class:`psyclone.domain.lfric.algorithm.LFRicAlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        '''
        self.validate(node, options=options)

        # The generic class does not handle Builtins so we do that here.
        builtin_symbols = set()
        for kern in node.children:
            if isinstance(kern, LFRicBuiltinFunctor):
                builtin_symbols.add(kern.symbol)

        super().apply(node, options=options)


__all__ = ['LFRicAlgorithmInvoke2PSyCallTrans']
