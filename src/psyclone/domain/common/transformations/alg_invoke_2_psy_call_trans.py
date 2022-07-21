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

''' Transform a PSyclone algorithm-layer-specific invoke call into a call
to the corresponding PSy-layer routine.

'''

from psyclone.core import SymbolicMaths
from psyclone.domain.common.algorithm import AlgorithmInvokeCall
from psyclone.errors import InternalError
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import ArrayReference, Call, Literal, Reference, \
    Routine
from psyclone.psyir.symbols import (ContainerSymbol,
                                    ImportInterface, RoutineSymbol)
from psyclone.psyir.transformations import TransformationError


class AlgInvoke2PSyCallTrans(Transformation):
    '''
    Transforms an AlgorithmInvokeCall into a standard Call to a generated
    PSy-layer routine.

    '''
    def validate(self, node, options=None):
        '''Validate the node argument.

        :param node: a PSyIR node capturing an invoke call.
        :type node: \
            :py:class:`psyclone.domain.common.algorithm.AlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, str]]

        :raises TransformationError: if the supplied call argument is \
            not a PSyIR AlgorithmInvokeCall node.
        :raises InternalError: if no corresponding 'invoke' symbol is present.

        '''
        if not isinstance(node, AlgorithmInvokeCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be an `AlgorithmInvokeCall` node but found "
                f"'{type(node).__name__}'.")

        try:
            _ = node.scope.symbol_table.lookup("invoke")
        except KeyError:
            # pylint: disable=raise-missing-from
            raise InternalError(
                "No 'invoke' symbol found despite there still being at least "
                "one AlgorithmInvokeCall node present.")

    def apply(self, node, options=None):
        ''' Apply the transformation to the supplied AlgorithmInvokeCall.
        The supplied node will be replaced with a Call node with appropriate
        arguments. If there are no more invoke calls in the scope of the symbol
        table containing the 'invoke' symbol then that symbol is removed.

        :param node: a PSyIR algorithm invoke call node.
        :type node: \
            :py:class:`psyclone.domain.common.psyir.AlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, str]]

        '''
        self.validate(node, options=options)

        node.create_psylayer_symbol_root_names()

        arguments = []
        sym_maths = SymbolicMaths.get()
        for kern in node.children:
            for arg in kern.children:
                if isinstance(arg, Literal):
                    # Literals are not passed by argument.
                    pass
                elif isinstance(arg, (Reference, ArrayReference)):
                    for existing_arg in arguments:
                        if sym_maths.equal(arg, existing_arg):
                            break
                    else:
                        arguments.append(arg.copy())
                else:
                    raise InternalError(
                        f"Expected Algorithm-layer kernel arguments to be "
                        f"a literal, reference or array reference, but "
                        f"found '{type(arg).__name__}'.")

        symbol_table = node.ancestor(Routine).symbol_table

        # TODO #753. At the moment the container and routine names
        # produced here will differ from the PSy-layer routine name if
        # there is a name clash in the algorithm layer.
        container_tag = node.psylayer_container_root_name
        try:
            container_symbol = symbol_table.lookup_with_tag(container_tag)
        except KeyError:
            container_symbol = symbol_table.new_symbol(
                root_name=container_tag, tag=container_tag,
                symbol_type=ContainerSymbol)

        routine_tag = node.psylayer_routine_root_name
        interface = ImportInterface(container_symbol)
        routine_symbol = symbol_table.new_symbol(
            root_name=routine_tag, tag=routine_tag, symbol_type=RoutineSymbol,
            interface=interface)

        psy_call = Call.create(routine_symbol, arguments)
        node.replace_with(psy_call)

        # Remove original 'invoke' symbol if there are no other
        # references to it. This keeps the symbol table up-to-date and
        # also avoids an exception being raised in the Fortran Writer
        # as the invoke symbol has an UnresolvedInterface.
        invoke_symbol = psy_call.scope.symbol_table.lookup("invoke")
        symbol_table = invoke_symbol.find_symbol_table(psy_call)
        if not symbol_table.node.walk(AlgorithmInvokeCall):
            symbol_table.remove(invoke_symbol)


__all__ = ['AlgInvoke2PSyCallTrans']
