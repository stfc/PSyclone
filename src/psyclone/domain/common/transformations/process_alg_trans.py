# -----------------------------------------------------------------------------
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
# Author R. W. Ford STFC Daresbury Lab

'''Transform the domain-specific algorithm PSyIR into its
PSyclone-processed output form.

'''
from collections import namedtuple
from psyclone.domain.common.algorithm import AlgorithmInvokeCall
from psyclone.psyir.symbols import ContainerSymbol, GlobalInterface, RoutineSymbol

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Call, Routine, Container, Literal
from psyclone.domain.common.transformations import InvokeCallTrans
from psyclone.psyir.transformations import TransformationError


class ProcessAlgTrans(Transformation):
    '''Transform domain-specific algorithm PSyIR into its generic PSyIR
    PSyclone-processed form.

    '''
    def validate(self, node, options=None):
        pass

    def apply(self, psyir, options=None):
        ''' Apply transformation to the supplied PSyIR node.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: :py:class:`psyclone.psyir.node.Routine` or \
            :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(psyir, options=options)

        
        names = []
        Name = namedtuple('Name', ["subroutine_name", "module_name"])
        for idx, invoke_call in enumerate(psyir.walk(AlgorithmInvokeCall)):
            symbol_table = invoke_call.scope.symbol_table
            subroutine_root_name = "invoke_{0}".format(idx)
            if len(invoke_call.children) == 1:
                # Add the name of the kernel if there is only one
                # call
                subroutine_root_name += "_" + invoke_call.children[0].name
            subroutine_name = symbol_table.next_available_name(
                root_name=subroutine_root_name)
            module_root_name = "{0}_mod".format(subroutine_name)
            module_name = symbol_table.next_available_name(
                root_name=module_root_name)
            names.append(Name(subroutine_name, module_name))

        for idx, invoke_call in enumerate(psyir.walk(AlgorithmInvokeCall)):

            arguments = [] # TBD
            for kern in invoke_call.children:
                for arg in kern.children:
                    if isinstance(arg, Literal):
                        pass
                    else:
                        print (type(arg))

            subroutine_name = names[idx].subroutine_name
            module_name = names[idx].module_name
            symbol_table = invoke_call.scope.symbol_table
            container_symbol = ContainerSymbol(module_name)
            symbol_table.add(container_symbol)
            interface = GlobalInterface(container_symbol)
            routine_symbol = RoutineSymbol(subroutine_name, interface=interface)
            symbol_table.add(routine_symbol)
            call = Call.create(routine_symbol, arguments)
            invoke_call.replace_with(call)
            # self._invoke_trans.apply(call, options=options)
        exit(1)

    @property
    def name(self):
        '''
        :returns: a name identifying this transformation.
        :rtype: str

        '''
        return "ProcessAlgTrans"


__all__ = ['ProcessAlgTrans']
