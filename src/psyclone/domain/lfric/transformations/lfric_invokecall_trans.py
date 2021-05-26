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

'''Specialise generic PSyIR representing an invoke call withing the
algorithm layer to an LFRic algorithm-layer-specific invoke call which
uses specialised classes.

'''
from fparser.two.Fortran2003 import Actual_Arg_Spec, Name, \
    Char_Literal_Constant, Structure_Constructor

from psyclone.psyir.nodes import ArrayReference
from psyclone.psyir.transformations import TransformationError

from psyclone.domain.common.transformations import InvokeCallTrans
from psyclone.domain.lfric.algorithm import LFRicBuiltinFunctor, \
    LFRicKernelFunctor, LFRicAlgorithmInvokeCall
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP as builtins


class LFRicInvokeCallTrans(InvokeCallTrans):
    '''Transform a generic PSyIR representation of an Algorithm-layer
    invoke call to an LFRic version with specialised domain-specific
    nodes.

    '''
    def __init__(self):
        super(LFRicInvokeCallTrans, self).__init__()
        self._call_description = None

    def validate(self, call, options=None):
        ''' Check that the call argument has the expected structure.

        :param call: the PSyIR invoke call to be validated.
        :type call: :py:class:`psyclone.psyir.nodes.Call`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self._call_description = None
        super(LFRicInvokeCallTrans, self).validate(call, options=options)

    def _validate_fp2_node(self, fp2_node):
        '''Specialisation of the fparser2 node validation routine to
        additionally validate named arguments, which are specific to
        the LFRic API.

        :param fp2_node: an fparser2 Structure Constructor or Actual \
            Arg Spec node.
        :type fp2_node: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor` or \
            :py:class:`fparser.two.Fortran2003.Actual_Arg_Spec

        :raises TransformationError: if the named argument is not in \
            the expected form.
        :raises TransformationError: if more than one named argument \
            is found.
        :raises TransformationError: if the fparser2 node is not the \
            expected type.

        '''
        if isinstance(fp2_node, Structure_Constructor):
            pass
        elif isinstance(fp2_node, Actual_Arg_Spec):
            if not (isinstance(fp2_node.children[0], Name) and
                    fp2_node.children[0].string.lower() == "name" and
                    isinstance(fp2_node.children[1], Char_Literal_Constant)):
                raise TransformationError(
                    "Error in {0} transformation. If there is a named "
                    "argument, it must take the form name='str', but found "
                    "'{1}'.".format(self.name, str(fp2_node)))
            if self._call_description:
                raise TransformationError(
                    "Error in {0} transformation. There should be at most one "
                    "named argument in an invoke, but there are at least two: "
                    "{1} and {2}.".format(self.name, self._call_description,
                                          fp2_node.children[1].string))
            self._call_description = fp2_node.children[1].string
        else:
            raise TransformationError(
                "Error in {0} transformation. Expecting an algorithm invoke "
                "codeblock to contain either Structure-Constructor or "
                "actual-arg-spec, but found '{1}'."
                "".format(self.name, type(fp2_node).__name__))

    def apply(self, call, index, options=None):
        ''' Apply the transformation to the supplied node.

        :param call: a PSyIR call node capturing an invoke call in \
            generic PSyIR.
        :type call: :py:class:`psyclone.psyir.nodes.Call`
        :param int index: the position of this invoke call relative to \
            other invokes in the algorithm layer.
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(call, options=options)

        call_description = None
        calls = []
        for call_arg in call.children:

            arg_info = []
            if isinstance(call_arg, ArrayReference):
                # kernel or builtin misrepresented as ArrayReference
                args = call_arg.pop_all_children()
                name = call_arg.name
                if name in builtins:
                    node_type = LFRicBuiltinFunctor
                    type_symbol = call.scope.symbol_table.lookup(name)
                else:
                    node_type = LFRicKernelFunctor
                    type_symbol = call_arg.symbol
                arg_info.append((node_type, type_symbol, args))
            else:
                for fp2_node in call_arg._fp2_nodes:
                    if isinstance(fp2_node, Actual_Arg_Spec):
                        # This child is a named argument
                        call_description = fp2_node.children[1].string
                    else:
                        # This child is a kernel or builtin
                        name = fp2_node.children[0].string
                        if name in builtins:
                            node_type = LFRicBuiltinFunctor
                        else:
                            node_type = LFRicKernelFunctor
                        type_symbol = InvokeCallTrans._get_symbol(
                            call, fp2_node)
                        args = InvokeCallTrans._parse_args(call_arg, fp2_node)
                        arg_info.append((node_type, type_symbol, args))

            for (node_type, type_symbol, args) in arg_info:
                self._specialise_symbol(type_symbol)
                calls.append(node_type.create(type_symbol, args))

        invoke_call = LFRicAlgorithmInvokeCall.create(
            call.routine, calls, index, description=call_description)
        call.replace_with(invoke_call)

    @property
    def name(self):
        '''
        :returns: a name identifying this transformation.
        :rtype: str

        '''
        return "LFRicInvokeCallTrans"


__all__ = ['LFRicInvokeCallTrans']
