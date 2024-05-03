# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab

'''Abstract base class to Transform a PSyclone
algorithm-layer-specific invoke call into a call to the corresponding
PSy-layer routine.

'''
import abc

from psyclone.core import SymbolicMaths
from psyclone.domain.common.algorithm import AlgorithmInvokeCall, KernelFunctor
from psyclone.errors import InternalError
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Call, Routine, Literal, Reference, CodeBlock, UnaryOperation, Node)
from psyclone.psyir.symbols import (ContainerSymbol,
                                    ImportInterface, RoutineSymbol)
from psyclone.psyir.transformations import TransformationError


class AlgInvoke2PSyCallTrans(Transformation, abc.ABC):
    '''Base class to transform (lower) an AlgorithmInvokeCall into a
    standard Call to a generated PSy-layer routine. Requires the
    abstract get_arguments method to be implemented as the logic to
    create arguments can differ between APIs.

    This transformation would normally be written as a lowering method
    on an AlgorithmInvokeCall. However, we don't always want to lower
    the code as we want the flexibility to also be able to output
    algorithm-layer code containing invoke's. We therefore need to
    selectively apply the lowering, which is naturally written as a
    transformation.

    '''
    def validate(self, node, options=None):
        '''Validate the node argument.

        :param node: a PSyIR node capturing an invoke call.
        :type node: \
            :py:class:`psyclone.domain.common.algorithm.AlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

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

    @abc.abstractmethod
    def get_arguments(self, node, options=None):
        '''
        :param node: a PSyIR algorithm invoke call node.
        :type node: \
            :py:class:`psyclone.domain.common.psyir.AlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        '''

    @staticmethod
    def _add_arg(arg, arguments):
        '''Utility method to add argument arg to the arguments list as long as
        it conforms to the expected constraints.

        :param arg: the argument that might be added to the arguments list.
        :type arg: :py:class:`psyclone.psyir.nodes.Reference`
        :param arguments: the arguments list that the argument might \
            be added to.
        :type arguments: List[:py:class:`psyclone.psyir.nodes.Reference`]

        :raises TypeError: if the arg argument is an unexpected type.

        '''
        sym_maths = SymbolicMaths.get()

        if isinstance(arg, Literal) or (
                isinstance(arg, UnaryOperation) and
                arg.operator == UnaryOperation.Operator.MINUS and
                isinstance(arg.children[0], Literal)):
            # Literals are not passed by argument.
            pass
        elif isinstance(arg, Reference):
            for existing_arg in arguments:
                if sym_maths.equal(arg, existing_arg):
                    break
            else:
                arguments.append(arg.copy())
        elif isinstance(arg, CodeBlock):
            arguments.append(arg.copy())
        else:
            if isinstance(arg, Node):
                string = f"but '{arg.debug_string()}'"
                if arg.parent and isinstance(arg.parent, KernelFunctor):
                    string = f"{string} passed to kernel '{arg.parent.name}'"
                string = f"{string} is of type '{type(arg).__name__}'."
            else:
                string = f"but found '{type(arg).__name__}'."
            raise TypeError(
                f"Expected Algorithm-layer kernel arguments to be "
                f"a Literal, Reference or CodeBlock, {string}.")

    @staticmethod
    def remove_imported_symbols(node):
        '''Removes any imported kernel functor symbols from the supplied
        AlgorithmInvokeCall if they are not used in another
        AlgorithmInvokeCall. Also removes the associated container
        symbol if there are now no symbols imported from it.

        :param node: an AlgorithmInvokeCall node.
        :type node: \
            :py:class:`psyclone.domain.common.algorithm.AlgorithmInvokeCall`

        '''
        all_functors = node.root.walk(KernelFunctor)
        local_functor_symbols = set()
        remote_functor_symbols = set()
        for functor in all_functors:
            if not functor.symbol.is_import:
                # Builtin KernelFunctors are not imported
                continue
            if functor.parent is node:
                local_functor_symbols.add(functor.symbol)
            else:
                remote_functor_symbols.add(functor.symbol)
        # Functor symbols in this invoke that are not used elsewhere
        functor_symbols_to_remove = local_functor_symbols.difference(
            remote_functor_symbols)

        for functor_symbol in functor_symbols_to_remove:
            # remove the symbol (and, potentially, the container
            # from which it is imported) from the symbol table.
            container_symbol = \
                functor_symbol.interface.container_symbol
            c_symbol_table = container_symbol.find_symbol_table(node)
            # issue #898 not currently possible to remove a
            # DataTypeSymbol using the remove method.
            # pylint: disable=protected-access
            norm_name = c_symbol_table._normalize(
                functor_symbol.name)
            c_symbol_table._symbols.pop(norm_name)
            # pylint: enable=protected-access
            try:
                c_symbol_table.remove(container_symbol)
            except ValueError:
                # container symbol still imports one or more symbols
                # so can not be removed.
                pass

    def apply(self, node, options=None):
        ''' Apply the transformation to the supplied AlgorithmInvokeCall.
        The supplied node will be replaced with a Call node with appropriate
        arguments. If there are no more invoke calls in the scope of the symbol
        table containing the 'invoke' symbol then that symbol is removed.

        :param node: a PSyIR algorithm invoke call node.
        :type node: \
            :py:class:`psyclone.domain.common.psyir.AlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options=options)
        node.create_psylayer_symbol_root_names()
        arguments = self.get_arguments(node, options=options)
        symbol_table = node.ancestor(Routine).symbol_table

        # Remove functor symbols that are no longer used.
        self.remove_imported_symbols(node)

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
