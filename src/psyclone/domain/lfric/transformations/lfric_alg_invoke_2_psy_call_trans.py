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
# Authors: R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab

''' Transform a PSyclone LFRic algorithm-layer-specific invoke call into a call
to the corresponding PSy-layer routine.

'''

from psyclone.core import SymbolicMaths
from psyclone.domain.common.transformations import AlgInvoke2PSyCallTrans
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicBuiltinFunctor)
from psyclone.domain.lfric.kernel import (
    FieldArgMetadata, FieldVectorArgMetadata,
    InterGridArgMetadata, InterGridVectorArgMetadata)
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Literal, Reference, ArrayReference


class LFRicAlgInvoke2PSyCallTrans(AlgInvoke2PSyCallTrans):
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
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied call argument is \
            not a PSyIR AlgorithmInvokeCall node.

        '''
        if not isinstance(node, LFRicAlgorithmInvokeCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be an `LFRicAlgorithmInvokeCall` node but "
                f"found '{type(node).__name__}'.")

    @staticmethod
    def add_arg(arg, arguments):
        '''Utility method to add argument arg to the arguments list as long as
        it conforms to the expected constraints.

        :param arg: the argument that might be added to the arguments \
            list.
        :type arg: :py:class:`psyclone.psyir.nodes.Reference`
        :param arguments: the arguments list that the argument might \
            be added to.
        :type arguments: List[:py:class:`psyclone.psyir.nodes.Reference`]

        :raises InternalError: if the arg argument is an unexpected \
            type.

        '''
        sym_maths = SymbolicMaths.get()

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

    def get_arguments(self, node, options=None):
        '''Creates the LFRic processed (lowered) argument list from the
        argument lists of the kernels within the invoke call and the
        kernel metadata.

        :param node: an LFRic algorithm invoke call.
        :type node: :py:class:`psyclone.domain.lfric.algorithm.psyir.\
            LFRicAlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param options["kernels"]: this option provides a list of \
            LFRic kernels for this LFRic Algorithm Invoke call.
        :type options["kernels"]: \
            List[:py:class:`psyclone.psyir.nodes.Container`]

        :raises GenerationError: if the number of arguments in the \
            invoke does not match the expected number of arguments \
            specified by the metadata.

        '''
        # TODO: Check that the number of arguments matches that expected by the metadata.

        const = LFRicConstants()
        kernels = options["kernels"]

        arguments = []
        quad_arguments = []
        stencil_arguments = []
        for add_arguments in [False, True]:
            # First check that the number of arguments in the kernel
            # functor matches the expected number of arguments
            # according to the associated kernel metadata, then, if
            # so, add the arguments in the appropriate order.
            for kern_call in node.children:
                kernel = kernels[id(kern_call)]
                kernel_metadata = kernel.children[0].metadata
                arg_idx = 0
                for meta_arg in kernel_metadata.meta_args:
                    arg = kern_call.children[arg_idx]
                    if add_arguments:
                        self.add_arg(arg, arguments)
                    # If this is a stencil arg then skip any additional
                    # arguments.
                    if type(meta_arg) in [
                            FieldArgMetadata, FieldVectorArgMetadata,
                            InterGridArgMetadata, InterGridVectorArgMetadata]:
                        if meta_arg.stencil:
                            arg_idx += 1
                            if add_arguments:
                                stencil_arg = kern_call.children[arg_idx]
                                self.add_arg(stencil_arg, stencil_arguments)
                            if meta_arg.stencil == "xory1d":
                                arg_idx += 1
                                if add_arguments:
                                    stencil_arg = kern_call.children[arg_idx]
                                    self.add_arg(
                                        stencil_arg, stencil_arguments)
                    arg_idx += 1
                if kernel_metadata.shapes and \
                   [quad for quad in kernel_metadata.shapes
                    if quad in const.VALID_QUADRATURE_SHAPES]:
                    # There is an additional quadrature argument.
                    arg_idx += 1
                    if add_arguments:
                        quad_arg = kern_call.children[arg_idx]
                        self.add_arg(quad_arg, quad_arguments)
            if not add_arguments:
                if len(kern_call.children) != arg_idx:
                    raise GenerationError(
                        f"The invoke kernel functor '{kern_call.name}' has "
                        f"{len(kern_call.children)} arguments, but the kernel "
                        f"metadata expects there to be {arg_idx} arguments.")

        arguments.extend(stencil_arguments)
        arguments.extend(quad_arguments)

        return arguments

    def apply(self, node, options=None):
        ''' Apply the transformation to the supplied LFRicAlgorithmInvokeCall
        node. That node is replaced by a Call to the corresponding PSy-layer
        routine with appropriate arguments. The symbols representing any
        LFRic Builtins that are now no longer referred to are removed.

        :param node: a PSyIR algorithm invoke call node.
        :type node: \
        :py:class:`psyclone.domain.lfric.algorithm.LFRicAlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options=options)

        # The generic class does not handle Builtins so we do that here. We
        # have to record which Builtins are involved before the call is
        # transformed.
        builtin_symbols = set()
        for kern in node.children:
            if isinstance(kern, LFRicBuiltinFunctor):
                builtin_symbols.add(kern.symbol)

        # 'node' will get replaced with a new Call node so keep a record
        # of where we are in the tree.
        parent = node.parent

        super().apply(node, options=options)

        # Now that the transformation is done, check whether we can remove
        # any of the symbols for the Builtins.
        for sym in builtin_symbols:
            table = sym.find_symbol_table(parent)
            node = table.node
            functors = node.walk(LFRicBuiltinFunctor)
            for func in functors:
                if sym is func.symbol:
                    break
            else:
                # We didn't find a Functor referring to this symbol so
                # we can remove it.
                # TODO #898 SymbolTable.remove() does not support
                # DataTypeSymbol so remove it manually.
                # pylint: disable=protected-access
                del table._symbols[sym.name]


__all__ = ['LFRicAlgInvoke2PSyCallTrans']
