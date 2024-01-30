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

''' Transform a PSyclone LFRic algorithm-layer-specific invoke call into a call
to the corresponding PSy-layer routine.

'''

from psyclone.domain.common.transformations import AlgInvoke2PSyCallTrans
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicBuiltinFunctor)
from psyclone.domain.lfric.kernel import (
    FieldArgMetadata, FieldVectorArgMetadata, InterGridArgMetadata,
    InterGridVectorArgMetadata, LFRicKernelContainer)
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP
from psyclone.errors import GenerationError
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Literal, Container


class LFRicAlgInvoke2PSyCallTrans(AlgInvoke2PSyCallTrans):
    '''
    Transforms an LFRicAlgorithmInvokeCall into a standard Call to a generated
    PSy-layer routine.

    This transformation would normally be written as a lowering method
    on a LFRicAlgorithmInvokeCall. However, we don't always want to
    lower the code as we want the flexibility to also be able to
    output algorithm-layer code containing invoke's. We therefore need
    to selectively apply the lowering, which is naturally written as a
    transformation.

    '''
    def validate(self, node, options=None):
        '''Validate the node argument.

        :param node: a PSyIR node capturing an LFRicinvoke call.
        :type node: \
          :py:class:`psyclone.domain.lfric.algorithm.LFRicAlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param options["kernels"]: this option provides a list of \
            LFRic kernels for this LFRic Algorithm Invoke call.
        :type options["kernels"]: \
            List[:py:class:`psyclone.psyir.nodes.Container`]

        :raises TransformationError: if the supplied call argument is \
            not a PSyIR AlgorithmInvokeCall node.
        :raises TransformationError: if the 'kernels' option is not provided.

        '''
        if not isinstance(node, LFRicAlgorithmInvokeCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be an `LFRicAlgorithmInvokeCall` node but "
                f"found '{type(node).__name__}'.")
        try:
            kernels = options["kernels"]
        except (KeyError, TypeError) as info:
            raise TransformationError(
                "A dictionary containing LFRic kernel PSyIR must be passed "
                "into the LFRicAlgInvoke2PSyCallTrans transformation but "
                "this was not found.") from info
        if not isinstance(kernels, dict):
            raise TransformationError(
                f"The value of 'kernels' in the options argument must be a "
                f"dictionary but found '{type(kernels).__name__}'.")
        for kern_call in node.children:
            if isinstance(kern_call, LFRicBuiltinFunctor):
                # Skip builtins as their metadata is stored internally
                continue
            try:
                _ = kernels[id(kern_call)]
            except KeyError as info:
                raise TransformationError(
                    f"The 'kernels' option must be a dictionary containing "
                    f"LFRic kernel PSyIR indexed by the id's of the "
                    f"associated kernel functors, but the id for "
                    f"kernel functor '{kern_call.name}' was not "
                    f"found.") from info

        # Check the algorithm arguments and kernel metadata match.
        self.get_arguments(node, options=options, check_args=True)

    @staticmethod
    def _get_metadata(kernel):
        '''Utility method to extract the kernel metadata from an LFRic
        kernel.

        :param kernel: LFRic kernel PSyIR.
        :type kernel: :py:class:`psyclone.psyir.nodes.Container`

        :returns: LFRic kernel metadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        :raises TransformationError: if the supplied kernel argument \
            is not a PSyIR container.
        :raises TransformationError: if the supplied kernel argument \
            does not contain an LFRicKernelContainer as the root node \
            or first child of the root node.

        '''
        if not isinstance(kernel, Container):
            raise TransformationError(
                f"A PSyIR Container (an LFRic kernel module) was expected, "
                f"but found '{type(kernel).__name__}'.")
        if isinstance(kernel, LFRicKernelContainer):
            kernel_metadata = kernel.metadata
        elif kernel.children and isinstance(
                kernel.children[0], LFRicKernelContainer):
            kernel_metadata = kernel.children[0].metadata
        else:
            raise TransformationError(
                "LFRic kernel PSyIR should contain an "
                "LFRicKernelContainer as the root or first child of the "
                "root but this was not found.")
        return kernel_metadata

    # pylint: disable=too-many-branches
    # pylint: disable=too-many-locals
    def get_arguments(self, node, options=None, check_args=False):
        '''By default this method creates the LFRic processed (lowered)
        argument list from the argument lists of the kernel functors
        within the invoke call and the kernel metadata.

        If the check_args flag is set to True this method does not
        create an argument list, but instead checks that the number of
        arguments expected by the kernel metadata and the number of
        arguments supplied in the algorithm layer match.

        Check args does not create the argument list because a) it is
        faster to not create the list and b) a mismatch in the number
        of expected and actual arguments can cause an index error.

        :param node: an LFRic algorithm invoke call.
        :type node: :py:class:`psyclone.domain.lfric.algorithm.psyir.\
            LFRicAlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param options["kernels"]: this option provides a list of \
            LFRic kernels for this LFRic Algorithm Invoke call.
        :type options["kernels"]: \
            List[:py:class:`psyclone.psyir.nodes.Container`]
        :param bool check_args: if True, checks the number of kernel \
            functor arguments matches the number expected by the kernel \
            metadata. Defaults to False.

        :returns: the processed (lowered) argument list.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

        :raises GenerationError: if the number of arguments in the \
            invoke does not match the expected number of arguments \
            specified by the metadata.

        '''
        # TODO #1618. This routine could/should make use of the new
        # LFRicArgOrder class if and when it makes its way onto trunk.

        const = LFRicConstants()
        # No need to check the lookup of "kernels" in the options
        # dictionary as it has already been validated by the validate()
        # method.
        kernels = options["kernels"]

        # 4 separate lists are used below because the processed
        # (lowered) argument list expects all scalar, field and
        # operator arguments first, then all stencil arguments
        # (separated into size arguments first followed by direction
        # arguments) and finally all qr arguments.

        # The processed (lowered) argument list for scalar, field and
        # operator arguments.
        arguments = []
        # The processed (lowered) argument lists for any stencil
        # arguments (separated into stencil size and direction
        # arguments).
        stencil_size_arguments = []
        stencil_direction_arguments = []
        # The processed (lowered) argument list for any quadrature
        # arguments.
        quad_arguments = []

        # pylint: disable=too-many-nested-blocks
        for kern_call in node.children:
            if isinstance(kern_call, LFRicBuiltinFunctor):
                # metadata is stored in PSyclone builtin classes
                builtin_name = kern_call.name.lower()
                kernel_metadata = BUILTIN_MAP[builtin_name].metadata()
            else:
                # metadata is stored in the user-written kernel
                kernel = kernels[id(kern_call)]
                kernel_metadata = self._get_metadata(kernel)
            arg_idx = 0
            for meta_arg in kernel_metadata.meta_args:
                if not check_args:
                    arg = kern_call.children[arg_idx]
                    self._add_arg(arg, arguments)
                if type(meta_arg) in [
                        FieldArgMetadata, FieldVectorArgMetadata,
                        InterGridArgMetadata, InterGridVectorArgMetadata]:
                    if meta_arg.stencil:
                        arg_idx += 1
                        if not check_args:
                            stencil_arg = kern_call.children[arg_idx]
                            self._add_arg(stencil_arg, stencil_size_arguments)
                        if meta_arg.stencil == "xory1d":
                            # An additional direction stencil argument
                            # is required in the algorithm layer.
                            arg_idx += 1
                            if not check_args:
                                stencil_arg = kern_call.children[arg_idx]
                                if isinstance(stencil_arg, Literal):
                                    raise GenerationError(
                                        f"A literal is not a valid value for "
                                        f"a stencil direction, but found "
                                        f"'{stencil_arg.value}' for field "
                                        f"'{arg.name}'.")
                                if stencil_arg.name.lower() not in [
                                        "x_direction", "y_direction"]:
                                    self._add_arg(
                                        stencil_arg,
                                        stencil_direction_arguments)
                arg_idx += 1
            if kernel_metadata.shapes:
                quad_args = [quad for quad in kernel_metadata.shapes
                             if quad in const.VALID_QUADRATURE_SHAPES]
                for quad_arg in quad_args:
                    # There is an additional quadrature argument.
                    if not check_args:
                        quad_arg = kern_call.children[arg_idx]
                        self._add_arg(quad_arg, quad_arguments)
                    arg_idx += 1

            # Incorrect number of kernel functor arguments
            if check_args and len(kern_call.children) != arg_idx:
                raise GenerationError(
                    f"The invoke kernel functor '{kern_call.name}' has "
                    f"{len(kern_call.children)} arguments, but the kernel "
                    f"metadata expects there to be {arg_idx} arguments.")

        # Add the stencil and quadrature arguments in the order
        # expected in the processed (lowered) argument list. (We
        # expect all scalar, field and operator arguments first, then
        # all stencil arguments (separated into size arguments first
        # followed by direction arguments) and finally all qr
        # arguments).
        arguments.extend(stencil_size_arguments)
        arguments.extend(stencil_direction_arguments)
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
        :param options["kernels"]: this option provides a list of \
            LFRic kernels for this LFRic Algorithm Invoke call.
        :type options["kernels"]: \
            List[:py:class:`psyclone.psyir.nodes.Container`]

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
