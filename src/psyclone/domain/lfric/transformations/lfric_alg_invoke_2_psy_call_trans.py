# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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
    InterGridArgMetadata, InterGridVectorArgMetadata,
    LFRicKernelContainer, LFRicKernelMetadata, ScalarArgMetadata)
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import (
    Literal, Reference, ArrayReference, Container, CodeBlock)


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
        :param options["kernels"]: this option provides a list of \
            LFRic kernels for this LFRic Algorithm Invoke call.
        :type options["kernels"]: \
            List[:py:class:`psyclone.psyir.nodes.Container`]

        :raises TransformationError: if the supplied call argument is \
            not a PSyIR AlgorithmInvokeCall node.
        :raises TransformationError: if kernels options are not provided.

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
                "Kernels metadata must be passed into the "
                "LFRicAlgInvoke2PSyCallTrans transformation but this was not "
                "found.") from info
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
                    f"Kernels metadata must be a dictionary indexed by the "
                    f"id's of the associated kernel functors, but the id for "
                    f"kernel functor '{kern_call.name}' was not "
                    f"found.") from info

        # Check the algorithm arguments and kernel metadata match.
        self.get_arguments(node, options=options, check_args=True)

    @staticmethod
    def _get_metadata(kernel):
        '''Utility method to extract the kernel metadata from an LFRic
        kernel.

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

    @staticmethod
    def _add_arg(arg, arguments):
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
        elif isinstance(arg, CodeBlock):
            arguments.append(arg.copy())
        else:
            raise InternalError(
                f"Expected Algorithm-layer kernel arguments to be "
                f"a literal, reference or array reference, but "
                f"found '{type(arg).__name__}'.")

    def get_arguments(self, node, options=None, check_args=False):
        '''Creates the LFRic processed (lowered) argument list from the
        argument lists of the kernel functors within the invoke call
        and the kernel metadata.

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

        :raises GenerationError: if the number of arguments in the \
            invoke does not match the expected number of arguments \
            specified by the metadata.

        '''
        # TODO #1618. This routine could/should make use of the new
        # LFRicArgOrder class if and when it makes its way onto trunk.

        const = LFRicConstants()
        kernels = options["kernels"]

        arguments = []
        quad_arguments = []
        stencil_arguments1 = []
        stencil_arguments2 = []

        # TODO #1618 builtin metadata should be properly stored within
        # PSyclone. For the moment add the required ones to run
        # PSyclone tests and examples.
        builtin_metadata = {}
        # setval_c metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata("gh_real", "gh_write", "any_space_1"),
                       ScalarArgMetadata("gh_real", "gh_read")],
            procedure_name="setval_c_code",
            name="setval_c")
        builtin_metadata["setval_c"] = metadata
        # x_innerproduct_x metadata
        # TODO #1618 Incorrect metadata due to bug: should be
        # ScalarArgMetadata("gh_real", "gh_sum"),
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[ScalarArgMetadata("gh_real", "gh_read"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="x_innerproduct_x_code",
            name="x_innerproduct_x")
        builtin_metadata["x_innerproduct_x"] = metadata
        # x_minus_y metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata("gh_real", "gh_write", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="x_minus_y_code",
            name="x_minus_y")
        builtin_metadata["x_minus_y"] = metadata
        # setval_x
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata("gh_real", "gh_write", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="setval_x_code",
            name="setval_x")
        builtin_metadata["setval_x"] = metadata
        # x_innerproduct_y metadata
        # TODO #1618 Incorrect metadata due to bug: should be
        # ScalarArgMetadata("gh_real", "gh_sum"),
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[ScalarArgMetadata("gh_real", "gh_read"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="x_innerproduct_y_code",
            name="x_innerproduct_y")
        builtin_metadata["x_innerproduct_y"] = metadata
        # x_minus_by metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata("gh_real", "gh_write", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1"),
                       ScalarArgMetadata("gh_real", "gh_read"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="x_minus_by_code",
            name="x_minus_by")
        builtin_metadata["x_minus_by"] = metadata
        # inc_ax_plus_y metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[ScalarArgMetadata("gh_real", "gh_read"),
                       FieldArgMetadata(
                           "gh_real", "gh_readwrite", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="inc_ax_plus_y_code",
            name="inc_ax_plus_y")
        builtin_metadata["inc_ax_plus_y"] = metadata
        # inc_x_plus_by metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata(
                "gh_real", "gh_readwrite", "any_space_1"),
                       ScalarArgMetadata("gh_real", "gh_read"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="inc_x_plus_by_code",
            name="inc_x_plus_by")
        builtin_metadata["inc_x_plus_by"] = metadata
        # inc_x_minus_by metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata(
                "gh_real", "gh_readwrite", "any_space_1"),
                       ScalarArgMetadata("gh_real", "gh_read"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="inc_x_minus_by_code",
            name="inc_x_minus_by")
        builtin_metadata["inc_x_minus_by"] = metadata
        # x_divideby_y metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata("gh_real", "gh_write", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="x_divideby_y_code",
            name="x_divideby_y")
        builtin_metadata["x_divideby_y"] = metadata
        # inc_x_divideby_y metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata(
                "gh_real", "gh_readwrite", "any_space_1"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="inc_x_divideby_y_code",
            name="inc_x_divideby_y")
        builtin_metadata["inc_x_divideby_y"] = metadata
        # a_times_x metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[FieldArgMetadata("gh_real", "gh_write", "any_space_1"),
                       ScalarArgMetadata("gh_real", "gh_read"),
                       FieldArgMetadata("gh_real", "gh_read", "any_space_1")],
            procedure_name="a_times_x_code",
            name="a_times_x")
        builtin_metadata["a_times_x"] = metadata
        # inc_a_times_x metadata
        metadata = LFRicKernelMetadata(
            operates_on="dof",
            meta_args=[ScalarArgMetadata("gh_real", "gh_read"),
                       FieldArgMetadata(
                           "gh_real", "gh_readwrite", "any_space_1")],
            procedure_name="inc_a_times_x_code",
            name="inc_a_times_x")
        builtin_metadata["inc_a_times_x"] = metadata

        for kern_call in node.children:
            if isinstance(kern_call, LFRicBuiltinFunctor):
                kernel_metadata = builtin_metadata[kern_call.name.lower()]
            else:
                kernel = kernels[id(kern_call)]
                kernel_metadata = self._get_metadata(kernel)
            arg_idx = 0
            for meta_arg in kernel_metadata.meta_args:
                if not check_args:
                    arg = kern_call.children[arg_idx]
                    self._add_arg(arg, arguments)
                # If this is a stencil arg then skip any additional
                # arguments.
                if type(meta_arg) in [
                        FieldArgMetadata, FieldVectorArgMetadata,
                        InterGridArgMetadata, InterGridVectorArgMetadata]:
                    if meta_arg.stencil:
                        arg_idx += 1
                        if not check_args:
                            stencil_arg = kern_call.children[arg_idx]
                            self._add_arg(stencil_arg, stencil_arguments1)
                        if meta_arg.stencil == "xory1d":
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
                                        stencil_arg, stencil_arguments2)
                arg_idx += 1
            if kernel_metadata.shapes and \
               [quad for quad in kernel_metadata.shapes
                    if quad in const.VALID_QUADRATURE_SHAPES]:
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

        arguments.extend(stencil_arguments1)
        arguments.extend(stencil_arguments2)
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
