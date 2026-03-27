# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

"""
This module provides the CalleeTransformationMixin class.

"""

from typing import Union

from psyclone.psyGen import CodedKern
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.container import Container
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class CalleeTransformationMixin:
    """
    A mixin class to be used by all Transformations that act upon Calls or
    PSyKAl Kernels.

    Provides functionality to check that the target of a (Kernel) call has
    been module-inlined before subsequent transformations are applied to it.

    """
    def _check_callee_implementation_is_local(
            self,
            node: Union[Call, CodedKern]) -> None:
        """
        Check that the target of the supplied Kernel or Call node has been
        module inlined.

        :param node: the Call or PSyKAl Kernel to check.

        :raises TransformationError: if the implementation of the target of
            the supplied Kernel or Call has not been module inlined (is not
            present in the current Container).
        :raises TransformationError: if the supplied Kernel/Call is not
            within a Container or the Container does not contain the
            implementation of the Kernel/target routine.

        """
        if isinstance(node, CodedKern):
            rsymbol = node.scope.symbol_table.lookup(node.name, otherwise=None)
            node_name = node.name
            kernel_txt = "Kernel "
        elif isinstance(node, Call):
            rsymbol = node.symbol
            node_name = rsymbol.name
            kernel_txt = ""
        else:
            raise TransformationError(
                f"Attempted to apply {self.name} to '{type(node).__name__}' "
                f"which is not a Call or a CodedKern")

        msg_text = (f"Cannot transform this {kernel_txt}call to '{node_name}' "
                    f"because")

        container = node.ancestor(Container)

        if not container:
            raise TransformationError(
                f"{msg_text} there is no ancestor Container in which "
                f"to look for its implementation."
            )
        names = container.resolve_routine(node_name)
        for name in names:
            rt = container.find_routine_psyir(name, allow_private=True)
            if not rt:
                raise TransformationError(
                    f"{msg_text} Routine '{name}' is not in the same Container"
                    f" ('{container.name}') as the call site. Try using "
                    f"KernelModuleInlineTrans to bring the routine into the "
                    f"same Container first.")

        # (The symbol could have an 'automatic' interface if it is a
        # GenericInterfaceSymbol rather than a RoutineSymbol. If it is in a
        # FileContainer rather than a Container then it can have an Unresolved)
        if not rsymbol:
            raise TransformationError(
                f"{msg_text} cannot find the RoutineSymbol for the target "
                f"routine '{name}'. This means its implementation resides in "
                f"a different source file. Apply KernelModuleInlineTrans first"
                f" to bring it into this module."
            )
        if not (rsymbol.is_modulevar or rsymbol.is_automatic):
            # We permit an import interface if the routine is being imported
            # (unnecessarily) from the ancestor Container.
            if not (rsymbol.is_import and
                    rsymbol.interface.container_symbol.name == container.name):
                raise TransformationError(
                    f"{msg_text} its implementation resides in a different "
                    f"source file. Apply KernelModuleInlineTrans first to "
                    f"bring it into this module."
                )
