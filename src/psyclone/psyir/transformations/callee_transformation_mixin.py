# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
        if not names:
            raise TransformationError(
                f"{msg_text} no routine or interface matching this "
                f"name could be found in the same Container as the "
                f"call site. Try using KernelModuleInlineTrans to bring the "
                f"routine into the same Container first.")
        for name in names:
            rt = container.find_routine_psyir(name, allow_private=True)
            if not rt:
                raise TransformationError(
                    f"{msg_text} Routine '{name}' is not in the same Container"
                    f" ('{container.name}') as the call site. Try using "
                    f"KernelModuleInlineTrans to bring the routine into the "
                    f"same Container first.")
