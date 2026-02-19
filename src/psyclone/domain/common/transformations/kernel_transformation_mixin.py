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
This module provides the KernelTransformationMixin class.

"""

from typing import Union

from psyclone.psyGen import CodedKern
from psyclone.psyir.nodes.container import Container
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class KernelTransformationMixin:
    """
    A mixin class to be used by all Transformations that act upon PSyKAl
    Kernels.

    Provides functionality to check that a Kernel has been module-inlined
    before subsequent transformations are applied to it.

    """
    def _check_kernel_is_local(self, node: Union[Node, CodedKern]) -> None:
        """
        Check that the supplied kernel node has been module inlined.

        :param node: the PSyKAl Kernel to check.

        :raises TransformationError: if the supplied Kernel has not been
            module inlined (is not present in the current Container).
        :raises TransformationError: if the supplied Kernel (call) is not
            within a Container or the Container does not contain the
            implementation of the Kernel.

        """
        if not isinstance(node, CodedKern):
            return

        msg_text = (f"Cannot transform this Kernel call to '{node.name}' "
                    f"because")

        rsymbol = node.scope.symbol_table.lookup(node.name, otherwise=None)
        if not rsymbol or not rsymbol.is_modulevar:
            raise TransformationError(
                f"{msg_text} its implementation resides in a different source "
                f"file. Apply KernelModuleInlineTrans first to bring it into "
                f"this module."
            )
        container = node.ancestor(Container)
        if not container:
            raise TransformationError(
                f"{msg_text} because there is no ancestor Container in which "
                f"to look for its implementation."
            )
        names = container.resolve_routine(node.name)
        for name in names:
            rt = container.find_routine_psyir(name, allow_private=True)
            if not rt:
                raise TransformationError(
                    f"{msg_text} the ancestor Container does not contain "
                    f"a Routine named '{name}'"
                )
