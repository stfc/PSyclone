# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""This module provides the KernelInlineTrans transformation."""

from typing import Any, Optional

from psyclone.psyGen import CodedKern, Transformation
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class KernelInlineTrans(Transformation):
    """Mark a PSyKAl kernel for inlining when it is lowered.

    A :class:`psyclone.psyGen.CodedKern` cannot be inlined immediately
    because API-specific lowering may add arguments to its eventual Call.
    This transformation therefore records the request on the kernel. Once
    lowering has constructed the complete Call, the standard
    :class:`psyclone.psyir.transformations.InlineTrans` is applied to it.

    """

    def __str__(self) -> str:
        """
        :returns: a description of this transformation.
        """
        return "Mark a PSyKAl kernel for inlining when it is lowered."

    def validate(self,
                 node: CodedKern,
                 options: Optional[dict[str, Any]] = None,
                 **kwargs) -> None:
        """Validate that the supplied kernel may be marked for inlining.

        Argument-dependent and routine-body validation is deliberately
        deferred until the complete language-level Call is constructed.

        :param node: the kernel to mark for inlining.
        :param options: a deprecated dictionary of transformation options.

        :raises TransformationError: if ``node`` is not a CodedKern.
        :raises TransformationError: if the kernel PSyIR cannot be found.
        :raises TransformationError: if the kernel is polymorphic.
        :raises TransformationError: if the kernel implementation is not in
            the same Container as the call site.

        """
        if not options:
            self.validate_options(**kwargs)

        if not isinstance(node, CodedKern):
            raise TransformationError(
                f"Target of a {self.name} must be a sub-class of "
                f"psyGen.CodedKern but got '{type(node).__name__}'")

    def apply(self,
              node: CodedKern,
              options: Optional[dict[str, Any]] = None,
              **kwargs) -> None:
        """Mark the supplied kernel to be inlined when it is lowered.

        :param node: the kernel to mark for inlining.
        :param options: a deprecated dictionary of transformation options.

        """
        self.validate(node, options=options, **kwargs)
        # pylint: disable=protected-access
        # TODO #2216: This is a temporary solution, longer-term we could
        # inline during transformation time (instead of deferring it) in
        # keep the contents in an InlinedKern node that preserve the metadata
        node._inline = True


__all__ = ["KernelInlineTrans"]
