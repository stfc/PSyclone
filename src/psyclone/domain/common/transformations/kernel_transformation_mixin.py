from typing import Union

from psyclone.psyGen import CodedKern
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class KernelTransformationMixin:
    """ """

    def _check_kernel_is_local(self, node: Union[Node, CodedKern]):
        """ """
        if not isinstance(node, CodedKern):
            return
        rsymbol = node.scope.symbol_table.lookup(node.name, otherwise=None)
        if not rsymbol or rsymbol.is_import or rsymbol.is_unresolved:
            raise TransformationError(
                f"Cannot transform this Kernel call to '{node.name}' "
                f"because it is not module inlined (i.e. local to the current "
                f"module). Use KernelModuleInlineTrans() first."
            )
