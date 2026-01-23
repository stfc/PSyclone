from typing import Union

from psyclone.psyGen import CodedKern
from psyclone.psyir.nodes.container import Container
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class KernelTransformationMixin:
    """ """

    def _check_kernel_is_local(self, node: Union[Node, CodedKern]):
        """ """
        if not isinstance(node, CodedKern):
            return

        msg_text = (f"Cannot transform this Kernel call to '{node.name}' "
                    f"because")

        rsymbol = node.scope.symbol_table.lookup(node.name, otherwise=None)
        if not rsymbol or rsymbol.is_import or rsymbol.is_unresolved:
            raise TransformationError(
                f"{msg_text} it is not module inlined (i.e. local to the "
                f"current module). Use KernelModuleInlineTrans() first."
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
