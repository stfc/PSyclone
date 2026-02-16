from psyclone.configuration import Config
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import Statement
from psyclone.psyir.nodes.node import Node


class GlobalReduction(Statement):
    '''
    Represents a global-reduction in the PSyIR.

    :raises GenerationError: if distributed memory is not enabled.

    '''
    # TODO is this really a leaf - it could have operands as children?
    #: Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "GlobalReduction"
    #: The colour to use when creating a view of this node.
    _colour = "cyan"

    def __init__(self, parent: Node = None):
        super().__init__(children=[], parent=parent)
        # Check that distributed memory is enabled
        if not Config.get().distributed_memory:
            raise GenerationError(
                f"It makes no sense to create a {self._text_name} object "
                f"when distributed memory is not enabled (dm=False).")
