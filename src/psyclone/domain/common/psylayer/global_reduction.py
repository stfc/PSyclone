import copy

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import KernelArgument
from psyclone.psyir.nodes import Statement
from psyclone.psyir.nodes.node import Node


class GlobalReduction(Statement):
    '''
    Represents a global-reduction in the PSyIR.

    :raises GenerationError: if distributed memory is not enabled.
    :raises InternalError: if the supplied argument doesn't represent a scalar.

    '''
    # TODO is this really a leaf - it could have operands as children?
    #: Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "GlobalReduction"
    #: The colour to use when creating a view of this node.
    _colour = "cyan"

    def __init__(self, scalar: KernelArgument, parent: Node = None):
        super().__init__(children=[], parent=parent)
        # Check that distributed memory is enabled
        if not Config.get().distributed_memory:
            raise GenerationError(
                f"It makes no sense to create a {self._text_name} object "
                f"when distributed memory is not enabled (dm=False).")

        # Check that the global sum argument is indeed a scalar
        if not scalar.is_scalar:
            raise InternalError(
                f"{self._text_name}.init(): A global reduction argument should"
                f" be a scalar but found argument of type "
                f"'{scalar.argument_type}'.")

        self._scalar = copy.copy(scalar)
        if scalar:
            # Update scalar values appropriately
            # Here "readwrite" denotes how the class GlobalSum
            # accesses/updates a scalar
            self._scalar.access = AccessType.READWRITE
            self._scalar.call = self

    def node_str(self, colour: bool = True) -> str:
        '''
        Returns a text description of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        '''
        return f"{self.coloured_name(colour)}[scalar='{self._scalar.name}']"

    @property
    def scalar(self):
        ''' Return the scalar field that this global reduction acts on '''
        return self._scalar

    @property
    def dag_name(self) -> str:
        '''
        :returns: the name to use in the DAG for this node.
        '''
        return f"{self._text_name}({self._scalar.name})_{self.position}"

    @property
    def args(self):
        ''' Return the list of arguments associated with this node. Override
        the base method and simply return our argument.'''
        return [self._scalar]
