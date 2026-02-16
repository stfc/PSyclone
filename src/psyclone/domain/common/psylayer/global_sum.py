from psyclone.core import AccessType
from psyclone.domain.common.psylayer import GlobalReduction
from psyclone.errors import InternalError
from psyclone.psyGen import KernelArgument
from psyclone.psyir.nodes.node import Node


class GlobalSum(GlobalReduction):
    '''
    Generic GlobalSum class which can be added to a Schedule.

    :param scalar: the scalar that the global sum is stored into
    :param parent: optional parent (default None) of this object

    '''
    _text_name = "GlobalSum"

    def __init__(self, scalar: KernelArgument, parent: Node = None):
        super().__init__(parent=parent)
        # Check that the global sum argument is indeed a scalar
        if not scalar.is_scalar:
            raise InternalError(
                f"{self._text_name}.init(): A global sum argument should be a "
                f"scalar but found argument of type '{scalar.argument_type}'.")

        import copy
        self._scalar = copy.copy(scalar)
        if scalar:
            # Update scalar values appropriately
            # Here "readwrite" denotes how the class GlobalSum
            # accesses/updates a scalar
            self._scalar.access = AccessType.READWRITE
            self._scalar.call = self

    @property
    def scalar(self):
        ''' Return the scalar field that this global sum acts on '''
        return self._scalar

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        return f"globalsum({self._scalar.name})_{self.position}"

    @property
    def args(self):
        ''' Return the list of arguments associated with this node. Override
        the base method and simply return our argument.'''
        return [self._scalar]

    def node_str(self, colour=True):
        '''
        Returns a text description of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return f"{self.coloured_name(colour)}[scalar='{self._scalar.name}']"
