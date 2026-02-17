import copy

from psyclone.core import AccessType
from psyclone.domain.common.psylayer.global_reduction import GlobalReduction
from psyclone.errors import InternalError
from psyclone.psyGen import KernelArgument
from psyclone.psyir.nodes.node import Node


class GlobalMax(GlobalReduction):
    '''
    Generic GlobalMax class which can be added to a Schedule.

    :param scalar: the scalar that the global maximum is computed for and
        the result stored into.
    :param parent: optional parent (default None) of this object

    '''
    _text_name = "GlobalMax"

    def __init__(self, scalar: KernelArgument, parent: Node = None):
        super().__init__(parent=parent)
        # Check that the argument is indeed a scalar
        if not scalar.is_scalar:
            raise InternalError(
                f"{self._text_name}.init(): A global max argument should be a "
                f"scalar but found argument of type '{scalar.argument_type}'.")

        self._scalar = copy.copy(scalar)
        if scalar:
            # Update scalar values appropriately
            # Here "readwrite" denotes how the class GlobalMax
            # accesses/updates a scalar
            self._scalar.access = AccessType.READWRITE
            self._scalar.call = self

    @property
    def scalar(self):
        ''' Return the scalar quantity that this global reduction acts on '''
        return self._scalar

    @property
    def dag_name(self) -> str:
        '''
        :returns: the name to use in the DAG for this node.
        '''
        return f"globalmax({self._scalar.name})_{self.position}"

    @property
    def args(self):
        ''' Return the list of arguments associated with this node. Override
        the base method and simply return our argument.'''
        return [self._scalar]
