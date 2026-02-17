from psyclone.domain.common.psylayer import GlobalReduction
from psyclone.errors import GenerationError
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (
    Assignment, Call, Reference, StructureReference)
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    UnresolvedType)


class LFRicGlobalMin(GlobalReduction):
    '''
    LFRic specific global min class which can be added to and
    manipulated in a schedule.

    :param scalar: the kernel argument for which to perform a global min.
    :type scalar: :py:class:`psyclone.lfric.LFRicKernelArgument`
    :param parent: the parent node of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises InternalError: if the supplied argument is not a scalar.
    :raises GenerationError: if the scalar is not of "real" intrinsic type.

    '''
    _text_name = "LFRicGlobalMin"

    def __init__(self, scalar, parent=None):
        # Initialise the parent class
        super().__init__(scalar, parent=parent)

        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if scalar.intrinsic_type not in ["real", "integer"]:
            raise GenerationError(
                f"LFRicGlobalMin currently only supports real or integer "
                f"scalars, but argument '{scalar.name}' in Kernel "
                f"'{scalar.call.name}' has "
                f"'{scalar.intrinsic_type}' intrinsic type.")

    def lower_to_language_level(self) -> Node:
        '''
        :returns: this node lowered to language-level PSyIR.
        '''

        # Get the name strings to use
        name = self._scalar.name
        type_name = self._scalar.data_type
        mod_name = self._scalar.module_name

        # Get the symbols from the given names
        symtab = self.ancestor(InvokeSchedule).symbol_table
        # The Container from which to import the scalar type.
        sum_mod = symtab.find_or_create(mod_name, symbol_type=ContainerSymbol)
        # The scalar type.
        scal_type = symtab.find_or_create(type_name,
                                          symbol_type=DataTypeSymbol,
                                          datatype=UnresolvedType(),
                                          interface=ImportInterface(sum_mod))
        # An instance of scalar type that we will use to get the global min.
        sum_name = symtab.find_or_create_tag("global_min",
                                             symbol_type=DataSymbol,
                                             datatype=scal_type)
        tmp_var = symtab.lookup(name)

        # Assign the value of the local scalar to the new scalar_type quantity
        assign1 = Assignment.create(
            lhs=StructureReference.create(sum_name, ["value"]),
            rhs=Reference(tmp_var)
        )
        assign1.preceding_comment = "Obtain global min"
        self.parent.addchild(assign1, self.position)
        # Use the 'get_min' method to compute the global min.
        assign2 = Assignment.create(
            lhs=Reference(tmp_var),
            rhs=Call.create(StructureReference.create(sum_name, ["get_min"]))
        )
        return self.replace_with(assign2)
