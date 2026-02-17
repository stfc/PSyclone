from psyclone.domain.common.psylayer import GlobalReduction
from psyclone.errors import GenerationError
from psyclone.lfric import LFRicKernelArgument
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (
    Assignment, Call, Reference, StructureReference)
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    UnresolvedType)


class LFRicGlobalMax(GlobalReduction):
    '''
    LFRic specific global max class which can be added to and
    manipulated in a schedule.

    :param scalar: the kernel argument for which to perform a global min.
    :param parent: the parent node of this node in the PSyIR.

    :raises InternalError: if the supplied argument is not a scalar.
    :raises GenerationError: if the scalar is not of "real" intrinsic type.

    '''
    _text_name = "LFRicGlobalMax"

    def __init__(self, scalar: LFRicKernelArgument, parent: Node = None):
        # Initialise the parent class
        super().__init__(scalar, parent=parent)

        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if scalar.intrinsic_type != "real":
            raise GenerationError(
                f"LFRicGlobalMax currently only supports real scalars, but "
                f"argument '{scalar.name}' in Kernel '{scalar.call.name}' has "
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
        sum_mod = symtab.find_or_create(mod_name, symbol_type=ContainerSymbol)
        sum_type = symtab.find_or_create(type_name,
                                         symbol_type=DataTypeSymbol,
                                         datatype=UnresolvedType(),
                                         interface=ImportInterface(sum_mod))
        sum_name = symtab.find_or_create_tag("global_max",
                                             symbol_type=DataSymbol,
                                             datatype=sum_type)
        tmp_var = symtab.lookup(name)

        # Create the assignments
        assign1 = Assignment.create(
            lhs=StructureReference.create(sum_name, ["value"]),
            rhs=Reference(tmp_var)
        )
        assign1.preceding_comment = "Perform global max"
        self.parent.addchild(assign1, self.position)
        assign2 = Assignment.create(
            lhs=Reference(tmp_var),
            rhs=Call.create(StructureReference.create(sum_name, ["get_max"]))
        )
        return self.replace_with(assign2)
