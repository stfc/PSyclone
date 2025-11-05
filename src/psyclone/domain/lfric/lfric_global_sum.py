from psyclone.domain.common.psylayer.global_sum import GlobalSum
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (Assignment, Call, Reference,
                                  StructureReference)
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    UnresolvedType)


class LFRicGlobalSum(GlobalSum):
    '''
    '''
    def __init__(self, operand, parent=None):
        # Check that the global sum argument is indeed a scalar
        if not operand.is_scalar:
            raise InternalError(
                f"LFRicGlobalSum.init(): A global reduction argument "
                f"should be a scalar but found argument of type "
                f"'{operand.argument_type}'.")
        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if operand.intrinsic_type != "real":
            raise GenerationError(
                f"LFRicGlobalSum currently only supports real scalars, "
                f"but argument '{operand.name}' in Kernel "
                f"'{operand.call.name}' has '{operand.intrinsic_type}' "
                f"intrinsic type.")
        # Initialise the parent class
        super().__init__(operand, parent=parent)

    def lower_to_language_level(self):
        '''
        :returns: this node lowered to language-level PSyIR.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''

        # Get the name strings to use
        name = self._operand.name
        type_name = self._operand.data_type
        mod_name = self._operand.module_name

        # Get the symbols from the given names
        symtab = self.ancestor(InvokeSchedule).symbol_table
        sum_mod = symtab.find_or_create(mod_name, symbol_type=ContainerSymbol)
        sum_type = symtab.find_or_create(type_name,
                                         symbol_type=DataTypeSymbol,
                                         datatype=UnresolvedType(),
                                         interface=ImportInterface(sum_mod))
        sum_name = symtab.find_or_create_tag("global_sum",
                                             symbol_type=DataSymbol,
                                             datatype=sum_type)
        tmp_var = symtab.lookup(name)

        # Create the assignments
        assign1 = Assignment.create(
            lhs=StructureReference.create(sum_name, ["value"]),
            rhs=Reference(tmp_var)
        )
        assign1.preceding_comment = "Perform global sum"
        self.parent.addchild(assign1, self.position)
        assign2 = Assignment.create(
            lhs=Reference(tmp_var),
            rhs=Call.create(StructureReference.create(sum_name, ["get_sum"]))
        )
        return self.replace_with(assign2)
