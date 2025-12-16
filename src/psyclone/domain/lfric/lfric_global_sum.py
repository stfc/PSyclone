from psyclone.domain.common.psylayer.global_sum import GlobalSum
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (Assignment, Call, Node, Reference,
                                  StructureReference)
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    UnresolvedType)


class LFRicGlobalSum(GlobalSum):
    '''
    Represents a global sum in the LFRic DSL.

    '''
    def lower_to_language_level(self) -> Node:
        '''
        :returns: this node lowered to language-level PSyIR.

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
