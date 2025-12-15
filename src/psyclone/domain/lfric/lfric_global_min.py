'''
'''

from psyclone.domain.common.psylayer.global_min import GlobalMin
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (Assignment, Call, Node, Reference,
                                  StructureReference)
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    REAL_TYPE, UnresolvedType)


class LFRicGlobalMin(GlobalMin):
    '''
    '''
    def lower_to_language_level(self) -> Node:
        '''
        :returns: this node lowered to language-level PSyIR.

        '''
        # Get the name strings to use
        name = self._operand.name

        symtab = self.ancestor(InvokeSchedule).symbol_table

        # We'll need the LFRic mpi_type.
        mpi_mod = symtab.find_or_create_tag("lfric_mpi_mod",
                                            symbol_type=ContainerSymbol)
        mpi_type = symtab.find_or_create_tag(
            "lfric_mpi_type",
            symbol_type=DataTypeSymbol,
            datatype=UnresolvedType(),
            interface=ImportInterface(mpi_mod))
        mpi_obj = symtab.new_symbol("mpi", symbol_type=DataSymbol,
                                    datatype=mpi_type)
        # Symbol holding the local minimum value.
        loc_min = symtab.lookup(name)

        # Symbol holding the global minimum value.
        result = symtab.new_symbol("glob_min", symbol_type=DataSymbol,
                                   # TODO - get correct type.
                                   datatype=REAL_TYPE)

        # Obtain a suitable mpi object from one of the field arguments.
        for sym in symtab.datasymbols:
            if (isinstance(sym.datatype, DataTypeSymbol) and
                    sym.datatype.name == "field_type"):
                break
        get_mpi = StructureReference.create(sym, ["get_mpi"])
        self.parent.addchild(Assignment.create(lhs=Reference(mpi_obj),
                                               rhs=Call.create(get_mpi)),
                             index=0)

        # Call the method to compute the global min.
        sref = StructureReference.create(mpi_obj, ["global_min"])
        call = Call.create(sref, [Reference(loc_min), Reference(result)])
        call.preceding_comment = "Perform global min"
        self.parent.addchild(call, self.position)
        assign = Assignment.create(lhs=Reference(loc_min),
                                   rhs=Reference(result))
        return self.replace_with(assign)
