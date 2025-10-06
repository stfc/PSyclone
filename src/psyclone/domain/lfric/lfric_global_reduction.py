from psyclone.configuration import Config
from psyclone.domain.common.psylayer.global_reduction import GlobalReduction
from psyclone.errors import GenerationError, InternalError


class LFRicGlobalReduction(GlobalReduction):
    '''
    LFRic specific global sum class which can be added to and
    manipulated in a schedule.

    :param scalar: the kernel argument for which to perform a global sum.
    :type scalar: :py:class:`psyclone.lfric.LFRicKernelArgument`
    :param parent: the parent node of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises GenerationError: if distributed memory is not enabled.
    :raises InternalError: if the supplied argument is not a scalar.
    :raises GenerationError: if the scalar is not of "real" intrinsic type.

    '''
    def __init__(self, operation, scalar, parent=None):
        # Check that distributed memory is enabled
        if not Config.get().distributed_memory:
            raise GenerationError(
                "It makes no sense to create an LFRicGlobalSum object when "
                "distributed memory is not enabled (dm=False).")
        # Check that the global sum argument is indeed a scalar
        if not scalar.is_scalar:
            raise InternalError(
                f"LFRicGlobalSum.init(): A global sum argument should be a "
                f"scalar but found argument of type '{scalar.argument_type}'.")
        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if scalar.intrinsic_type != "real":
            raise GenerationError(
                f"LFRicGlobalSum currently only supports real scalars, but "
                f"argument '{scalar.name}' in Kernel '{scalar.call.name}' has "
                f"'{scalar.intrinsic_type}' intrinsic type.")
        # Initialise the parent class
        super().__init__(operation, scalar, parent=parent)

    def lower_to_language_level(self):
        '''
        :returns: this node lowered to language-level PSyIR.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
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
