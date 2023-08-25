from psyclone.psyGen import Kern
from psyclone.psyir.tools import DependencyTools, DTCode


class LFRicDependencyTools(DependencyTools):
    '''
    '''
    def __init__(self):
        '''
        '''
        self._loop_types_to_parallelise = ["dof", "colour"]

    def _is_loop_suitable_for_parallel(self, loop, _):
        '''
        '''
        return super()._is_loop_suitable_for_parallel(
            loop, only_nested_loops=False)

    def _is_scalar_parallelisable(self, var_info):
        '''
        '''
        # Read only scalar variables can be parallelised
        if var_info.is_read_only():
            return True

        all_accesses = var_info.all_accesses
        if len(all_accesses) == 1:
            # Variable is only used once. If this is as an argument to a
            # Kernel then that is OK so long as it is not a reduction.
            if isinstance(all_accesses[0].node, Kern):
                if all_accesses[0].node.is_reduction:
                    self._add_message(
                        f"Scalar variable '{var_info.var_name}' is "
                        f"only written once.",
                        DTCode.WARN_SCALAR_WRITTEN_ONCE,
                        [f"{var_info.var_name}"])
                    return False
                else:
                    return True

        # Otherwise, we proceed to the method in the superclass.
        return super()._is_scalar_parallelisable(self, var_info)

    def can_loop_be_parallelised(self, loop,
                                 only_nested_loops=True,
                                 test_all_variables=False,
                                 signatures_to_ignore=None):
        '''
        '''
        try:
            return super().can_loop_be_parallelised(
                loop,
                only_nested_loops=only_nested_loops,
                test_all_variables=test_all_variables,
                signatures_to_ignore=signatures_to_ignore)
        except KeyError:
            # LFRic still has symbols that don't exist in the symbol_table
            # until the gen_code() step, so the dependency analysis raises
            # KeyErrors in some cases.
            return True

    def _array_access_parallelisable(self, loop_variables, var_info):
        '''
        :param loop_variables: the list of all loop variables in the code to \
            be parallelised. The first one must be the loop to be \
            parallelised (a possible outer loop does not matter, the value of \
            the loop variable is a constant within the loop to be parallelised.
        :type loop_variables: List[str]
        :param var_info: access information for this variable.
        :type var_info: \
            :py:class:`psyclone.core.SingleVariableAccessInfo`

        :return: whether the variable can be used in parallel.
        :rtype: bool

        '''
        # pylint: disable=too-many-locals
        # If a variable is read-only, it can be parallelised
        if var_info.is_read_only():
            return True

        all_write_accesses = var_info.all_write_accesses

        for write_access in all_write_accesses:
            # We need to compare each write access with any other access,
            # including itself (to detect write-write race conditions:
            # a((i-2)**2) = b(i): i=1 and i=3 would write to a(1))
            for other_access in var_info:
                if not self._is_loop_carried_dependency(loop_variables,
                                                        write_access,
                                                        other_access):
                    # There is a dependency. Try to give precise error
                    # messages:
                    # We need to use default parameters, since otherwise
                    # the value of a variable might be different when
                    # the message is actually evaluated.
                    # Some pylint version complain here (because of the
                    # above). The code is correct, so disable this
                    # message:
                    # pylint: disable=cell-var-from-loop
                    if write_access is other_access:
                        # The write access has a dependency on itself, e.g.
                        # a(3) = ...    or a((i-2)**2) = ...
                        # Both would result in a write-write conflict
                        node = write_access.node
                        self._add_message(LazyString(
                            lambda node=write_access.node:
                                (f"The write access to '"
                                 f"{node.debug_string()}' causes "
                                 f"a write-write race condition.")),
                            DTCode.ERROR_WRITE_WRITE_RACE,
                            [LazyString(lambda node=node:
                                        f"{node.debug_string()}")])
                    else:
                        self._add_message(LazyString(
                            lambda wnode=write_access.node,
                            onode=other_access.node:
                                (f"The write access to "
                                 f"'{wnode.debug_string()}' "
                                 f"and to '{onode.debug_string()}"
                                 f"' are dependent and cannot be "
                                 f"parallelised.")),
                            DTCode.ERROR_DEPENDENCY,
                            [LazyString(lambda wnode=write_access.node:
                                        f"{wnode.debug_string()}"
                                        ),
                             LazyString(lambda onode=other_access.node:
                                        f"{onode.debug_string()}")])

                    return False
        return True
