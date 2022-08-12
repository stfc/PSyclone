import logging

from psyclone.psyad import AdjointVisitor
from psyclone.psyad.domain.common import create_adjoint_name, find_container
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import RoutineSymbol


def generate_lfric_adjoint(tl_psyir, active_variables):
    '''Takes an LFRic tangent-linear kernel represented in language-level PSyIR
    and returns its adjoint represented in language-level PSyIR.

    :param tl_psyir: language-level PSyIR containing the LFRic \
        tangent-linear kernel.
    :type tl_psyir: :py:class:`psyclone.psyir.Node`
    :param list of str active_variables: list of active variable names.

    :returns: language-level PSyIR containing the adjoint of the \
        supplied tangent-linear kernel.
    :rtype: :py:class:`psyclone.psyir.Node`

    :raises InternalError: if the PSyIR does not have a Container.
    :raises InternalError: if the PSyIR does not contain any Routines.

    '''
    logger = logging.getLogger(__name__)

    # Translate from TL to AD
    logger.debug("Translating from TL to AD.")
    adjoint_visitor = AdjointVisitor(active_variables)
    ad_psyir = adjoint_visitor(tl_psyir)

    container = find_container(ad_psyir)
    if not container:
        raise InternalError(
            "An LFRic kernel must be within a Container but the supplied "
            "PSyIR does not contain one.")

    # Re-name the Container for the adjoint code. Use the symbol table
    # for the existing TL code so that we don't accidentally clash with
    # e.g. the name of the kernel routine.
    container.name = container.symbol_table.next_available_name(
        create_adjoint_name(container.name))

    routines = ad_psyir.walk(Routine)

    if not routines:
        raise InternalError("The supplied PSyIR does not contain any "
                            "routines.")

    # TODO issue #1800 if there are multiple
    # modules/subroutines/program implementations in the file then
    # raise an exception unless a particular name is specified (by
    # e.g. command line -kernel_name=xyz. The name can be for a routine
    # or an interface.)

    # Until we can query the kernel metadata to see whether it points
    # to a kernel or an interface (issue #1807), we simply assume that we
    # should allow multiple routines as they imply an interface. We
    # further assume that the implementation of the routines in the
    # interface use the same variable names which allows us to
    # continue to use a single command line list of active
    # variables. This is the case for the implementations we care
    # about but in general may not be the case. Issue #1595 should
    # help fix this problem as it would only be arguments that would
    # need to have the same names.

    for routine in routines:

        # We need to re-name the kernel routine.
        kernel_sym = container.symbol_table.lookup(routine.name)
        adj_kernel_name = create_adjoint_name(routine.name)
        # A symbol's name is immutable so create a new RoutineSymbol
        adj_kernel_sym = container.symbol_table.new_symbol(
            adj_kernel_name, symbol_type=RoutineSymbol,
            visibility=kernel_sym.visibility)
        container.symbol_table.remove(kernel_sym)
        routine.name = adj_kernel_sym.name

        logger.debug("AD LFRic kernel will be named '%s'", routine.name)

    return ad_psyir
