import logging

from psyclone.psyad import AdjointVisitor
from psyclone.psyad.domain.common import create_adjoint_name, find_container
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import RoutineSymbol


def generate_lfric_adjoint(tl_psyir, active_variables):
    '''Takes an LFRic tangent-linear kernel represented in language-level PSyIR
    and returns its adjoint represented in language-level PSyIR.

    Currently just takes a copy of the supplied PSyIR and re-names the
    Container (if there is one) and Routine nodes.

    :param tl_psyir: language-level PSyIR containing the LFRic \
        tangent-linear kernel.
    :type tl_psyir: :py:class:`psyclone.psyir.Node`
    :param list of str active_variables: list of active variable names.

    :returns: language-level PSyIR containing the adjoint of the \
        supplied tangent-linear kernel.
    :rtype: :py:class:`psyclone.psyir.Node`

    :raises InternalError: if the PSyIR does not contain any Routines.
    :raises NotImplementedError: if the PSyIR contains >1 Routine.

    '''
    logger = logging.getLogger(__name__)

    # Translate from TL to AD
    logger.debug("Translating from TL to AD.")
    adjoint_visitor = AdjointVisitor(active_variables)
    ad_psyir = adjoint_visitor(tl_psyir)

    # We permit the input code to be a single Program or Subroutine
    container = find_container(ad_psyir)
    if container:
        # Re-name the Container for the adjoint code. Use the symbol table
        # for the existing TL code so that we don't accidentally clash with
        # e.g. the name of the kernel routine.
        container.name = container.symbol_table.next_available_name(
            create_adjoint_name(container.name))

    routines = ad_psyir.walk(Routine)

    if not routines:
        raise InternalError("The supplied PSyIR does not contain any "
                            "routines.")

    # TODO issue #1782 if this is an LFRic-specific implementation and
    # the metadata code points to an interface then adjoint all
    # routines specified in the interface. If it points to a routine
    # then only translate that routine. If this is a generic
    # implementation then only support one routine as we don't know
    # which one to support.

    # TODO issue #1800 if there are multiple
    # modules/subroutines/program implementations in the file then
    # raise an exception unless a particular name is specified (by
    # e.g. command line -kernel_name=xyz. The name can be for a routine
    # or an interface.)

    # Until we know whether this is meant to be a generic or
    # LFRic-specific kernel (issue #1782) and, for LFRic, can specify
    # the particular kernel metadata if multiple versions exist and
    # then read kernel metadata to determine whether it points to a
    # kernel or interface (issue #1807), we simply assume that we
    # should allow multiple routines as they imply an interface. We
    # further assume that the implementation of the routines in the
    # interface use the same variable names which allows us to
    # continue to use a single command line list of active
    # variables. This is the case for the implementations we care
    # about but in general may not be the case. Issue #1595 should
    # help fix this problem as it would only be arguments that would
    # need to have the same names.

    for routine in routines:

        # We need to re-name the kernel routines. We have to take care
        # in case we've been supplied with a bare program/subroutine
        # rather than one or more subroutines within a module (which
        # we will get with LFRic mixed precision kernels).
        if container:
            kernel_sym = container.symbol_table.lookup(routine.name)
            adj_kernel_name = _create_adjoint_name(routine.name)
            # A symbol's name is immutable so create a new RoutineSymbol
            adj_kernel_sym = container.symbol_table.new_symbol(
                adj_kernel_name, symbol_type=RoutineSymbol,
                visibility=kernel_sym.visibility)
            container.symbol_table.remove(kernel_sym)
            routine.name = adj_kernel_sym.name
        else:
            routine.name = routine.symbol_table.next_available_name(
                _create_adjoint_name(routine.name))

        logger.debug("AD kernel will be named '%s'", routine.name)

    return ad_psyir
