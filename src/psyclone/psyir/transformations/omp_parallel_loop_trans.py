# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


import logging
from typing import Iterable
from psyclone.psyir.nodes import (
    OMPParallelDoDirective, OMPReductionClause, Loop)
from psyclone.psyir.nodes.omp_directives import MAP_REDUCTION_OP_TO_OMP
from psyclone.psyir.transformations.omp_loop_trans import OMPLoopTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper(inherit=False)
class OMPParallelLoopTrans(OMPLoopTrans):
    ''' Adds an OpenMP PARALLEL DO directive to a loop. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> psyir = FortranReader().psyir_from_source("""
    ... program do_loop
    ...     real, dimension(10) :: A
    ...     integer i
    ...     do i = 1, 10
    ...       A(i) = i
    ...     end do
    ... end program do_loop
    ... """)
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.transformations import OMPParallelLoopTrans
    >>> trans = OMPParallelLoopTrans()
    >>> trans.apply(psyir.walk(Loop)[0])
    >>> print(FortranWriter()(psyir))
    program do_loop
      real, dimension(10) :: a
      integer :: i
    <BLANKLINE>
      !$omp parallel do default(shared) private(i) schedule(auto)
      do i = 1, 10, 1
        a(i) = i
      enddo
      !$omp end parallel do
    <BLANKLINE>
    end program do_loop
    <BLANKLINE>

    '''

    def __str__(self):
        return "Add an 'OpenMP PARALLEL DO' directive"

    def apply(self, node: Loop,
              force_private: Iterable[str] = tuple(),
              enable_reductions: bool = False,
              options=None, **kwargs):
        ''' Apply an OMPParallelLoop Transformation to the supplied node
        (which must be a Loop). In the generated code this corresponds to
        wrapping the Loop with directives:

        .. code-block:: fortran

          !$OMP PARALLEL DO ...
          do ...
            ...
          end do
          !$OMP END PARALLEL DO

        :param node: the node (loop) to which to apply the transformation.
        :param force_private: specify a list of symbol names
            explicitly requested to be private.
        :param enable_reductions: whether to enable PSyclone to compute
            reduction clauses on the parallelised loop.
        :param options: a dictionary with options for transformations
            and validation.
        :type options: Optional[Dict[str, Any]]
        '''
        logger = logging.getLogger(__name__)
        # TODO #2668 - deprecate options dictionary.
        local_options = options.copy() if options is not None else None
        reduction_ops = []
        if options:
            enable_reductions = options.get("enable_reductions", False)
            if enable_reductions:
                local_options["reduction_ops"] = \
                    list(MAP_REDUCTION_OP_TO_OMP.keys())
        else:
            if enable_reductions:
                # Reduction_ops isn't a supported option provided in this
                # Transformation's docstring, however since its in the
                # options for its superclass we give a warning and override
                # it as needed.
                if "reduction_ops" in kwargs:
                    del kwargs["reduction_ops"]
                    logger.warning(
                        f"{self.name} overrides the provided reduction_ops "
                        f"keyword argument to those supported by PSyclone."
                    )
                reduction_ops = list(MAP_REDUCTION_OP_TO_OMP.keys())

        # reduction_ops is the argument used by the superclass to determine
        # whether to allow reductions, so we don't pass enable_reductions.
        self.validate(node, options=local_options, force_private=force_private,
                      reduction_ops=reduction_ops, **kwargs)

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # add our OpenMP loop directive setting its parent to the node's
        # parent and its children to the node
        directive = OMPParallelDoDirective(children=[node.detach()],
                                           omp_schedule=self.omp_schedule)

        # add any inferred reduction clauses to the newly introduced directive
        for (op, ref) in self.inferred_reduction_clauses:
            clause = OMPReductionClause(MAP_REDUCTION_OP_TO_OMP[op])
            clause.addchild(ref)
            directive.addchild(clause)

        # add the OpenMP loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)

        # Add explicit private variables
        explicitly_private_symbols = set()
        for symbol_name in force_private:
            try:
                sym = node.scope.symbol_table.lookup(symbol_name)
                explicitly_private_symbols.add(sym)
            except KeyError:
                # This is not an error, but we will log the missed string
                logger.warning(
                    "%s has been provided with the '%s' symbol name in "
                    "the 'force_private' option, but there is no such "
                    "symbol in this scope.", self.name, symbol_name)
        directive.explicitly_private_symbols.update(
            explicitly_private_symbols)


# For Sphinx AutoAPI documentation generation
__all__ = ["OMPParallelLoopTrans"]
