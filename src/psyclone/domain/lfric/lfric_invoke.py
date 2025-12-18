# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module implements the LFRic-specific implementation of the Invoke
    base class from psyGen.py. '''

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.errors import GenerationError, FieldNotFoundError
from psyclone.psyGen import Invoke
from psyclone.psyir.nodes import Assignment, Reference, Call, Literal
from psyclone.psyir.symbols import (
    ContainerSymbol, RoutineSymbol, ImportInterface, DataSymbol, INTEGER_TYPE)


class LFRicInvoke(Invoke):
    '''
    The LFRic-specific Invoke class. This passes the LFRic-specific
    InvokeSchedule class to the base class so it creates the one we
    require.

    :param alg_invocation: object containing the invoke call information.
    :type alg_invocation: :py:class:`psyclone.parse.algorithm.InvokeCall`
    :param int idx: the position of the invoke in the list of invokes
                    contained in the Algorithm.
    :param invokes: the Invokes object containing this LFRicInvoke
                    object.
    :type invokes: :py:class:`psyclone.domain.lfric.LFRicInvokes`

    :raises GenerationError: if integer reductions are required in the
                    PSy-layer.

    '''
    # pylint: disable=too-many-instance-attributes
    # pylint: disable=too-many-locals
    def __init__(self, alg_invocation, idx, invokes):
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric import LFRicInvokeSchedule
        const = LFRicConstants()
        Invoke.__init__(self, alg_invocation, idx, LFRicInvokeSchedule,
                        invokes)

        # The base class works out the algorithm code's unique argument
        # list and stores it in the 'self._alg_unique_args'
        # list. However, the base class currently ignores any stencil,
        # quadrature and halo-depth arguments so we need to add them in.

        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.lfric import (LFRicFunctionSpaces, LFRicGlobalSum,
                                    LFRicLMAOperators,
                                    LFRicReferenceElement,
                                    LFRicCMAOperators, LFRicBasisFunctions,
                                    LFRicMeshes, LFRicBoundaryConditions,
                                    LFRicProxies, LFRicMeshProperties)
        from psyclone.domain.lfric import (
            LFRicCellIterators, LFRicHaloDepths, LFRicLoopBounds,
            LFRicRunTimeChecks,
            LFRicScalarArgs, LFRicFields, LFRicDofmaps, LFRicStencils)

        self.scalar_args = LFRicScalarArgs(self)

        # Initialise our Invoke stencil information
        self.stencil = LFRicStencils(self)

        # Initialise our information on the function spaces used by this Invoke
        self.function_spaces = LFRicFunctionSpaces(self)

        # Initialise the object holding all information on the dofmaps
        # required by this Invoke
        self.dofmaps = LFRicDofmaps(self)

        # Initialise information on all of the fields accessed in this Invoke
        self.fields = LFRicFields(self)

        # Initialise info on all of the LMA operators used in this Invoke
        self.lma_ops = LFRicLMAOperators(self)

        # Initialise the object holding all information on the column-
        # -matrix assembly operators required by this Invoke
        self.cma_ops = LFRicCMAOperators(self)

        self.halo_depths = LFRicHaloDepths(self)

        # Initialise the object holding all information on the quadrature
        # and/or evaluators required by this Invoke
        self.evaluators = LFRicBasisFunctions(self)

        # Initialise the object holding all information related to meshes
        # and inter-grid operations
        self.meshes = LFRicMeshes(self, self.psy_unique_vars)

        # Initialise the object holding information on any boundary-condition
        # kernel calls
        self.boundary_conditions = LFRicBoundaryConditions(self)

        # Information on all proxies required by this Invoke
        self.proxies = LFRicProxies(self)

        # Run-time checks for this Invoke
        self.run_time_checks = LFRicRunTimeChecks(self)

        # Information required by kernels that operate on cell-columns
        self.cell_iterators = LFRicCellIterators(self)

        # Information on the required properties of the reference element
        self.reference_element_properties = LFRicReferenceElement(self)

        # Properties of the mesh
        self.mesh_properties = LFRicMeshProperties(self)

        # Manage the variables used to store the upper and lower limits of
        # all loops in this Invoke
        self.loop_bounds = LFRicLoopBounds(self)

        # Extend argument list with stencil information
        self._alg_unique_args.extend(self.stencil.unique_alg_vars)

        # Adding in qr arguments
        self._alg_unique_qr_args = []
        for call in self.schedule.kernels():
            for rule in call.qr_rules.values():
                if rule.alg_name not in self._alg_unique_qr_args:
                    self._alg_unique_qr_args.append(rule.alg_name)
        self._alg_unique_args.extend(self._alg_unique_qr_args)
        # We also need to work out the names to use for the qr
        # arguments within the PSy-layer. These are stored in the
        # '_psy_unique_qr_vars' list.
        self._psy_unique_qr_vars = []
        for call in self.schedule.kernels():
            for rule in call.qr_rules.values():
                if rule.psy_name not in self._psy_unique_qr_vars:
                    self._psy_unique_qr_vars.append(rule.psy_name)

        # Lastly, add in halo exchange calls and global sums if
        # required. We only need to add halo exchange calls for fields
        # since operators are assembled in place and scalars don't
        # have halos. We only need to add global sum calls for scalars
        # which have a 'gh_sum' access.
        if Config.get().distributed_memory:
            # halo exchange calls
            const = LFRicConstants()
            for loop in self.schedule.loops():
                loop.create_halo_exchanges()
            # global sum calls
            for loop in self.schedule.loops():
                for scalar in loop.args_filter(
                        arg_types=const.VALID_SCALAR_NAMES,
                        arg_accesses=AccessType.get_valid_reduction_modes(),
                        unique=True):
                    global_sum = LFRicGlobalSum(scalar, parent=loop.parent)
                    loop.parent.children.insert(loop.position+1, global_sum)

        # Add the halo depth(s) for any kernel(s) that operate in the halos
        self._alg_unique_halo_depth_args = []
        if Config.get().distributed_memory:
            for call in self.schedule.kernels():
                if ("halo" in call.iterates_over and not
                        isinstance(call.halo_depth, Literal)):
                    sym = call.halo_depth.symbol
                    if sym.name not in self._alg_unique_halo_depth_args:
                        self._alg_unique_halo_depth_args.append(sym.name)

            self._alg_unique_args.extend(self._alg_unique_halo_depth_args)

    def arg_for_funcspace(self, fspace):
        '''
        Returns an argument object which is on the requested
        function space. Searches through all Kernel calls in this
        Invoke. Currently the first argument object that is found is
        used. Throws an exception if no argument exists.

        :param fspace: function space of the argument.
        :type fspace: :py:class:`psyclone.domain.lfric.FunctionSpace`

        :returns: an argument object which is on the requested function space.
        :rtype: :py:class:`psyclone.lfric.LFRicKernelArgument`

        :raises GenerationError: if the argument object does not exist.

        '''
        for kern_call in self.schedule.kernels():
            try:
                return kern_call.arguments.get_arg_on_space(fspace)
            except FieldNotFoundError:
                pass
        raise GenerationError(
            f"No argument found on '{fspace.mangled_name}' space")

    def unique_fss(self):
        '''

        :returns: the unique function space *objects* over all kernel
                  calls in this Invoke.
        :rtype: list of :py:class:`psyclone.domain.lfric.FunctionSpace`

        '''
        unique_fs = []
        unique_fs_names = []
        for kern_call in self.schedule.kernels():
            kern_fss = kern_call.arguments.unique_fss
            for fspace in kern_fss:
                if fspace.mangled_name not in unique_fs_names:
                    unique_fs.append(fspace)
                    unique_fs_names.append(fspace.mangled_name)
        return unique_fs

    @property
    def operates_on_dofs_only(self):
        '''
        :returns: whether or not this Invoke consists only of kernels that \
                  operate on DoFs.
        :rtype: bool

        '''
        return all(call.iterates_over.lower() == "dof" for call in
                   self.schedule.kernels())

    def field_on_space(self, func_space):
        '''
        If a field exists on this space for any kernel in this
        Invoke then return that field. Otherwise return 'None'.

        :param func_space: the function space for which to find an argument.
        :type func_space: :py:class:`psyclone.domain.lfric.FunctionSpace`

        '''
        for kern_call in self.schedule.kernels():
            field = func_space.field_on_space(kern_call.arguments)
            if field:
                return field
        return None

    def setup_psy_layer_symbols(self):
        ''' Declare, initialise and deallocate all symbols required by the
        PSy-layer Invoke subroutine.

        '''
        # Declare all quantities required by this PSy routine (Invoke)
        for entities in [self.scalar_args, self.fields, self.lma_ops,
                         self.stencil, self.meshes,
                         self.function_spaces, self.dofmaps, self.cma_ops,
                         self.boundary_conditions, self.evaluators,
                         self.halo_depths,
                         self.proxies, self.cell_iterators,
                         self.reference_element_properties,
                         self.mesh_properties, self.loop_bounds,
                         self.run_time_checks]:
            entities.invoke_declarations()

        cursor = 0
        for entities in [self.proxies, self.run_time_checks,
                         self.cell_iterators, self.meshes,
                         self.stencil, self.dofmaps,
                         self.cma_ops, self.boundary_conditions,
                         self.function_spaces, self.evaluators,
                         self.reference_element_properties,
                         self.mesh_properties, self.loop_bounds]:
            cursor = entities.initialise(cursor)

        if self.schedule.reductions(reprod=True):
            # We have at least one reproducible reduction so we need
            # to know the number of OpenMP threads
            symtab = self.schedule.symbol_table
            nthreads = symtab.find_or_create_tag(
                            "omp_num_threads",
                            root_name="nthreads",
                            symbol_type=DataSymbol,
                            datatype=INTEGER_TYPE)
            omp_lib = symtab.find_or_create("omp_lib",
                                            symbol_type=ContainerSymbol)
            omp_get_max_threads = symtab.find_or_create(
                "omp_get_max_threads", symbol_type=RoutineSymbol,
                interface=ImportInterface(omp_lib))

            assignment = Assignment.create(
                lhs=Reference(nthreads),
                rhs=Call.create(omp_get_max_threads))
            assignment.append_preceding_comment(
                "Determine the number of OpenMP threads")
            self.schedule.addchild(assignment, 0)
            cursor += 1

        # Now that all initialisation is done, add the comment before
        # the start of the kernels
        if Config.get().distributed_memory:
            self.schedule[cursor].preceding_comment = (
                "Call kernels and communication routines")
        else:
            self.schedule[cursor].preceding_comment = (
                "Call kernels")

        # Deallocate any basis arrays
        self.evaluators.deallocate()


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicInvoke']
