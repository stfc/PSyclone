# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

# Imports
from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric import LFRicConstants
from psyclone.errors import GenerationError, FieldNotFoundError
from psyclone.f2pygen import (AssignGen, CommentGen, DeclGen, SubroutineGen,
                              UseGen)
from psyclone.psyGen import Invoke


class LFRicInvoke(Invoke):
    '''
    The LFRic-specific Invoke class. This passes the LFRic-specific
    InvokeSchedule class to the base class so it creates the one we
    require.  Also overrides the 'gen_code' method so that we generate
    dynamo specific invocation code.

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
    def __init__(self, alg_invocation, idx, invokes):
        if not alg_invocation and not idx:
            # This 'if' test is added to support pyreverse
            return
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynInvokeSchedule
        self._schedule = DynInvokeSchedule('name', None)  # for pyreverse
        reserved_names_list = []
        const = LFRicConstants()
        reserved_names_list.extend(const.STENCIL_MAPPING.values())
        reserved_names_list.extend(const.VALID_STENCIL_DIRECTIONS)
        reserved_names_list.extend(["omp_get_thread_num",
                                    "omp_get_max_threads"])
        Invoke.__init__(self, alg_invocation, idx, DynInvokeSchedule,
                        invokes, reserved_names=reserved_names_list)

        # The base class works out the algorithm code's unique argument
        # list and stores it in the 'self._alg_unique_args'
        # list. However, the base class currently ignores any stencil and qr
        # arguments so we need to add them in.

        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import (DynFunctionSpaces, DynDofmaps,
                                        DynLMAOperators, DynGlobalSum,
                                        DynCMAOperators, DynBasisFunctions,
                                        DynMeshes, DynBoundaryConditions,
                                        DynProxies, DynCellIterators,
                                        DynReferenceElement,
                                        LFRicMeshProperties)
        from psyclone.domain.lfric import (LFRicLoopBounds, LFRicRunTimeChecks,
                                           LFRicScalarArgs, LFRicFields,
                                           LFRicStencils)

        self.scalar_args = LFRicScalarArgs(self)

        # Initialise our Invoke stencil information
        self.stencil = LFRicStencils(self)

        # Initialise our information on the function spaces used by this Invoke
        self.function_spaces = DynFunctionSpaces(self)

        # Initialise the object holding all information on the dofmaps
        # required by this Invoke
        self.dofmaps = DynDofmaps(self)

        # Initialise information on all of the fields accessed in this Invoke
        self.fields = LFRicFields(self)

        # Initialise info on all of the LMA operators used in this Invoke
        self.lma_ops = DynLMAOperators(self)

        # Initialise the object holding all information on the column-
        # -matrix assembly operators required by this Invoke
        self.cma_ops = DynCMAOperators(self)

        # Initialise the object holding all information on the quadrature
        # and/or evaluators required by this Invoke
        self.evaluators = DynBasisFunctions(self)

        # Initialise the object holding all information related to meshes
        # and inter-grid operations
        self.meshes = DynMeshes(self, self.psy_unique_vars)

        # Initialise the object holding information on any boundary-condition
        # kernel calls
        self.boundary_conditions = DynBoundaryConditions(self)

        # Information on all proxies required by this Invoke
        self.proxies = DynProxies(self)

        # Run-time checks for this Invoke
        self.run_time_checks = LFRicRunTimeChecks(self)

        # Information required by kernels that operate on cell-columns
        self.cell_iterators = DynCellIterators(self)

        # Information on the required properties of the reference element
        self.reference_element_properties = DynReferenceElement(self)

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
                    global_sum = DynGlobalSum(scalar, parent=loop.parent)
                    loop.parent.children.insert(loop.position+1, global_sum)

    def arg_for_funcspace(self, fspace):
        '''
        Returns an argument object which is on the requested
        function space. Searches through all Kernel calls in this
        Invoke. Currently the first argument object that is found is
        used. Throws an exception if no argument exists.

        :param fspace: function space of the argument.
        :type fspace: :py:class:`psyclone.domain.lfric.FunctionSpace`

        :returns: an argument object which is on the requested function space.
        :rtype: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

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

    def gen_code(self, parent):
        '''
        Generates LFRic-specific invocation code (the subroutine
        called by the associated Invoke call in the algorithm
        layer). This consists of the PSy invocation subroutine and the
        declaration of its arguments.

        :param parent: the parent node in the AST (of the code to be \
                       generated) to which the node describing the PSy \
                       subroutine will be added.
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`

        '''
        # Create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names +
                                   self.stencil.unique_alg_vars +
                                   self._psy_unique_qr_vars)

        # Declare all quantities required by this PSy routine (Invoke)
        for entities in [self.scalar_args, self.fields, self.lma_ops,
                         self.stencil, self.meshes,
                         self.function_spaces, self.dofmaps, self.cma_ops,
                         self.boundary_conditions, self.evaluators,
                         self.proxies, self.cell_iterators,
                         self.reference_element_properties,
                         self.mesh_properties, self.loop_bounds,
                         self.run_time_checks]:
            entities.declarations(invoke_sub)

        # Initialise all quantities required by this PSy routine (Invoke)

        if self.schedule.reductions(reprod=True):
            # We have at least one reproducible reduction so we need
            # to know the number of OpenMP threads
            omp_function_name = "omp_get_max_threads"
            tag = "omp_num_threads"
            nthreads_name = \
                self.schedule.symbol_table.lookup_with_tag(tag).name
            invoke_sub.add(UseGen(invoke_sub, name="omp_lib", only=True,
                                  funcnames=[omp_function_name]))
            # Note: There is no assigned kind for 'integer' 'nthreads' as this
            # would imply assigning 'kind' to 'th_idx' and other elements of
            # the OMPParallelDirective
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=[nthreads_name]))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(
                invoke_sub, " Determine the number of OpenMP threads"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(AssignGen(invoke_sub, lhs=nthreads_name,
                                     rhs=omp_function_name+"()"))

        for entities in [self.proxies, self.run_time_checks,
                         self.cell_iterators, self.meshes,
                         self.stencil, self.dofmaps,
                         self.cma_ops, self.boundary_conditions,
                         self.function_spaces, self.evaluators,
                         self.reference_element_properties,
                         self.mesh_properties, self.loop_bounds]:
            entities.initialise(invoke_sub)

        # Now that everything is initialised and checked, we can call
        # our kernels

        invoke_sub.add(CommentGen(invoke_sub, ""))
        if Config.get().distributed_memory:
            invoke_sub.add(CommentGen(invoke_sub, " Call kernels and "
                                      "communication routines"))
        else:
            invoke_sub.add(CommentGen(invoke_sub, " Call our kernels"))
        invoke_sub.add(CommentGen(invoke_sub, ""))

        # Add content from the schedule
        self.schedule.gen_code(invoke_sub)

        # Deallocate any basis arrays
        self.evaluators.deallocate(invoke_sub)

        invoke_sub.add(CommentGen(invoke_sub, ""))

        # finally, add me to my parent
        parent.add(invoke_sub)


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicInvoke']
