# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''This module contains the GOcean-specific OpenCL transformation.
'''

from psyclone.configuration import Config
from psyclone.gocean1p0 import GOInvokeSchedule, GOLoop
from psyclone.psyGen import Transformation, args_filter, InvokeSchedule
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Routine, Call, Reference, Literal, Assignment
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, \
    ContainerSymbol, UnknownFortranType, ArgumentInterface, ImportInterface, \
    INTEGER_TYPE, CHARACTER_TYPE
from psyclone.transformations import TransformationError, KernelTrans


class GOOpenCLTrans(Transformation):
    '''
    Switches on/off the generation of an OpenCL PSy layer for a given
    InvokeSchedule. Additionally, it will generate OpenCL kernels for
    each of the kernels referenced by the Invoke. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> API = "gocean1.0"
    >>> FILENAME = "shallow_alg.f90" # examples/gocean/eg1
    >>> ast, invoke_info = parse(FILENAME, api=API)
    >>> psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> ocl_trans = GOOpenCLTrans()
    >>> ocl_trans.apply(schedule)
    >>> schedule.view()

    '''

    # Specify which OpenCL command queue to use for management operations like
    # data transfers when generating an OpenCL PSy-layer
    _OCL_MANAGEMENT_QUEUE = 1

    # TODO #1134: These are class attributes because multiple invokes may have
    # to generate a single OpenCL environment (e.g. to share the device data
    # pointers) and therefore guarantee the same properties, but this hasn't
    # been tested. PSycloneBench ShallowWater could be an example of this.

    # Biggest queue number that any kernel is allocated to. The OpenCL
    # environment should be set up with at least this number of queues.
    _max_queue_number = 1
    # Whether to enable the profiling option in the OpenCL environment
    _enable_profiling = False
    # Whether to enable the out_of_order option in the OpenCL environment
    _out_of_order = False
    # Total number of invokes that have been transformed to OpenCL
    _transformed_invokes = 0

    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str
        '''
        return "GOOpenCLTrans"

    def validate(self, node, options=None):
        '''
        Checks that the supplied InvokeSchedule is valid and that an OpenCL
        version of it can be generated.

        :param node: the Schedule to check.
        :type node: :py:class:`psyclone.psyGen.InvokeSchedule`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:value or None
        :param bool options["enable_profiling"]: whether or not to set up the \
                OpenCL environment with the profiling option enabled.
        :param bool options["out_of_order"]: whether or not to set up the \
                OpenCL environment with the out_of_order option enabled.
        :param bool options["end_barrier"]: whether or not to add an OpenCL \
                barrier at the end of the transformed invoke.

        :raises TransformationError: if the InvokeSchedule is not for the \
                                     GOcean1.0 API.
        :raises TransformationError: if any of the kernels have arguments \
                                     which are passed as a literal.
        :raises TransformationError: if any of the provided options is invalid.
        :raises TransformationError: if any of the provided options is not \
                                     compatible with a previous OpenCL
                                     environment.
        :raises TransformationError: if any kernel in this invoke has a \
                                     global variable used by an import.
        :raises TransformationError: if any kernel does not iterate over \
                                     the whole grid.
        '''

        if isinstance(node, InvokeSchedule):
            if not isinstance(node, GOInvokeSchedule):
                raise TransformationError(
                    "OpenCL generation is currently only supported for the "
                    "GOcean API but got an InvokeSchedule of type: '{0}'".
                    format(type(node).__name__))
        else:
            raise TransformationError(
                "Error in GOOpenCLTrans: the supplied node must be a (sub-"
                "class of) InvokeSchedule but got {0}".format(type(node)))

        # Validate options map
        valid_options = ['end_barrier', 'enable_profiling', 'out_of_order']
        for key, value in options.items():
            if key in valid_options:
                # All current options should contain boolean values
                if not isinstance(value, bool):
                    raise TransformationError(
                        "InvokeSchedule OpenCL option '{0}' "
                        "should be a boolean.".format(key))
            else:
                raise TransformationError(
                    "InvokeSchedule does not support the OpenCL option '{0}'. "
                    "The supported options are: {1}."
                    "".format(key, valid_options))

        # Validate that the options are valid with previously generated OpenCL
        if self._transformed_invokes > 0:
            if ('enable_profiling' in options and
                    self._enable_profiling != options['enable_profiling']):
                raise TransformationError(
                    "Can't generate an OpenCL Invoke with enable_profiling='"
                    "{0}' since a previous transformation used a different "
                    "value, and their OpenCL environments must match."
                    "".format(options['enable_profiling']))

            if ('out_of_order' in options and
                    self._out_of_order != options['out_of_order']):
                raise TransformationError(
                    "Can't generate an OpenCL Invoke with out_of_order='{0}' "
                    "since a previous transformation used a different value, "
                    "and their OpenCL environments must match."
                    "".format(options['out_of_order']))

        # Now we need to check that none of the invoke arguments is a literal
        args = args_filter(node.args, arg_types=["scalar"])
        for arg in args:
            if arg.is_literal:
                raise TransformationError(
                    "Cannot generate OpenCL for Invokes that contain kernel "
                    "arguments which are a literal, but found the literal "
                    "'{0}' used as an argument in invoke '{1}'."
                    "".format(arg.name, node.name))

        # Check that we can construct the PSyIR and SymbolTable of each of
        # the kernels in this Schedule. Also check that none of them access
        # any form of global data (that is not a routine argument).
        for kern in node.kernels():
            KernelTrans.validate(kern)
            ksched = kern.get_kernel_schedule()
            global_variables = ksched.symbol_table.imported_symbols
            if global_variables:
                raise TransformationError(
                    "The Symbol Table for kernel '{0}' contains the following "
                    "symbols with 'global' scope: {1}. An OpenCL kernel cannot"
                    " call other kernels and all of the data it accesses must "
                    "be passed by argument. Use the KernelImportsToArguments "
                    "transformation to convert such symbols to kernel "
                    "arguments first.".
                    format(kern.name, [sym.name for sym in global_variables]))

        # In OpenCL all kernel loops should iterate the whole grid
        for kernel in node.kernels():
            inner_loop = kernel.ancestor(GOLoop)
            outer_loop = inner_loop.ancestor(GOLoop)
            if not (inner_loop.field_space == "go_every" and
                    outer_loop.field_space == "go_every" and
                    inner_loop.iteration_space == "go_all_pts" and
                    outer_loop.iteration_space == "go_all_pts"):
                raise TransformationError(
                    "The kernel '{0}' does not iterate over all grid points. "
                    "This is a necessary requirement for generating "
                    "the OpenCL code and can be done by applying the "
                    "GOMoveIterationBoundariesInsideKernelTrans to each kernel"
                    " before the GOOpenCLTrans.".format(kernel.name))

    def apply(self, node, options=None):
        '''
        Apply the OpenCL transformation to the supplied GOInvokeSchedule. This
        causes PSyclone to generate an OpenCL version of the corresponding
        PSy-layer routine. The generated code makes use of the FortCL
        library (https://github.com/stfc/FortCL) in order to manage the
        OpenCL device directly from Fortran.

        :param node: the InvokeSchedule to transform.
        :type node: :py:class:`psyclone.psyGen.GOInvokeSchedule`
        :param options: set of option to tune the OpenCL generation.
        :type options: dict of str:value or None
        :param bool options["enable_profiling"]: whether or not to set up the \
                OpenCL environment with the profiling option enabled.
        :param bool options["out_of_order"]: whether or not to set up the \
                OpenCL environment with the out_of_order option enabled.
        :param bool options["end_barrier"]: whether or not to add an OpenCL \
                barrier at the end of the transformed invoke.

        :returns: None, None
        :rtype: (NoneType, NoneType)

        '''
        if not options:
            options = {}

        self.validate(node, options)

        # Update class attributes
        if 'enable_profiling' in options:
            self._enable_profiling = options['enable_profiling']

        if 'out_of_order' in options:
            self._out_of_order = options['out_of_order']

        self._transformed_invokes += 1

        # Update the maximum value that the queue_number have.
        for kernel in node.coded_kernels():
            self._max_queue_number = max(self._max_queue_number,
                                         kernel.opencl_options["queue_number"])

        # Insert, if they don't already exist, the necessary OpenCL helper
        # subroutines in the root Container,
        self._insert_opencl_init_routine(node.root)
        self._insert_initialise_grid_buffers(node.root)
        self._insert_write_grid_buffers(node.root)
        self._insert_ocl_read_from_device_function(node.root)
        self._insert_ocl_write_to_device_function(node.root)
        self._insert_ocl_initialise_buffer(node.root)

        for kern in node.coded_kernels():
            self._insert_ocl_arg_setter_routine(node.root, kern)

        # TODO #1134: Some of the OpenCL logic is provided at generation time
        # when the following opencl flag is set. This should eventually be part
        # of this tranformation.
        node.opencl = True
        if 'end_barrier' in options:
            node._opencl_end_barrier = options['end_barrier']

        # TODO #595: Remove transformation return values
        return None, None

    @staticmethod
    def _insert_ocl_arg_setter_routine(node, kernel):
        '''
        Returns the symbol of the subroutine that sets the OpenCL kernel
        arguments for the provided PSy-layer kernel using FortCL. If the
        subroutine doesn't exist it also generates it.

        :param node: the container where the new subroutine will be inserted.
        :param type: :py:class:`psyclone.psyir.nodes.Container`
        :param kernel: the kernel call for which to provide the arg_setter \
                       subroutine.
        :param type: :py:class:`psyclone.psyGen.CodedKern`

        :returns: the symbol representing the arg_setter subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        # Check if the subroutine already exist.
        sub_name = kernel.name + "_set_args"
        try:
            return node.symbol_table.lookup_with_tag(sub_name)
        except KeyError:
            # If the Symbol does not exist, the rest of this method
            # will generate it.
            pass

        # Create the new Routine and RoutineSymbol
        node.symbol_table.add(RoutineSymbol(sub_name), tag=sub_name)
        argsetter = Routine(sub_name)
        arg_list = []

        # Add subroutine imported symbols
        clfortran = ContainerSymbol("clfortran")
        clsetkernelarg = RoutineSymbol("clSetKernelArg",
                                       interface=ImportInterface(clfortran))
        iso_c = ContainerSymbol("iso_c_binding")
        c_sizeof = RoutineSymbol("C_SIZEOF", interface=ImportInterface(iso_c))
        c_loc = RoutineSymbol("C_LOC", interface=ImportInterface(iso_c))
        c_intptr_t = RoutineSymbol("c_intptr_t",
                                   interface=ImportInterface(iso_c))
        ocl_utils = ContainerSymbol("ocl_utils_mod")
        check_status = RoutineSymbol("check_status",
                                     interface=ImportInterface(ocl_utils))
        argsetter.symbol_table.add(clfortran)
        argsetter.symbol_table.add(clsetkernelarg)
        argsetter.symbol_table.add(iso_c)
        argsetter.symbol_table.add(c_sizeof)
        argsetter.symbol_table.add(c_loc)
        argsetter.symbol_table.add(c_intptr_t)
        argsetter.symbol_table.add(ocl_utils)
        argsetter.symbol_table.add(check_status)

        # Add an argument symbol for the kernel object
        kobj = argsetter.symbol_table.new_symbol(
            "kernel_obj", symbol_type=DataSymbol,
            interface=ArgumentInterface(ArgumentInterface.Access.READ),
            datatype=UnknownFortranType(
                "INTEGER(KIND=c_intptr_t), TARGET :: kernel_obj"))
        arg_list.append(kobj)

        # Include each kernel call argument as an argument of this routine
        for arg in kernel.arguments.args:

            name = argsetter.symbol_table.next_available_name(arg.name)

            # This function requires 'TARGET' annotated declarations which are
            # not supported in the PSyIR, so we build them as
            # UnknownFortranType for now.
            if arg.is_scalar and arg.intrinsic_type == "real":
                pointer_type = UnknownFortranType(
                    "REAL(KIND=go_wp), INTENT(IN), TARGET :: " + name)
            elif arg.is_scalar:
                pointer_type = UnknownFortranType(
                    "INTEGER, INTENT(IN), TARGET :: " + name)
            else:
                # Everything else is a cl_mem pointer (c_intptr_t)
                pointer_type = UnknownFortranType(
                    "INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: " + name)

            new_arg = DataSymbol(
                name, datatype=pointer_type,
                interface=ArgumentInterface(ArgumentInterface.Access.READ))
            argsetter.symbol_table.add(new_arg)
            arg_list.append(new_arg)

        argsetter.symbol_table.specify_argument_list(arg_list)

        # Create the ierr local variable
        ierr = argsetter.symbol_table.new_symbol(
            "ierr", symbol_type=DataSymbol, datatype=INTEGER_TYPE)

        # Call the clSetKernelArg for each argument and a check_status to
        # see if the OpenCL call has succeeded
        for index, variable in enumerate(arg_list[1:]):
            call = Call.create(clsetkernelarg,
                               [Reference(kobj),
                                Literal(str(index), INTEGER_TYPE),
                                Call.create(c_sizeof, [Reference(variable)]),
                                Call.create(c_loc, [Reference(variable)])])
            assignment = Assignment.create(Reference(ierr), call)
            argsetter.addchild(assignment)
            emsg = "clSetKernelArg: arg {0} of {1}".format(index, kernel.name)
            call = Call.create(check_status, [Literal(emsg, CHARACTER_TYPE),
                                              Reference(ierr)])
            argsetter.addchild(call)

        argsetter.children[0].preceding_comment = \
            "Set the arguments for the {0} OpenCL Kernel".format(kernel.name)

        # Add the subroutine as child of the provided node
        node.addchild(argsetter)

        return node.symbol_table.lookup_with_tag(sub_name)

    def _insert_opencl_init_routine(self, node):
        '''
        Returns the symbol of the subroutine that initialises the OpenCL
        environment using FortCL. If the subroutine doesn't exist it also
        generates it.

        :param node: the container where the new subroutine will be inserted.
        :param type: :py:class:`psyclone.psyir.nodes.Container`

        :returns: the symbol representing the OpenCL initialisation subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        symtab = node.symbol_table
        try:
            # TODO #1134: The init routine may need to be regenerated if there
            # are multiple Invokes (multiple applies of this transformation),
            # because _max_queue_number may have increased and we need to load
            # the kernels of both invokes.
            return symtab.lookup_with_tag("ocl_init_routine")
        except KeyError:
            # If the Symbol does not exist, the rest of this method
            # will generate it.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol("psy_init",
                                            symbol_type=RoutineSymbol,
                                            tag="ocl_init_routine").name

        # Choose a round-robin device number if it has MPI and multiple
        # accelerators.
        distributed_memory = Config.get().distributed_memory
        devices_per_node = Config.get().ocl_devices_per_node
        additional_uses = ""
        additional_stmts = ""
        if devices_per_node > 1 and distributed_memory:
            additional_uses += "USE parallel_mod, ONLY: get_rank"
            additional_stmts += \
                ("ocl_device_num = mod(get_rank() - 1, {0}) + 1"
                 "".format(devices_per_node))

        # Get a set of all kernel names in the Container. This implementation
        # currently assumes all of them will be available in OpenCL
        unique_kernels = {kernel.name for kernel in node.coded_kernels()}

        # Code of the subroutine in Fortran
        code = '''
        subroutine psy_init()
          {0}
          use fortcl, only: ocl_env_init, add_kernels
          character(len=30) kernel_names({1})
          integer :: ocl_device_num=1
          logical, save :: initialised=.false.
          ! Check to make sure we only execute this routine once
          if (.not. initialised) then
            initialised = .true.
            ! Initialise the opencl environment/device
            {2}
            call ocl_env_init({3}, ocl_device_num, {4}, {5})
            ! The kernels this psy layer module requires
        '''.format(
                additional_uses,
                len(unique_kernels),
                additional_stmts,
                self._max_queue_number,
                ".true." if self._enable_profiling else ".false.",
                ".true." if self._out_of_order else ".false.",
                )

        for index, kernel_name in enumerate(unique_kernels):
            code += "kernel_names({0}) = \"{1}\"\n".format(index + 1,
                                                           kernel_name)

        code += '''\
            ! Create the opencl kernel objects. This expects to find all of
            ! the compiled kernels in FORTCL_KERNELS_FILE environment variable
            call add_kernels({0}, kernel_names)
          end if
        end subroutine psy_init'''.format(len(unique_kernels))

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Add the subroutine as child of the provided node
        node.addchild(subroutine.detach())

        return symtab.lookup_with_tag("ocl_init_routine")

    @staticmethod
    def _insert_initialise_grid_buffers(node):
        '''
        Returns the symbol of a subroutine that initialises all OpenCL grid
        buffers in the OpenCL device using FortCL. If the subroutine doesn't
        already exist it also generates it.

        :param node: the container where the new subroutine will be inserted.
        :param type: :py:class:`psyclone.psyir.nodes.Container`

        :returns: the symbol of the grid buffer initialisation subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        # pylint: disable=too-many-locals
        symtab = node.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_init_grid_buffers")
        except KeyError:
            # If the Symbol does not exist, the rest of this method
            # will generate it.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol("initialise_grid_device_buffers",
                                            symbol_type=RoutineSymbol,
                                            tag="ocl_init_grid_buffers").name

        # Get the GOcean API property names used in this routine
        api_config = Config.get().api_conf("gocean1.0")
        props = api_config.grid_properties
        num_x = props["go_grid_nx"].fortran.format("field")
        num_y = props["go_grid_ny"].fortran.format("field")

        int_arrays = []
        real_arrays = []
        for key, prop in props.items():
            if key == "go_grid_data":
                # TODO #676: Ignore because go_grid_data is actually a field
                # property
                continue
            if prop.type == "array" and prop.intrinsic_type == "integer":
                int_arrays.append(prop.fortran.format("field"))
            elif prop.type == "array" and prop.intrinsic_type == "real":
                real_arrays.append(prop.fortran.format("field"))

        # Code of the subroutine in Fortran
        code = '''
        subroutine initialise_device_grid(field)
            USE fortcl, ONLY: create_ronly_buffer
            use field_mod
            type(r2d_field), intent(inout), target :: field
            integer(kind=c_size_t) size_in_bytes
            IF (.not. c_associated({2}_device)) THEN
                ! Create integer grid fields
                size_in_bytes = int({0}*{1}, 8) * c_sizeof({2}(1,1))
        '''.format(num_x, num_y, int_arrays[0])

        for int_array in int_arrays:
            code += '''
                {0}_device = transfer(create_ronly_buffer(size_in_bytes), &
                                      {0}_device)
            '''.format(int_array)

        code += '''
                ! Create real grid buffers
                size_in_bytes = int({0} * {1}, 8) * c_sizeof({2}(1,1))
        '''.format(num_x, num_y, real_arrays[0])

        for real_array in real_arrays:
            code += '''
                {0}_device = transfer(create_ronly_buffer(size_in_bytes), &
                                      {0}_device)
            '''.format(real_array)

        code += '''
            END IF
        end subroutine initialise_device_grid
        '''

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Add the subroutine as child of the provided node
        node.addchild(subroutine.detach())

        return symtab.lookup_with_tag("ocl_init_grid_buffers")

    def _insert_write_grid_buffers(self, node):
        '''
        Returns the symbol of a subroutine that writes the values of the grid
        properties into the OpenCL device buffers using FortCL. If the
        subroutine doesn't already exist it also generates it.

        :param node: the container where the new subroutine will be inserted.
        :param type: :py:class:`psyclone.psyir.nodes.Container`

        :returns: the symbol representing the grid buffers writing subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        # pylint: disable=too-many-locals
        symtab = node.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_write_grid_buffers")
        except KeyError:
            # If the Symbol does not exist, the rest of this method
            # will generate it.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol("write_grid_buffers",
                                            symbol_type=RoutineSymbol,
                                            tag="ocl_write_grid_buffers").name

        # Get the GOcean API property names used in this routine
        api_config = Config.get().api_conf("gocean1.0")
        props = api_config.grid_properties
        num_x = props["go_grid_nx"].fortran.format("field")
        num_y = props["go_grid_ny"].fortran.format("field")

        # Code of the subroutine in Fortran
        code = '''
        subroutine write_device_grid(field)
            USE fortcl, ONLY: get_cmd_queues
            use iso_c_binding, only: c_intptr_t, c_size_t, c_sizeof
            USE clfortran
            USE ocl_utils_mod, ONLY: check_status
            type(r2d_field), intent(inout), target :: field
            integer(kind=c_size_t) size_in_bytes
            INTEGER(c_intptr_t), pointer :: cmd_queues(:)
            integer(c_intptr_t) :: cl_mem
            integer :: ierr
            cmd_queues => get_cmd_queues()
            ! Integer grid buffers
            size_in_bytes = int({0} * {1}, 8) * &
                            c_sizeof(field%grid%tmask(1,1))
            cl_mem = transfer(field%grid%tmask_device, cl_mem)
            ierr = clEnqueueWriteBuffer(cmd_queues({2}), &
                        cl_mem, CL_TRUE, 0_8, size_in_bytes, &
                        C_LOC(field%grid%tmask), 0, C_NULL_PTR, C_NULL_PTR)
            CALL check_status("clEnqueueWriteBuffer tmask", ierr)
            ! Real grid buffers
            size_in_bytes = int({0} * {1}, 8) * &
                            c_sizeof(field%grid%area_t(1,1))
        '''.format(num_x, num_y, self._OCL_MANAGEMENT_QUEUE)
        write_str = '''
            cl_mem = transfer(field%grid%{0}_device, cl_mem)
            ierr = clEnqueueWriteBuffer(cmd_queues({1}), &
                       cl_mem, CL_TRUE, 0_8, size_in_bytes, &
                       C_LOC(field%grid%{0}), 0, C_NULL_PTR, C_NULL_PTR)
            CALL check_status("clEnqueueWriteBuffer {0}_device", ierr)
        '''
        for grid_prop in ['area_t', 'area_u', 'area_v', 'dx_u', 'dx_v',
                          'dx_t', 'dy_u', 'dy_v', 'dy_t', 'gphiu', 'gphiv']:
            code += write_str.format(grid_prop, self._OCL_MANAGEMENT_QUEUE)
        code += "end subroutine write_device_grid"

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Add the subroutine as child of the provided node
        node.addchild(subroutine.detach())

        return symtab.lookup_with_tag("ocl_write_grid_buffers")

    def _insert_ocl_read_from_device_function(self, node):
        '''
        Returns the symbol of a subroutine that retrieves the data back from
        an OpenCL device using FortCL. If the subroutine doesn't already exist
        it also generates it.

        :param node: the container where the new subroutine will be inserted.
        :param type: :py:class:`psyclone.psyir.nodes.Container`

        :returns: the symbol of the buffer data retrieving subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        symtab = node.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_read_func")
        except KeyError:
            # If the subroutines does not exist, it needs to be
            # generated first.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol("read_from_device",
                                            symbol_type=RoutineSymbol,
                                            tag="ocl_read_func").name

        # Code of the subroutine in Fortran
        code = '''
        subroutine read_sub(from, to, startx, starty, nx, ny, blocking)
            USE iso_c_binding, only: c_ptr, c_intptr_t, c_size_t, c_sizeof
            USE ocl_utils_mod, ONLY: check_status
            use kind_params_mod, only: go_wp
            USE clfortran
            USE fortcl, ONLY: get_cmd_queues
            type(c_ptr), intent(in) :: from
            real(go_wp), intent(inout), dimension(:,:), target :: to
            integer, intent(in) :: startx, starty, nx, ny
            logical, intent(in) :: blocking
            INTEGER(c_size_t) :: size_in_bytes, offset_in_bytes
            integer(c_intptr_t) :: cl_mem
            INTEGER(c_intptr_t), pointer :: cmd_queues(:)
            integer :: ierr, i

            ! Give the from pointer the appropriate OpenCL memory object type
            cl_mem = transfer(from, cl_mem)
            cmd_queues => get_cmd_queues()

            ! Two copy strategies depending on how much of the total length
            ! nx covers.
            if (nx < size(to, 1) / 2) then
                ! Dispatch asynchronous copies of just the contiguous data.
                do i = starty, starty+ny
                    size_in_bytes = int(nx, 8) * c_sizeof(to(1,1))
                    offset_in_bytes = int(size(to, 1) * (i-1) + (startx-1)) &
                                      * c_sizeof(to(1,1))
                    ierr = clEnqueueReadBuffer(cmd_queues({0}), cl_mem, &
                        CL_FALSE, offset_in_bytes, size_in_bytes, &
                        C_LOC(to(startx, i)), 0, C_NULL_PTR, C_NULL_PTR)
                    CALL check_status("clEnqueueReadBuffer", ierr)
                enddo
                if (blocking) then
                    CALL check_status("clFinish on read", &
                        clFinish(cmd_queues({0})))
                endif
            else
                ! Copy across the whole starty:starty+ny rows in a single
                ! copy operation.
                size_in_bytes = int(size(to, 1) * ny, 8) * c_sizeof(to(1,1))
                offset_in_bytes = int(size(to,1)*(starty-1), 8) &
                                  * c_sizeof(to(1,1))
                ierr = clEnqueueReadBuffer(cmd_queues({0}), cl_mem, &
                    CL_TRUE, offset_in_bytes, size_in_bytes, &
                    C_LOC(to(1,starty)), 0, C_NULL_PTR, C_NULL_PTR)
                CALL check_status("clEnqueueReadBuffer", ierr)
            endif
        end subroutine read_sub
        '''.format(self._OCL_MANAGEMENT_QUEUE)

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]

        # Rename subroutine
        subroutine.name = subroutine_name

        # Add the subroutine as child of the provided node
        node.addchild(subroutine.detach())

        return symtab.lookup_with_tag("ocl_read_func")

    def _insert_ocl_write_to_device_function(self, node):
        '''
        Returns the symbol of a subroutine that writes the buffer data into
        an OpenCL device using FortCL. If the subroutine doesn't already exist
        it also generates it.

        :param node: the container where the new subroutine will be inserted.
        :param type: :py:class:`psyclone.psyir.nodes.Container`

        :returns: the symbol of the buffer writing subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        symtab = node.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_write_func")
        except KeyError:
            # If the subroutines does not exist, it needs to be
            # generated first.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol("write_to_device",
                                            symbol_type=RoutineSymbol,
                                            tag="ocl_write_func").name

        # Code of the subroutine in Fortran
        code = '''
        subroutine write_sub(from, to, startx, starty, nx, ny, blocking)
            USE iso_c_binding, only: c_ptr, c_intptr_t, c_size_t, c_sizeof
            USE ocl_utils_mod, ONLY: check_status
            use kind_params_mod, only: go_wp
            USE clfortran
            USE fortcl, ONLY: get_cmd_queues
            real(go_wp), intent(in), dimension(:,:), target :: from
            type(c_ptr), intent(in) :: to
            integer, intent(in) :: startx, starty, nx, ny
            logical, intent(in) :: blocking
            integer(c_intptr_t) :: cl_mem
            INTEGER(c_size_t) :: size_in_bytes, offset_in_bytes
            INTEGER(c_intptr_t), pointer :: cmd_queues(:)
            integer :: ierr, i

            ! Give the to pointer the appropriate OpenCL memory object type
            cl_mem = transfer(to, cl_mem)
            cmd_queues => get_cmd_queues()

            ! Two copy strategies depending on how much of the total length
            ! nx covers.
            if (nx < size(from,1) / 2) then
                ! Dispatch asynchronous copies of just the contiguous data.
                do i=starty, starty+ny
                    size_in_bytes = int(nx, 8) * c_sizeof(from(1,1))
                    offset_in_bytes = int(size(from, 1) * (i-1) + (startx-1)) &
                                      * c_sizeof(from(1,1))
                    ierr = clEnqueueWriteBuffer(cmd_queues({0}), cl_mem, &
                        CL_FALSE, offset_in_bytes, size_in_bytes, &
                        C_LOC(from(startx, i)), 0, C_NULL_PTR, C_NULL_PTR)
                    CALL check_status("clEnqueueWriteBuffer", ierr)
                enddo
                if (blocking) then
                    CALL check_status("clFinish on write", &
                        clFinish(cmd_queues({0})))
                endif
            else
                ! Copy across the whole starty:starty+ny rows in a single
                ! copy operation.
                size_in_bytes = int(size(from,1) * ny, 8) * c_sizeof(from(1,1))
                offset_in_bytes = int(size(from,1) * (starty-1)) &
                                  * c_sizeof(from(1,1))
                ierr = clEnqueueWriteBuffer(cmd_queues({0}), cl_mem, &
                    CL_TRUE, offset_in_bytes, size_in_bytes, &
                    C_LOC(from(1, starty)), 0, C_NULL_PTR, C_NULL_PTR)
                CALL check_status("clEnqueueWriteBuffer", ierr)
            endif
        end subroutine write_sub
        '''.format(self._OCL_MANAGEMENT_QUEUE)

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Add the subroutine as child of the provided node
        node.addchild(subroutine.detach())

        return symtab.lookup_with_tag("ocl_write_func")

    @staticmethod
    def _insert_ocl_initialise_buffer(node):
        '''
        Returns the symbol of a subroutine that initialises an OpenCL buffer in
        the OpenCL device using FortCL. If the subroutine doesn't already exist
        it also generates it.

        :param node: the container where the new subroutine will be inserted.
        :param type: :py:class:`psyclone.psyir.nodes.Container`
        :returns: the symbol of the buffer initialisation subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        # pylint: disable=too-many-locals
        symtab = node.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_init_buffer_func")
        except KeyError:
            # If the Symbol does not exist, the rest of this method
            # will generate it.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol("initialise_device_buffer",
                                            symbol_type=RoutineSymbol,
                                            tag="ocl_init_buffer_func").name

        # Get the GOcean API property names used in this routine
        api_config = Config.get().api_conf("gocean1.0")
        host_buff = \
            api_config.grid_properties["go_grid_data"].fortran.format("field")
        props = api_config.grid_properties
        num_x = props["go_grid_nx"].fortran.format("field")
        num_y = props["go_grid_ny"].fortran.format("field")

        # Fields need to provide a function pointer to how the
        # device data is going to be read and written, if it doesn't
        # exist, create the appropriate subroutine first.
        read_fp = symtab.lookup_with_tag("ocl_read_func").name
        write_fp = symtab.lookup_with_tag("ocl_write_func").name

        # Code of the subroutine in Fortran
        code = '''
        subroutine initialise_device_buffer(field)
            USE fortcl, ONLY: create_rw_buffer
            use field_mod
            type(r2d_field), intent(inout), target :: field
            integer(kind=c_size_t) size_in_bytes
            IF (.NOT. field%data_on_device) THEN
                size_in_bytes = int({0}*{1}, 8) * &
                                    c_sizeof({2}(1,1))
                ! Create buffer on device, we store it without type information
                ! on the dl_esm_inf pointer (transfer/static_cast to void*)
                field%device_ptr = transfer( &
                    create_rw_buffer(size_in_bytes), &
                    field%device_ptr)
                field%data_on_device = .true.
                field%read_from_device_f => {3}
                field%write_to_device_f => {4}
            END IF
        end subroutine initialise_device_buffer
        '''.format(num_x, num_y, host_buff, read_fp, write_fp)

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Add the subroutine as child of the provided node
        node.addchild(subroutine.detach())

        return symtab.lookup_with_tag("ocl_init_buffer_func")


# For AutoAPI documentation generation
__all__ = ["GOOpenCLTrans"]
