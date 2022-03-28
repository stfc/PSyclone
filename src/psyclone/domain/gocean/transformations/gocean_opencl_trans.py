# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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

import os

from fparser.two import Fortran2003
from psyclone.configuration import Config
from psyclone.errors import GenerationError
from psyclone.gocean1p0 import GOInvokeSchedule, GOLoop
from psyclone.psyGen import Transformation, args_filter, InvokeSchedule, \
    HaloExchange
from psyclone.psyir.backend.opencl import OpenCLWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Routine, Call, Reference, Literal, \
    Assignment, IfBlock, ArrayReference, Schedule, BinaryOperation, \
    StructureReference, FileContainer, CodeBlock
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, \
    ContainerSymbol, UnknownFortranType, ArgumentInterface, ImportInterface, \
    INTEGER_TYPE, CHARACTER_TYPE, ArrayType, BOOLEAN_TYPE, ScalarType
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
    >>> print(schedule.view())

    '''
    # Specify which OpenCL command queue to use for management operations like
    # data transfers when generating an OpenCL PSy-layer
    _OCL_MANAGEMENT_QUEUE = 1

    # TODO #1572: These are class attributes because multiple invokes may need
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
    # Reference to the OpenCL kernels file
    _kernels_file = None

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
                    f"OpenCL generation is currently only supported for the "
                    f"GOcean API but got an InvokeSchedule of type: "
                    f"'{type(node).__name__}'")
        else:
            raise TransformationError(
                f"Error in GOOpenCLTrans: the supplied node must be a (sub-"
                f"class of) InvokeSchedule but got {type(node)}")

        # Validate options map
        valid_options = ['end_barrier', 'enable_profiling', 'out_of_order']
        for key, value in options.items():
            if key in valid_options:
                # All current options should contain boolean values
                if not isinstance(value, bool):
                    raise TransformationError(
                        f"InvokeSchedule OpenCL option '{key}' should be a "
                        f"boolean.")
            else:
                raise TransformationError(
                    f"InvokeSchedule does not support the OpenCL option "
                    f"'{key}'. The supported options are: {valid_options}.")

        # Validate that the options are valid with previously generated OpenCL
        if self._transformed_invokes > 0:
            if ('enable_profiling' in options and
                    self._enable_profiling != options['enable_profiling']):
                raise TransformationError(
                    f"Can't generate an OpenCL Invoke with enable_profiling='"
                    f"{options['enable_profiling']}' since a previous "
                    f"transformation used a different value, and their OpenCL"
                    f" environments must match.")

            if ('out_of_order' in options and
                    self._out_of_order != options['out_of_order']):
                raise TransformationError(
                    f"Can't generate an OpenCL Invoke with out_of_order='"
                    f"{options['out_of_order']}' since a previous "
                    f"transformation used a different value, and their OpenCL "
                    f"environments must match.")

        # Now we need to check that none of the invoke arguments is a literal
        args = args_filter(node.args, arg_types=["scalar"])
        for arg in args:
            if arg.is_literal:
                raise TransformationError(
                    f"Cannot generate OpenCL for Invokes that contain kernel "
                    f"arguments which are a literal, but found the literal "
                    f"'{arg.name}' used as an argument in invoke "
                    f"'{node.name}'.")

        # Check that we can construct the PSyIR and SymbolTable of each of
        # the kernels in this Schedule. Also check that none of them access
        # any form of global data (that is not a routine argument).
        for kern in node.kernels():
            KernelTrans.validate(kern)
            ksched = kern.get_kernel_schedule()
            global_variables = ksched.symbol_table.imported_symbols
            if global_variables:
                raise TransformationError(
                    f"The Symbol Table for kernel '{kern.name}' contains the "
                    f"following symbols with 'global' scope: "
                    f"{[sym.name for sym in global_variables]}. An OpenCL "
                    f"kernel cannot call other kernels and all of the data it "
                    f"accesses must be passed by argument. Use the "
                    f"KernelImportsToArguments transformation to convert such "
                    f"symbols to kernel arguments first.")

        # In OpenCL all kernel loops should iterate the whole grid
        for kernel in node.kernels():
            inner_loop = kernel.ancestor(GOLoop)
            outer_loop = inner_loop.ancestor(GOLoop)
            if not (inner_loop.field_space == "go_every" and
                    outer_loop.field_space == "go_every" and
                    inner_loop.iteration_space == "go_all_pts" and
                    outer_loop.iteration_space == "go_all_pts"):
                raise TransformationError(
                    f"The kernel '{kernel.name}' does not iterate over all "
                    f"grid points. This is a necessary requirement for "
                    f"generating the OpenCL code and can be done by applying "
                    f"the GOMoveIterationBoundariesInsideKernelTrans to each "
                    f"kernel before the GOOpenCLTrans.")

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

        '''
        if not options:
            options = {}

        self.validate(node, options)
        api_config = Config.get().api_conf("gocean1.0")

        # Update class attributes
        if 'enable_profiling' in options:
            self._enable_profiling = options['enable_profiling']

        if 'out_of_order' in options:
            self._out_of_order = options['out_of_order']

        self._transformed_invokes += 1

        # Get end_barrier option
        end_barrier = options.get('end_barrier', True)

        # Update the maximum value that the queue_number have.
        for kernel in node.coded_kernels():
            self._max_queue_number = max(self._max_queue_number,
                                         kernel.opencl_options["queue_number"])

        # Insert, if they don't already exist, the necessary OpenCL helper
        # subroutines in the root Container.
        psy_init = self._insert_opencl_init_routine(node.root)
        init_grid = self._insert_initialise_grid_buffers(node.root)
        write_grid_buf = self._insert_write_grid_buffers(node.root)
        self._insert_ocl_read_from_device_function(node.root)
        self._insert_ocl_write_to_device_function(node.root)
        init_buf = self._insert_ocl_initialise_buffer(node.root)

        for kern in node.coded_kernels():
            self._insert_ocl_arg_setter_routine(node.root, kern)

        # Insert fortcl, clfortran and c_iso_binding import statement
        fortcl = ContainerSymbol("fortcl")
        node.symbol_table.add(fortcl)
        get_num_cmd_queues = RoutineSymbol(
                "get_num_cmd_queues", interface=ImportInterface(fortcl))
        get_cmd_queues = RoutineSymbol(
                "get_cmd_queues", interface=ImportInterface(fortcl))
        get_kernel_by_name = RoutineSymbol(
                "get_kernel_by_name", interface=ImportInterface(fortcl))
        node.symbol_table.add(get_num_cmd_queues)
        node.symbol_table.add(get_cmd_queues)
        node.symbol_table.add(get_kernel_by_name)
        clfortran = ContainerSymbol("clfortran")
        node.symbol_table.add(clfortran)
        cl_finish = RoutineSymbol(
                "clFinish", interface=ImportInterface(clfortran))
        cl_launch = RoutineSymbol(
                "clEnqueueNDRangeKernel",
                interface=ImportInterface(clfortran))
        node.symbol_table.add(cl_finish)
        node.symbol_table.add(cl_launch)
        iso_c_binding = ContainerSymbol("iso_c_binding")
        node.symbol_table.add(iso_c_binding)
        c_loc = RoutineSymbol(
                "C_LOC", interface=ImportInterface(iso_c_binding))
        c_null = DataSymbol(
                "C_NULL_PTR", datatype=INTEGER_TYPE,
                interface=ImportInterface(iso_c_binding))
        node.symbol_table.add(c_loc)
        node.symbol_table.add(c_null)

        # Include the check_status subroutine if we are in debug_mode
        if api_config.debug_mode:
            ocl_utils = ContainerSymbol("ocl_utils_mod")
            check_status = RoutineSymbol(
                "check_status", interface=ImportInterface(ocl_utils))
            node.symbol_table.add(ocl_utils)
            node.symbol_table.add(check_status)

        # Declare local variables needed by an OpenCL PSy-layer invoke
        qlist = node.symbol_table.new_symbol(
            "cmd_queues", symbol_type=DataSymbol,
            datatype=UnknownFortranType(
                "integer(kind=c_intptr_t), pointer, save :: cmd_queues(:)"),
            tag="opencl_cmd_queues")
        # 'first_time' needs to be an UnknownFortranType because it has SAVE
        # and initial value
        first = DataSymbol("first_time",
                           datatype=UnknownFortranType(
                               "logical, save :: first_time = .true."))
        node.symbol_table.add(first, tag="first_time")
        flag = node.symbol_table.new_symbol(
            "ierr", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
            tag="opencl_error")
        global_size = node.symbol_table.new_symbol(
            "globalsize", symbol_type=DataSymbol,
            datatype=UnknownFortranType(
                "integer(kind=c_size_t), target :: globalsize(2)"))
        local_size = node.symbol_table.new_symbol(
            "localsize", symbol_type=DataSymbol,
            datatype=UnknownFortranType(
                "integer(kind=c_size_t), target :: localsize(2)"))

        # Bring all the boundaries at the beginning (since we are going to
        # use them during the setup block - and they don't change)
        boundary_vars = []
        for tag, symbol in node.symbol_table.tags_dict.items():
            if tag.startswith(("xstart_", "xstop_", "ystart_", "ystop_")):
                boundary_vars.append(symbol)
        cursor = 0
        for assignment in node.walk(Assignment):
            if assignment.lhs.symbol in boundary_vars:
                node.children.insert(cursor, assignment.detach())
                cursor += 1

        # Create block of code to execute only the first time:
        setup_block = IfBlock.create(Reference(first), [])
        setup_block.preceding_comment = \
            "Initialise OpenCL runtime, kernels and buffers"
        node.children.insert(cursor, setup_block)
        setup_block.if_body.addchild(Call.create(psy_init, []))

        # Set up cmd_queues pointer
        ptree = Fortran2003.Pointer_Assignment_Stmt(
            f"{qlist.name} => {get_cmd_queues.name}()")
        cblock = CodeBlock([ptree], CodeBlock.Structure.STATEMENT)
        setup_block.if_body.addchild(cblock)

        # Declare and assign kernel pointers
        for kern in node.coded_kernels():
            name = "kernel_" + kern.name
            try:
                kpointer = node.symbol_table.lookup_with_tag(name)
            except KeyError:
                pointer_type = UnknownFortranType(
                    "INTEGER(KIND=c_intptr_t), TARGET, SAVE :: " + name)
                kpointer = DataSymbol(name, datatype=pointer_type)
                node.symbol_table.add(kpointer, tag=name)
            setup_block.if_body.addchild(
                Assignment.create(
                    Reference(kpointer),
                    Call.create(get_kernel_by_name,
                                [Literal(kern.name, CHARACTER_TYPE)])))

        # Traverse all arguments and make sure all the buffers are initialised
        initialised_fields = set()
        there_is_a_grid_buffer = False
        for kern in node.coded_kernels():
            for arg in kern.arguments.args:
                if arg.argument_type == "field":
                    field = node.symbol_table.lookup(arg.name)
                    if field not in initialised_fields:
                        # Call the init_buffer routine with this field
                        call = Call.create(init_buf, [Reference(field)])
                        setup_block.if_body.addchild(call)
                        initialised_fields.add(field)
                elif (arg.argument_type == "grid_property" and
                      not arg.is_scalar):
                    if not there_is_a_grid_buffer:
                        # Call the grid init_buffer routine
                        field = node.symbol_table.lookup(
                                kern.arguments.find_grid_access().name)
                        call = Call.create(init_grid, [Reference(field)])
                        setup_block.if_body.addchild(call)
                        there_is_a_grid_buffer = True
                if not arg.is_scalar:
                    # All buffers will be assigned to a local OpenCL memory
                    # object to easily reference them, make sure this local
                    # variable is declared in the Invoke.
                    name = arg.name + "_cl_mem"
                    try:
                        node.symbol_table.lookup_with_tag(name)
                    except KeyError:
                        node.symbol_table.new_symbol(
                            name, tag=name, symbol_type=DataSymbol,
                            datatype=UnknownFortranType(
                                "INTEGER(KIND=c_intptr_t) :: " + name))

        # Now call all the set_args routines because in some platforms (e.g.
        # in Xilinx FPGA) knowing which arguments each kernel is going to use
        # allows the write operation to place the data into the appropriate
        # memory bank.
        first_statement_comment = False
        kernel_names = set()
        for kern in node.coded_kernels():
            if kern.name not in kernel_names:
                kernel_names.add(kern.name)
                callblock = self._generate_set_args_call(kern, node.scope)
                for child in callblock.pop_all_children():
                    setup_block.if_body.addchild(child)
                    if not first_statement_comment:
                        child.preceding_comment = (
                            "Do a set_args now so subsequent writes place the "
                            "data appropriately")
                        first_statement_comment = True

        # Now we can insert calls to write_to_device method for each buffer
        # and the grid writing call if there is one (in a new first time block)
        first_statement_comment = False
        for field in initialised_fields:
            call = Call.create(
                RoutineSymbol(field.name+"%write_to_device"), [])
            setup_block.if_body.addchild(call)
            if not first_statement_comment:
                call.preceding_comment = "Write data to the device"
                first_statement_comment = True

        if there_is_a_grid_buffer:
            fieldarg = node.coded_kernels()[0].arguments.find_grid_access()
            field = node.symbol_table.lookup(fieldarg.name)
            call = Call.create(write_grid_buf, [Reference(field)])
            setup_block.if_body.addchild(call)

        # We will just mark the nodes we are replacing as deleting them inside
        # the loop would break the PSy-layer backward_dependency method in the
        # following iterations. We will detach all these nodes after the loop.
        nodes_to_detach = []

        # Transform each kernel call loop construct to its equivalent FortCL
        # statements
        for kern in node.coded_kernels():
            outerloop = kern.ancestor(GOLoop).ancestor(GOLoop)

            # Set up globalsize and localsize arrays
            garg = node.coded_kernels()[0].arguments.find_grid_access()
            num_x = api_config.grid_properties["go_grid_nx"].fortran\
                .format(garg.name)
            num_y = api_config.grid_properties["go_grid_ny"].fortran\
                .format(garg.name)
            assig = Assignment.create(
                    Reference(global_size),
                    Literal(f"(/{num_x}, {num_y}/)",
                            ArrayType(INTEGER_TYPE, [2])))
            node.children.insert(outerloop.position, assig)
            local_size_value = kern.opencl_options['local_size']
            assig = Assignment.create(
                    Reference(local_size),
                    Literal(f"(/{local_size_value}, 1/)",
                            ArrayType(INTEGER_TYPE, [2])))
            node.children.insert(outerloop.position, assig)

            # Check that the global_size is multiple of the local_size
            if api_config.debug_mode:
                fortran_reader = FortranReader()
                global_size_expr = fortran_reader.psyir_from_expression(
                        num_x, node.symbol_table)
                self._add_divisibility_check(node, outerloop.position,
                                             check_status, global_size_expr,
                                             local_size_value)

            # Retrieve kernel symbol
            kernelsym = node.symbol_table.lookup_with_tag(
                            "kernel_" + kern.name)

            # Choose the command queue number to which to dispatch this kernel.
            # We have do deal with possible dependencies to kernels dispatched
            # in different command queues as the order of execution is not
            # guaranteed.
            queue_number = kern.opencl_options['queue_number']
            cmd_queue = ArrayReference.create(
                    qlist, [Literal(str(queue_number), INTEGER_TYPE)])
            dependency = outerloop.backward_dependence()

            # If the dependency is a loop containing a kernel, add a barrier if
            # the previous kernels were dispatched in a different command queue
            if dependency:
                for kernel_dep in dependency.coded_kernels():
                    previous_queue = kernel_dep.opencl_options['queue_number']
                    if previous_queue != queue_number:
                        # If the backward dependency is being executed in
                        # another queue we add a barrier to make sure the
                        # previous kernel has finished before this halo
                        # exchange starts.
                        barrier = Assignment.create(
                                    Reference(flag),
                                    Call.create(cl_finish, [
                                        ArrayReference.create(qlist, [
                                            Literal(str(previous_queue),
                                                    INTEGER_TYPE)])]))
                        node.children.insert(outerloop.position, barrier)

            # If the dependency is something other than a kernel, currently we
            # dispatch everything else to queue _OCL_MANAGEMENT_QUEUE, so add a
            # barrier if this kernel is not on queue _OCL_MANAGEMENT_QUEUE.
            if dependency and not dependency.coded_kernels() and \
                    queue_number != self._OCL_MANAGEMENT_QUEUE:
                barrier = Assignment.create(
                            Reference(flag),
                            Call.create(cl_finish, [
                                ArrayReference.create(qlist, [
                                    Literal(str(self._OCL_MANAGEMENT_QUEUE),
                                            INTEGER_TYPE)])]))
                node.children.insert(outerloop.position, barrier)

            # Check that everything has succeeded before the kernel launch
            if api_config.debug_mode:
                self._add_ready_check(node, outerloop.position, check_status,
                                      kern.name, flag, cl_finish,
                                      cmd_queue.copy())
            callblock = self._generate_set_args_call(kern, node.scope)
            for child in callblock.pop_all_children():
                node.children.insert(outerloop.position, child)

            # Then we call the clEnqueueNDRangeKernel
            assig = Assignment.create(
                        Reference(flag),
                        Call.create(cl_launch, [
                            # OpenCL Command Queue
                            cmd_queue,
                            # OpenCL Kernel object
                            Reference(kernelsym),
                            # Number of work dimensions
                            Literal("2", INTEGER_TYPE),
                            # Global offset (if NULL the global IDs start at
                            # offset (0,0,0))
                            Reference(c_null),
                            # Global work size
                            Call.create(c_loc, [Reference(global_size)]),
                            # Local work size
                            Call.create(c_loc, [Reference(local_size)]),
                            # Number of events in wait list
                            Literal("0", INTEGER_TYPE),
                            # Event wait list that need to be completed before
                            # this kernel
                            Reference(c_null),
                            # Event that identifies this kernel completion
                            Reference(c_null)]))
            assig.preceding_comment = "Launch the kernel"
            node.children.insert(outerloop.position, assig)
            self._insert_kernel_code_in_opencl_file(kern)

            # Add additional checks if we are in debug mode
            if api_config.debug_mode:
                self._add_kernel_check(node, outerloop.position, check_status,
                                       kern.name, flag, cl_finish,
                                       cmd_queue.copy())

            nodes_to_detach.append(outerloop)

        # If we execute the kernels asynchronously, we need to add wait
        # statements before the halo exchanges to guarantee that the data
        # has been updated
        for possible_dependent_node in node.walk(HaloExchange):
            # The backward_dependences returns the last Loop with a kernel
            # that has a dependency with this halo exchange
            dependency = possible_dependent_node.backward_dependence()
            if dependency:
                for kernel_dep in dependency.coded_kernels():
                    previous_queue = kernel_dep.opencl_options['queue_number']
                    if previous_queue != self._OCL_MANAGEMENT_QUEUE:
                        # If the backward dependency is being executed in
                        # another queue we add a barrier to make sure the
                        # previous kernel has finished before this one starts.
                        barrier = Assignment.create(
                                    Reference(flag),
                                    Call.create(cl_finish, [
                                        ArrayReference.create(qlist, [
                                            Literal(str(previous_queue),
                                                    INTEGER_TYPE)])]))
                        pos = possible_dependent_node.position
                        node.children.insert(pos, barrier)

        for node_to_detach in nodes_to_detach:
            node_to_detach.detach()

        if end_barrier:
            self._add_end_barrier(node, flag, cl_finish, qlist)

        # And at the very end always makes sure that first_time value is False
        assign = Assignment.create(Reference(first),
                                   Literal("false", BOOLEAN_TYPE))
        assign.preceding_comment = "Unset the first time flag"
        node.addchild(assign)

        self._output_opencl_kernels_file()

    def _add_end_barrier(self, node, flag, cl_finish, qlist):
        ''' Append into the given node a OpenCL Wait operation for each of
        the OpenCL queues in use.

        :param node: PSyIR node where to append the barrier.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param flag: PSyIR symbol to use as flag.
        :type flag: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param cl_finish: PSyIR symbol of the barrier routine.
        :type cl_finish: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param qlist: PSyIR symbol of the OpenCL queues array.
        :type qlist: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        # We need a clFinish for each of the queues in the implementation
        added_comment = False
        for num in range(1, self._max_queue_number + 1):
            queue = ArrayReference.create(qlist, [Literal(str(num),
                                                  INTEGER_TYPE)])
            node.addchild(
                Assignment.create(
                    Reference(flag), Call.create(cl_finish, [queue])))
            if not added_comment:
                node.children[-1].preceding_comment = \
                    "Wait until all kernels have finished"
                added_comment = True

    @staticmethod
    def _add_divisibility_check(node, position, check_status, global_size_expr,
                                local_size):
        ''' Insert into node a check that the global_size is exactly
        divisible by the local size.

        :param node: where to insert the divisibility check.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param int position: location where to insert the divisibilitay check.
        :param check_status: PSyIR symbol of the check routine.
        :type check_status: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param global_size_expr: PSyIR representing the global_size.
        :type global_size_expr: :py:class:`psyclone.psyir.nodes.DataNode`
        :param int local_size: size of the OpenCL local work_group.

        '''
        check = BinaryOperation.create(
                    BinaryOperation.Operator.NE,
                    BinaryOperation.create(
                        BinaryOperation.Operator.REM,
                        global_size_expr,
                        Literal(str(local_size), INTEGER_TYPE)
                        ),
                    Literal("0", INTEGER_TYPE))
        message = ("Global size is not a multiple of local size ("
                   "mandatory in OpenCL < 2.0).")
        error = Call.create(check_status,
                            [Literal(message, CHARACTER_TYPE),
                             Literal("-1", INTEGER_TYPE)])
        ifblock = IfBlock.create(check, [error])
        node.children.insert(position, ifblock)

    @staticmethod
    def _add_kernel_check(node, position, check_status, kernel_name,
                          flag, cl_finish, cmd_queue):
        ''' Insert into node a check that the kernel has been launched and
        has been executed successfully.

        :param node: where to insert the kernel check.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param int position: location where to insert the kernel check.
        :param check_status: PSyIR symbol of the check routine.
        :type check_status: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param str kernel_name: name of the kernel being checked.
        :param flag: PSyIR symbol to use as flag.
        :type flag: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param cl_finish: PSyIR symbol of the barrier routine.
        :type cl_finish: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param cmd_queue: PSyIR symbol of the OpenCL command queues array.
        :type cmd_queue: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        # First check the launch return value
        message = Literal(f"{kernel_name} clEnqueueNDRangeKernel",
                          CHARACTER_TYPE)
        check = Call.create(check_status, [message, Reference(flag)])
        node.children.insert(position, check)

        # Then add a barrier
        barrier = Assignment.create(
                    Reference(flag),
                    Call.create(cl_finish, [cmd_queue]))
        node.children.insert(position + 1, barrier)

        # And check the kernel executed successfully
        message = Literal(f"Errors during {kernel_name}", CHARACTER_TYPE)
        check = Call.create(check_status, [message, Reference(flag)])
        node.children.insert(position + 2, check)

    @staticmethod
    def _add_ready_check(node, position, check_status, kernel_name,
                         flag, cl_finish, cmd_queue):
        ''' Insert into node a check that verifies if everything in the
        command queues previous to a kernel launch has completed successfully.

        :param node: where to insert the kernel check.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param int position: location where to insert the kernel check.
        :param check_status: PSyIR symbol of the check routine.
        :type check_status: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param str kernel_name: name of the kernel being checked.
        :param flag: PSyIR symbol to use as flag.
        :type flag: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param cl_finish: PSyIR symbol of the barrier routine.
        :type cl_finish: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param cmd_queue: PSyIR symbol of the OpenCL command queues array.
        :type cmd_queue: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        barrier = Assignment.create(
                    Reference(flag),
                    Call.create(cl_finish, [cmd_queue]))
        node.children.insert(position, barrier)
        message = Literal(f"Errors before {kernel_name} launch",
                          CHARACTER_TYPE)
        check = Call.create(check_status, [message, Reference(flag)])
        node.children.insert(position + 1, check)

    def _insert_kernel_code_in_opencl_file(self, kernel):
        ''' Insert the given kernel into a OpenCL file. For this we need
        to remove the 'go_wp' precision symbol which can't be generated
        by OpenCL. We assume 'go_wp' is a OpenCL double.

        :param kernel: the kernel to insert.
        :type kernel: :py:class:`psyclone.psyir.nodes.KernelSchedule`

        '''
        if not self._kernels_file:
            self._kernels_file = FileContainer("opencl_kernels")

        # Create a copy of the kernel and remove precision symbols since they
        # are not supported in the OpenCL backend.
        kernel_copy = kernel.get_kernel_schedule().copy()
        symtab = kernel_copy.symbol_table

        # TODO #898: Removing symbols is not properly supported by PSyIR
        # because we have to deal with all references to it. In this case we
        # implement manually a conversion of all 'go_wp' to a double precision
        # and remove the symbol because we guarantee that it just appear in the
        # declarations of other symbols (symtab.datasymbols).
        # pylint: disable=protected-access
        for sym in symtab.datasymbols:
            # Not all types have the 'precision' attribute (e.g. DeferredType)
            if (hasattr(sym.datatype, "precision") and
                    isinstance(sym.datatype.precision, DataSymbol)):
                sym.datatype._precision = ScalarType.Precision.DOUBLE

        if 'go_wp' in symtab:
            del symtab._symbols['go_wp']

        # Insert kernel in the OpenCL kernels file if it doesn't already exist
        for routine in self._kernels_file.walk(Routine):
            if routine.name == kernel.name:
                break  # if it exist re-use existing one
                # TODO 1572: Here we assume that in the same Invoke (scope) a
                # kernel with the same name will be the same kernel, but that
                # may not be true when doing multiple invokes.
        else:
            self._kernels_file.addchild(kernel_copy)

    def _output_opencl_kernels_file(self):
        ''' Write the OpenCL kernels to a file using the OpenCL backend.

        '''
        # TODO 1013: The code below duplicates some logic of the CodedKern
        # rename_and_write method. Ideally this should be moved out of
        # the AST and transformations and put into some kind of IOManager.

        ocl_writer = OpenCLWriter(kernels_local_size=64)
        new_kern_code = ocl_writer(self._kernels_file)

        fdesc = None
        name_idx = -1
        while not fdesc:
            name_idx += 1
            new_name = f"opencl_kernels_{name_idx}.cl"

            try:
                # Atomically attempt to open the new kernel file (in case
                # this is part of a parallel build)
                fdesc = os.open(
                    os.path.join(Config.get().kernel_output_dir, new_name),
                    os.O_CREAT | os.O_WRONLY | os.O_EXCL)
            except (OSError, IOError):
                # The os.O_CREATE and os.O_EXCL flags in combination mean
                # that open() raises an error if the file exists
                continue

        # Write the modified AST out to file
        os.write(fdesc, new_kern_code.encode())
        # Close the new kernel file
        os.close(fdesc)

    @staticmethod
    def _generate_set_args_call(kernel, scope):
        '''
        Generate the Call statement to the set_args subroutine for the
        provided kernel.

        :param kernel: the kernel for which to generate a call to its \
            arg_setter subroutine.
        :type kernel: :py:class:`psyclone.psyGen.CodedKern`
        :param scope: The node representing the scope where the call \
            statements will be inserted.
        :type scope: :py:class:`psyclone.psyir.nodes.ScopingNode`

        :returns: a block of statements that represent the set_args call
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        call_block = Schedule()

        # Retrieve symbol table and kernel symbol
        symtab = scope.symbol_table
        kernelsym = symtab.lookup_with_tag("kernel_" + kernel.name)

        # Find the symbol that defines each boundary for this kernel.
        # In OpenCL the iteration boundaries are passed as arguments to the
        # kernel because the global work size may exceed the dimensions and
        # therefore the updates outside the boundaries should be masked.
        # If any of the boundaries is not found, it can not proceed.
        boundaries = []
        try:
            for boundary in ["xstart", "xstop", "ystart", "ystop"]:
                tag = boundary + "_" + kernel.name
                symbol = symtab.lookup_with_tag(tag)
                boundaries.append(symbol.name)
        except KeyError as err:
            raise GenerationError(
                f"Boundary symbol tag '{tag}' not found while generating the "
                f"OpenCL code for kernel '{kernel.name}'. Make sure to apply "
                f"the GOMoveIterationBoundariesInsideKernelTrans before "
                f"attempting the OpenCL code generation.") from err

        api_config = Config.get().api_conf("gocean1.0")
        # Prepare the argument list for the set_args routine
        arguments = [Reference(kernelsym)]
        for arg in kernel.arguments.args:
            if arg.argument_type == "scalar":
                if arg.name in boundaries:
                    # Boundary values are 0-indexed in OpenCL and 1-indexed in
                    # PSyIR, therefore we need to subtract 1
                    bop = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                                 arg.psyir_expression(),
                                                 Literal("1", INTEGER_TYPE))
                    arguments.append(bop)
                else:
                    arguments.append(arg.psyir_expression())
            elif arg.argument_type == "field":
                # Cast buffer to cl_mem type expected by OpenCL
                field = symtab.lookup(arg.name)
                symbol = symtab.lookup_with_tag(arg.name + "_cl_mem")
                source = StructureReference.create(field, ['device_ptr'])
                dest = Reference(symbol)
                bop = BinaryOperation.create(BinaryOperation.Operator.CAST,
                                             source, dest)
                assig = Assignment.create(dest.copy(), bop)
                call_block.addchild(assig)
                arguments.append(Reference(symbol))
            elif arg.argument_type == "grid_property":
                garg = kernel.arguments.find_grid_access()
                if arg.is_scalar:
                    arguments.append(
                        StructureReference.create(
                            symtab.lookup(garg.name),
                            api_config.grid_properties[arg._property_name]
                            .fortran.split('%')[1:]
                        ))
                else:
                    # Cast grid buffer to cl_mem type expected by OpenCL
                    device_grid_property = arg.name + "_device"
                    field = symtab.lookup(garg.name)
                    source = StructureReference.create(
                                field, ['grid', device_grid_property])
                    symbol = symtab.lookup_with_tag(arg.name + "_cl_mem")
                    dest = Reference(symbol)
                    bop = BinaryOperation.create(BinaryOperation.Operator.CAST,
                                                 source, dest)
                    assig = Assignment.create(dest.copy(), bop)
                    call_block.addchild(assig)
                    arguments.append(Reference(symbol))

        call_symbol = symtab.lookup_with_tag(kernel.name + "_set_args")
        call_block.addchild(Call.create(call_symbol, arguments))
        return call_block

    @staticmethod
    def _insert_ocl_arg_setter_routine(node, kernel):
        '''
        Returns the symbol of the subroutine that sets the OpenCL kernel
        arguments for the provided PSy-layer kernel using FortCL. If the
        subroutine doesn't exist it also generates it.

        :param node: the container where the new subroutine will be inserted.
        :type node: :py:class:`psyclone.psyir.nodes.Container`
        :param kernel: the kernel call for which to provide the arg_setter \
                       subroutine.
        :type kernel: :py:class:`psyclone.psyGen.CodedKern`

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
            emsg = f"clSetKernelArg: arg {index} of {kernel.name}"
            call = Call.create(check_status, [Literal(emsg, CHARACTER_TYPE),
                                              Reference(ierr)])
            argsetter.addchild(call)

        argsetter.children[0].preceding_comment = \
            f"Set the arguments for the {kernel.name} OpenCL Kernel"

        # Add the subroutine as child of the provided node
        node.addchild(argsetter)

        return node.symbol_table.lookup_with_tag(sub_name)

    def _insert_opencl_init_routine(self, node):
        '''
        Returns the symbol of the subroutine that initialises the OpenCL
        environment using FortCL. If the subroutine doesn't exist it also
        generates it.

        :param node: the container where the new subroutine will be inserted.
        :type node: :py:class:`psyclone.psyir.nodes.Container`

        :returns: the symbol representing the OpenCL initialisation subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        symtab = node.symbol_table
        try:
            # TODO #1572: The ocl_init routine may need to be regenerated if
            # there are multiple Invokes because _max_queue_number may have
            # increased and we need to load the kernels of both invokes.
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
                f"ocl_device_num = mod(get_rank()-1, {devices_per_node}) + 1"

        # Get a set of all kernel names in the Container. This implementation
        # currently assumes all of them will be available in OpenCL
        unique_kernels = {kernel.name for kernel in node.coded_kernels()}

        # Code of the subroutine in Fortran
        code = f'''
        subroutine psy_init()
          {additional_uses}
          use fortcl, only: ocl_env_init, add_kernels
          character(len=30) kernel_names({len(unique_kernels)})
          integer :: ocl_device_num=1
          logical, save :: initialised=.false.
          ! Check to make sure we only execute this routine once
          if (.not. initialised) then
            initialised = .true.
            ! Initialise the opencl environment/device
            {additional_stmts}
            call ocl_env_init({self._max_queue_number}, ocl_device_num, &
                {".true." if self._enable_profiling else ".false."}, &
                {".true." if self._out_of_order else ".false."})
            ! The kernels this psy layer module requires
        '''

        for index, kernel_name in enumerate(unique_kernels):
            code += f"kernel_names({index + 1}) = \"{kernel_name}\"\n"

        code += f'''\
            ! Create the opencl kernel objects. This expects to find all of
            ! the compiled kernels in FORTCL_KERNELS_FILE environment variable
            call add_kernels({len(unique_kernels)}, kernel_names)
          end if
        end subroutine psy_init'''

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
        :type node: :py:class:`psyclone.psyir.nodes.Container`

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
        code = f'''
        subroutine initialise_device_grid(field)
            USE fortcl, ONLY: create_ronly_buffer
            use field_mod
            type(r2d_field), intent(inout), target :: field
            integer(kind=c_size_t) size_in_bytes
            IF (.not. c_associated({int_arrays[0]}_device)) THEN
                ! Create integer grid fields
                size_in_bytes = int({num_x}*{num_y}, 8) * &
                    c_sizeof({int_arrays[0]}(1,1))
        '''

        for int_array in int_arrays:
            code += f'''
                {int_array}_device = transfer( &
                    create_ronly_buffer(size_in_bytes), {int_array}_device)
            '''

        code += f'''
                ! Create real grid buffers
                size_in_bytes = int({num_x} * {num_y}, 8) * &
                                    c_sizeof({real_arrays[0]}(1,1))
        '''

        for real_array in real_arrays:
            code += f'''
                {real_array}_device = transfer( &
                    create_ronly_buffer(size_in_bytes), {real_array}_device)
            '''

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
        :type node: :py:class:`psyclone.psyir.nodes.Container`

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
        code = f'''
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
            size_in_bytes = int({num_x} * {num_y}, 8) * &
                            c_sizeof(field%grid%tmask(1,1))
            cl_mem = transfer(field%grid%tmask_device, cl_mem)
            ierr = clEnqueueWriteBuffer( &
                        cmd_queues({self._OCL_MANAGEMENT_QUEUE}), &
                        cl_mem, CL_TRUE, 0_8, size_in_bytes, &
                        C_LOC(field%grid%tmask), 0, C_NULL_PTR, C_NULL_PTR)
            CALL check_status("clEnqueueWriteBuffer tmask", ierr)
            ! Real grid buffers
            size_in_bytes = int({num_x} * {num_y}, 8) * &
                            c_sizeof(field%grid%area_t(1,1))
        '''
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
        :type node: :py:class:`psyclone.psyir.nodes.Container`

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
        code = f'''
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
                    ierr = clEnqueueReadBuffer( &
                        cmd_queues({self._OCL_MANAGEMENT_QUEUE}), cl_mem, &
                        CL_FALSE, offset_in_bytes, size_in_bytes, &
                        C_LOC(to(startx, i)), 0, C_NULL_PTR, C_NULL_PTR)
                    CALL check_status("clEnqueueReadBuffer", ierr)
                enddo
                if (blocking) then
                    CALL check_status("clFinish on read", &
                        clFinish(cmd_queues({self._OCL_MANAGEMENT_QUEUE})))
                endif
            else
                ! Copy across the whole starty:starty+ny rows in a single
                ! copy operation.
                size_in_bytes = int(size(to, 1) * ny, 8) * c_sizeof(to(1,1))
                offset_in_bytes = int(size(to,1)*(starty-1), 8) &
                                  * c_sizeof(to(1,1))
                ierr = clEnqueueReadBuffer( &
                    cmd_queues({self._OCL_MANAGEMENT_QUEUE}), cl_mem, &
                    CL_TRUE, offset_in_bytes, size_in_bytes, &
                    C_LOC(to(1,starty)), 0, C_NULL_PTR, C_NULL_PTR)
                CALL check_status("clEnqueueReadBuffer", ierr)
            endif
        end subroutine read_sub
        '''

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
        :type node: :py:class:`psyclone.psyir.nodes.Container`

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
        code = f'''
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
                    ierr = clEnqueueWriteBuffer( &
                        cmd_queues({self._OCL_MANAGEMENT_QUEUE}), cl_mem, &
                        CL_FALSE, offset_in_bytes, size_in_bytes, &
                        C_LOC(from(startx, i)), 0, C_NULL_PTR, C_NULL_PTR)
                    CALL check_status("clEnqueueWriteBuffer", ierr)
                enddo
                if (blocking) then
                    CALL check_status("clFinish on write", &
                        clFinish(cmd_queues({self._OCL_MANAGEMENT_QUEUE})))
                endif
            else
                ! Copy across the whole starty:starty+ny rows in a single
                ! copy operation.
                size_in_bytes = int(size(from,1) * ny, 8) * c_sizeof(from(1,1))
                offset_in_bytes = int(size(from,1) * (starty-1)) &
                                  * c_sizeof(from(1,1))
                ierr = clEnqueueWriteBuffer(&
                    cmd_queues({self._OCL_MANAGEMENT_QUEUE}), cl_mem, &
                    CL_TRUE, offset_in_bytes, size_in_bytes, &
                    C_LOC(from(1, starty)), 0, C_NULL_PTR, C_NULL_PTR)
                CALL check_status("clEnqueueWriteBuffer", ierr)
            endif
        end subroutine write_sub
        '''

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
        :type node: :py:class:`psyclone.psyir.nodes.Container`
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
        code = f'''
        subroutine initialise_device_buffer(field)
            USE fortcl, ONLY: create_rw_buffer
            use field_mod
            type(r2d_field), intent(inout), target :: field
            integer(kind=c_size_t) size_in_bytes
            IF (.NOT. field%data_on_device) THEN
                size_in_bytes = int({num_x} * {num_y}, 8) * &
                                    c_sizeof({host_buff}(1,1))
                ! Create buffer on device, we store it without type information
                ! on the dl_esm_inf pointer (transfer/static_cast to void*)
                field%device_ptr = transfer( &
                    create_rw_buffer(size_in_bytes), &
                    field%device_ptr)
                field%data_on_device = .true.
                field%read_from_device_f => {read_fp}
                field%write_to_device_f => {write_fp}
            END IF
        end subroutine initialise_device_buffer
        '''

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
