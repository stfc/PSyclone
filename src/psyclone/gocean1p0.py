# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Authors R. Ford and A. R. Porter, STFC Daresbury Lab

'''This module implements the PSyclone GOcean 1.0 API by specialising
    the required base classes for both code generation (PSy, Invokes,
    Invoke, Schedule, Loop, Kern, Arguments and KernelArgument)
    and parsing (Descriptor and KernelType). It adds a
    GOKernelGridArgument class to capture information on kernel arguments
    that supply properties of the grid (and are generated in the PSy
    layer).

'''

from psyclone.parse import Descriptor, KernelType, ParseError
from psyclone.psyGen import PSy, Invokes, Invoke, Schedule, \
    Loop, Kern, Arguments, Argument, KernelArgument, GenerationError
import psyclone.expression as expr

# The different grid-point types that a field can live on
VALID_FIELD_GRID_TYPES = ["cu", "cv", "ct", "cf", "every"]

# The two scalar types we support
VALID_SCALAR_TYPES = ["i_scalar", "r_scalar"]

# Index-offset schemes (for the Arakawa C-grid)
VALID_OFFSET_NAMES = ["offset_se", "offset_sw",
                      "offset_ne", "offset_nw", "offset_any"]

# The offset schemes for which we can currently generate constant
# loop bounds in the PSy layer
SUPPORTED_OFFSETS = ["offset_ne", "offset_sw", "offset_any"]

# The sets of grid points that a kernel may operate on
VALID_ITERATES_OVER = ["all_pts", "internal_pts", "external_pts"]

# Valid values for the type of access a kernel argument may have
VALID_ARG_ACCESSES = ["read", "write", "readwrite"]

# The list of valid stencil properties. We currently only support
# pointwise. This property could probably be removed from the
# GOcean API altogether.
VALID_STENCIL_NAMES = ["pointwise"]

# A dictionary giving the mapping from meta-data names for
# properties of the grid to their names in the Fortran grid_type.
GRID_PROPERTY_DICT = {"grid_area_t": "area_t",
                      "grid_area_u": "area_u",
                      "grid_area_v": "area_v",
                      "grid_mask_t": "tmask",
                      "grid_dx_t": "dx_t",
                      "grid_dx_u": "dx_u",
                      "grid_dx_v": "dx_v",
                      "grid_dy_t": "dy_t",
                      "grid_dy_u": "dy_u",
                      "grid_dy_v": "dy_v",
                      "grid_lat_u": "gphiu",
                      "grid_lat_v": "gphiv",
                      "grid_dx_const": "dx",
                      "grid_dy_const": "dy"}

# The valid types of loop. In this API we expect only doubly-nested
# loops.
VALID_LOOP_TYPES = ["inner", "outer"]


class GOPSy(PSy):
    ''' The GOcean 1.0 specific PSy class. This creates a GOcean specific
        invokes object (which controls all the required invocation calls).
        Also overrides the PSy gen method so that we generate GOcean-
        specific PSy module code. '''
    def __init__(self, invoke_info):
        PSy.__init__(self, invoke_info)
        self._invokes = GOInvokes(invoke_info.calls)

    @property
    def gen(self):
        '''
        Generate PSy code for the GOcean api v.1.0.

        :rtype: ast

        '''
        from psyclone.f2pygen import ModuleGen, UseGen

        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include the kind_params module
        psy_module.add(UseGen(psy_module, name="kind_params_mod"))
        # include the field_mod module
        psy_module.add(UseGen(psy_module, name="field_mod"))
        # add in the subroutines for each invocation
        self.invokes.gen_code(psy_module)
        # inline kernels where requested
        self.inline(psy_module)
        return psy_module.root


class GOInvokes(Invokes):
    ''' The GOcean specific invokes class. This passes the GOcean specific
        invoke class to the base class so it creates the one we require. '''
    def __init__(self, alg_calls):
        if False:  # pylint: disable=using-constant-test
            self._0_to_n = GOInvoke(None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, GOInvoke)

        index_offsets = []
        # Loop over all of the kernels in all of the invoke() calls
        # and check that they work on compatible grid-index offsets.
        # Strictly speaking this check should be done in the parsing
        # code since it is a check on the correctness of the meta-data.
        # However, that would require a fundamental change to the parsing
        # code since it requires information  on all of the invokes and
        # kernels in an application. Therefore it is much simpler to
        # do it here where we have easy access to that information.
        for invoke in self.invoke_list:
            for kern_call in invoke.schedule.kern_calls():
                # We only care if the index offset is not offset_any (since
                # that is compatible with any other offset)
                if kern_call.index_offset != "offset_any":
                    # Loop over the offsets we've seen so far
                    for offset in index_offsets:
                        if offset != kern_call.index_offset:
                            raise GenerationError(
                                "Meta-data error in kernel {0}: "
                                "INDEX_OFFSET of '{1}' does not match that "
                                "({2}) of other kernels. This is not "
                                "supported.".format(kern_call.name,
                                                    kern_call.index_offset,
                                                    offset))
                    # Append the index-offset of this kernel to the list of
                    # those seen so far
                    index_offsets.append(kern_call.index_offset)


class GOInvoke(Invoke):
    ''' The GOcean specific invoke class. This passes the GOcean specific
        schedule class to the base class so it creates the one we require.
        A set of GOcean infrastructure reserved names are also passed to
        ensure that there are no name clashes. Also overrides the gen_code
        method so that we generate GOcean specific invocation code and
        provides three methods which separate arguments that are arrays from
        arguments that are {integer, real} scalars. '''
    def __init__(self, alg_invocation, idx):
        if False:  # pylint: disable=using-constant-test
            self._schedule = GOSchedule(None)  # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, GOSchedule)

    @property
    def unique_args_arrays(self):
        ''' find unique arguments that are arrays (defined as those that are
            field objects as opposed to scalars or properties of the grid). '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'field' and arg.name not in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_rscalars(self):
        ''' find unique arguments that are scalars of type real (defined
            as those that are r_scalar 'space'. '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'scalar' and \
                   arg.space.lower() == "r_scalar" and \
                   not arg.is_literal and arg.name not in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_iscalars(self):
        ''' find unique arguments that are scalars of type integer (defined
            as those that are i_scalar 'space'). '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'scalar' and \
                   arg.space.lower() == "i_scalar" and \
                   not arg.is_literal and arg.name not in result:
                    result.append(arg.name)
        return result

    def gen_code(self, parent):
        ''' Generates GOcean specific invocation code (the subroutine called
            by the associated invoke call in the algorithm layer). This
            consists of the PSy invocation subroutine and the declaration of
            its arguments.'''
        from psyclone.f2pygen import SubroutineGen, DeclGen, TypeDeclGen, \
            CommentGen, AssignGen
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names)
        parent.add(invoke_sub)

        # add declarations for the variables holding the upper bounds
        # of loops in i and j
        if self.schedule.const_loop_bounds:
            invoke_sub.add(DeclGen(invoke_sub, datatype="INTEGER",
                                   entity_decls=[self.schedule.iloop_stop,
                                                 self.schedule.jloop_stop]))

        # Generate the code body of this subroutine
        self.schedule.gen_code(invoke_sub)

        # add the subroutine argument declarations for fields
        if len(self.unique_args_arrays) > 0:
            my_decl_arrays = TypeDeclGen(invoke_sub, datatype="r2d_field",
                                         intent="inout",
                                         entity_decls=self.unique_args_arrays)
            invoke_sub.add(my_decl_arrays)

        # add the subroutine argument declarations for real scalars
        if len(self.unique_args_rscalars) > 0:
            my_decl_rscalars = DeclGen(invoke_sub, datatype="REAL",
                                       intent="inout", kind="wp",
                                       entity_decls=self.unique_args_rscalars)
            invoke_sub.add(my_decl_rscalars)
        # add the subroutine argument declarations for integer scalars
        if len(self.unique_args_iscalars) > 0:
            my_decl_iscalars = DeclGen(invoke_sub, datatype="INTEGER",
                                       intent="inout",
                                       entity_decls=self.unique_args_iscalars)
            invoke_sub.add(my_decl_iscalars)

        if self._schedule.const_loop_bounds and \
           len(self.unique_args_arrays) > 0:

            # Look-up the loop bounds using the first field object in the
            # list
            sim_domain = self.unique_args_arrays[0] +\
                "%grid%simulation_domain%"
            position = invoke_sub.last_declaration()

            invoke_sub.add(CommentGen(invoke_sub, ""),
                           position=["after", position])
            invoke_sub.add(AssignGen(invoke_sub, lhs=self.schedule.jloop_stop,
                                     rhs=sim_domain+"ystop"),
                           position=["after", position])
            invoke_sub.add(AssignGen(invoke_sub, lhs=self.schedule.iloop_stop,
                                     rhs=sim_domain+"xstop"),
                           position=["after", position])
            invoke_sub.add(CommentGen(invoke_sub, " Look-up loop bounds"),
                           position=["after", position])
            invoke_sub.add(CommentGen(invoke_sub, ""),
                           position=["after", position])


class GOSchedule(Schedule):
    ''' The GOcean specific schedule class. We call the base class
    constructor and pass it factories to create GO-specific calls to both
    user-supplied kernels and built-ins. '''

    def __init__(self, alg_calls):
        Schedule.__init__(self, GOKernCallFactory, GOBuiltInCallFactory,
                          alg_calls)

        # Configuration of this Schedule - we default to having
        # constant loop bounds. If we end up having a long list
        # of configuration member variables here we may want
        # to create a a new ScheduleConfig object to manage them.
        self._const_loop_bounds = True

    def view(self, indent=0):
        ''' Print a representation of this GOSchedule '''
        print self.indent(indent) + self.coloured_text + "[invoke='" + \
            self.invoke.name + "',Constant loop bounds=" + \
            str(self._const_loop_bounds) + "]"
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this GOSchedule '''
        result = "GOSchedule(Constant loop bounds=" + \
                 str(self._const_loop_bounds) + "):\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result

    @property
    def coloured_text(self):
        ''' Return the name of this object with control-codes for
        display in terminals that support colour '''
        from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
        return colored("GOSchedule", SCHEDULE_COLOUR_MAP["Schedule"])

    @property
    def iloop_stop(self):
        '''Returns the variable name to use for the upper bound of inner
        loops if we're generating loops with constant bounds. Raises
        an error if constant bounds are not being used.

        '''
        if self._const_loop_bounds:
            return "istop"
        else:
            raise GenerationError(
                "Refusing to supply name of inner loop upper bound "
                "because constant loop bounds are not being used.")

    @property
    def jloop_stop(self):
        '''Returns the variable name to use for the upper bound of outer
        loops if we're generating loops with constant bounds. Raises
        an error if constant bounds are not being used.

        '''
        if self._const_loop_bounds:
            return "jstop"
        else:
            raise GenerationError(
                "Refusing to supply name of outer loop upper bound "
                "because constant loop bounds are not being used.")

    @property
    def const_loop_bounds(self):
        ''' Returns True if constant loop bounds are enabled for this
        schedule. Returns False otherwise. '''
        return self._const_loop_bounds

    @const_loop_bounds.setter
    def const_loop_bounds(self, obj):
        ''' Set whether the Schedule will use constant loop bounds or
        will look them up from the field object for every loop '''
        self._const_loop_bounds = obj


class GOLoop(Loop):
    ''' The GOcean specific Loop class. This passes the GOcean specific
        single loop information to the base class so it creates the one we
        require. Adds a GOcean specific setBounds method which tells the loop
        what to iterate over. Need to harmonise with the topology_name method
        in the Dynamo api. '''
    def __init__(self, parent=None,
                 topology_name="", loop_type=""):
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        self.loop_type = loop_type

        # We set the loop variable name in the constructor so that it is
        # available when we're determining which vars should be OpenMP
        # PRIVATE (which is done *before* code generation is performed)
        if self.loop_type == "inner":
            self._variable_name = "i"
        elif self.loop_type == "outer":
            self._variable_name = "j"
        else:
            raise GenerationError(
                "Invalid loop type of '{0}'. Expected one of {1}".
                format(self._loop_type, VALID_LOOP_TYPES))

        # Create a dictionary to simplify the business of looking-up
        # loop bounds
        self._bounds_lookup = {}
        for grid_offset in SUPPORTED_OFFSETS:
            self._bounds_lookup[grid_offset] = {}
            for gridpt_type in VALID_FIELD_GRID_TYPES:
                self._bounds_lookup[grid_offset][gridpt_type] = {}
                for itspace in VALID_ITERATES_OVER:
                    self._bounds_lookup[grid_offset][gridpt_type][itspace] = {}

        # Loop bounds for a mesh with NE offset
        self._bounds_lookup['offset_ne']['ct']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_ne']['ct']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': ""},
             'outer': {'start': "2", 'stop': ""}}
        self._bounds_lookup['offset_ne']['cu']['all_pts'] = \
            {'inner': {'start': "1", 'stop': ""},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_ne']['cu']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': "-1"},
             'outer': {'start': "2", 'stop': ""}}
        self._bounds_lookup['offset_ne']['cv']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': ""}}
        self._bounds_lookup['offset_ne']['cv']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': ""},
             'outer': {'start': "2", 'stop': "-1"}}
        self._bounds_lookup['offset_ne']['cf']['all_pts'] = \
            {'inner': {'start': "1", 'stop': ""},
             'outer': {'start': "1", 'stop': ""}}
        self._bounds_lookup['offset_ne']['cf']['internal_pts'] = \
            {'inner': {'start': "1", 'stop': "-1"},
             'outer': {'start': "1", 'stop': "-1"}}
        # Loop bounds for a mesh with SE offset
        self._bounds_lookup['offset_sw']['ct']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['ct']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': ""},
             'outer': {'start': "2", 'stop': ""}}
        self._bounds_lookup['offset_sw']['cu']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['cu']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': "+1"},
             'outer': {'start': "2", 'stop': ""}}
        self._bounds_lookup['offset_sw']['cv']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['cv']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': ""},
             'outer': {'start': "2", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['cf']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['cf']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': "+1"},
             'outer': {'start': "2", 'stop': "+1"}}
        # For offset 'any'
        for gridpt_type in VALID_FIELD_GRID_TYPES:
            for itspace in VALID_ITERATES_OVER:
                self._bounds_lookup['offset_any'][gridpt_type][itspace] = \
                    {'inner': {'start': "1", 'stop': ""},
                     'outer': {'start': "1", 'stop': ""}}
        # For 'every' grid-point type
        for offset in SUPPORTED_OFFSETS:
            for itspace in VALID_ITERATES_OVER:
                self._bounds_lookup[offset]['every'][itspace] = \
                    {'inner': {'start': "1", 'stop': "+1"},
                     'outer': {'start': "1", 'stop': "+1"}}

    def _upper_bound(self):
        ''' Returns the upper bound of this loop as a string '''
        schedule = self.ancestor(GOSchedule)
        if schedule.const_loop_bounds:
            index_offset = ""
            # Look for a child kernel in order to get the index offset.
            # Since this is the __str__ method we have no guarantee
            # what state we expect our object to be in so we allow
            # for the case where we don't have any child kernels.
            go_kernels = self.walk(self.children, GOKern)
            if go_kernels:
                index_offset = go_kernels[0].index_offset

            if self._loop_type == "inner":
                stop = schedule.iloop_stop
            else:
                stop = schedule.jloop_stop

            if index_offset:
                stop += (self._bounds_lookup[index_offset][self.field_space]
                         [self._iteration_space][self._loop_type]["stop"])
            else:
                stop = "not yet set"
        else:
            if self.field_space == "every":
                # Bounds are independent of the grid-offset convention in use

                # We look-up the upper bounds by enquiring about the SIZE of
                # the array itself
                if self._loop_type == "inner":
                    stop = "SIZE("+self.field_name+"%data, 1)"
                elif self._loop_type == "outer":
                    stop = "SIZE("+self.field_name+"%data, 2)"

            else:
                # loop bounds are pulled from the field object which
                # is more straightforward for us but provides the
                # Fortran compiler with less information.
                stop = self.field_name

                if self._iteration_space.lower() == "internal_pts":
                    stop += "%internal"
                elif self._iteration_space.lower() == "all_pts":
                    stop += "%whole"
                else:
                    raise GenerationError("Unrecognised iteration space, {0}. "
                                          "Cannot generate loop bounds.".
                                          format(self._iteration_space))
                if self._loop_type == "inner":
                    stop += "%xstop"
                elif self._loop_type == "outer":
                    stop += "%ystop"
        return stop

    def _lower_bound(self):
        ''' Returns a string containing the expression for the lower
        bound of the loop '''
        schedule = self.ancestor(GOSchedule)
        if schedule.const_loop_bounds:
            index_offset = ""
            # Look for a child kernel in order to get the index offset.
            # Since this is the __str__ method we have no guarantee
            # what state we expect our object to be in so we allow
            # for the case where we don't have any child kernels.
            go_kernels = self.walk(self.children, GOKern)
            if go_kernels:
                index_offset = go_kernels[0].index_offset

            if index_offset:
                start = (self._bounds_lookup[index_offset][self.field_space]
                         [self._iteration_space][self._loop_type]["start"])
            else:
                start = "not yet set"
        else:
            if self.field_space == "every":
                # Bounds are independent of the grid-offset convention in use
                start = "1"
            else:
                # loop bounds are pulled from the field object which
                # is more straightforward for us but provides the
                # Fortran compiler with less information.
                start = self.field_name
                if self._iteration_space.lower() == "internal_pts":
                    start += "%internal"
                elif self._iteration_space.lower() == "all_pts":
                    start += "%whole"
                else:
                    raise GenerationError("Unrecognised iteration space, {0}. "
                                          "Cannot generate loop bounds.".
                                          format(self._iteration_space))
                if self._loop_type == "inner":
                    start += "%xstart"
                elif self._loop_type == "outer":
                    start += "%ystart"
        return start

    def __str__(self):
        ''' Returns a string describing this Loop object '''
        step = self._step
        if not step:
            step = "1"

        result = ("Loop[" + self._id + "]: " + self._variable_name +
                  "=" + self._id + " lower=" + self._lower_bound() +
                  "," + self._upper_bound() + "," + step + "\n")
        for entity in self._children:
            result += str(entity)+"\n"
        result += "EndLoop"
        return result

    def gen_code(self, parent):
        ''' Generate the Fortran source for this loop '''
        # Our schedule holds the names to use for the loop bounds.
        # Climb up the tree looking for our enclosing Schedule
        schedule = self.ancestor(GOSchedule)
        if schedule is None or not isinstance(schedule, GOSchedule):
            raise GenerationError("Internal error: cannot find parent"
                                  " GOSchedule for this Do loop")

        # Walk down the tree looking for a kernel so that we can
        # look-up what index-offset convention we are to use
        go_kernels = self.walk(self.children, GOKern)
        if len(go_kernels) == 0:
            raise GenerationError("Internal error: cannot find the "
                                  "GOcean Kernel enclosed by this loop")
        index_offset = go_kernels[0].index_offset
        if schedule.const_loop_bounds and \
           index_offset not in SUPPORTED_OFFSETS:
            raise GenerationError("Constant bounds generation"
                                  " not implemented for a grid offset "
                                  "of {0}. Supported offsets are {1}".
                                  format(index_offset,
                                         SUPPORTED_OFFSETS))
        # Check that all kernels enclosed by this loop expect the same
        # grid offset
        for kernel in go_kernels:
            if kernel.index_offset != index_offset:
                raise GenerationError("All Kernels must expect the same "
                                      "grid offset but kernel {0} has offset "
                                      "{1} which does not match {2}".
                                      format(kernel.name,
                                             kernel.index_offset,
                                             index_offset))
        # Generate the upper and lower loop bounds
        self._start = self._lower_bound()
        self._stop = self._upper_bound()
        Loop.gen_code(self, parent)


class GOBuiltInCallFactory(object):
    ''' A GOcean-specific built-in call factory. No built-ins
        are supported in GOcean at the moment. '''

    @staticmethod
    def create():
        ''' Placeholder to create a GOocean-specific built-in call.
        This will require us to create a doubly-nested loop and then create
        the body of the particular built-in operation. '''
        raise GenerationError(
            "Built-ins are not supported for the GOcean 1.0 API")


class GOKernCallFactory(object):
    ''' A GOcean-specific kernel-call factory. A standard kernel call in
    GOcean consists of a doubly-nested loop (over i and j) and a call to
    the user-supplied kernel routine. '''
    @staticmethod
    def create(call, parent=None):
        ''' Create a new instance of a call to a GO kernel. Includes the
        looping structure as well as the call to the kernel itself. '''
        outer_loop = GOLoop(parent=parent,
                            loop_type="outer")
        inner_loop = GOLoop(parent=outer_loop,
                            loop_type="inner")
        outer_loop.addchild(inner_loop)
        gocall = GOKern()
        gocall.load(call, parent=inner_loop)
        inner_loop.addchild(gocall)
        # determine inner and outer loops space information from the
        # child kernel call. This is only picked up automatically (by
        # the inner loop) if the kernel call is passed into the inner
        # loop.
        inner_loop.iteration_space = gocall.iterates_over
        outer_loop.iteration_space = inner_loop.iteration_space
        inner_loop.field_space = gocall.\
            arguments.iteration_space_arg().function_space
        outer_loop.field_space = inner_loop.field_space
        inner_loop.field_name = gocall.\
            arguments.iteration_space_arg().name
        outer_loop.field_name = inner_loop.field_name
        return outer_loop


class GOKern(Kern):
    ''' Stores information about GOcean Kernels as specified by the Kernel
        metadata. Uses this information to generate appropriate PSy layer
        code for the Kernel instance. Specialises the gen_code method to
        create the appropriate GOcean specific kernel call. '''
    def __init__(self):
        ''' Create an empty GOKern object. The object is given state via
        the load method '''
        if False:  # pylint: disable=using-constant-test
            self._arguments = GOKernelArguments(None, None)  # for pyreverse
        # Create those member variables required for testing and to keep
        # pylint happy
        self._children = []
        self._name = ""
        self._index_offset = ""

    def load(self, call, parent=None):
        ''' Populate the state of this GOKern object '''
        Kern.__init__(self, GOKernelArguments, call, parent, check=False)

        # Pull out the grid index-offset that this kernel expects and
        # store it here. This is used to check that all of the kernels
        # invoked by an application are using compatible index offsets.
        self._index_offset = call.ktype.index_offset

    def local_vars(self):
        '''Return a list of the variable (names) that are local to this loop
        (and must therefore be e.g. threadprivate if doing OpenMP)

        '''
        return []

    def _find_grid_access(self):
        '''Determine the best kernel argument from which to get properties of
            the grid. For this, an argument must be a field (i.e. not
            a scalar) and must be supplied by the algorithm layer
            (i.e. not a grid property). If possible it should also be
            a field that is read-only as otherwise compilers can get
            confused about data dependencies and refuse to SIMD
            vectorise.

        '''
        for access in ["read", "readwrite", "write"]:
            for arg in self._arguments.args:
                if arg.type == "field" and arg.access.lower() == access:
                    return arg
        # We failed to find any kernel argument which could be used
        # to access the grid properties. This will only be a problem
        # if the kernel requires a grid-property argument.
        return None

    def gen_code(self, parent):
        ''' Generates GOcean v1.0 specific psy code for a call to the dynamo
            kernel instance. '''
        from psyclone.f2pygen import CallGen, UseGen

        # Before we do anything else, go through the arguments and
        # determine the best one from which to obtain the grid properties.
        grid_arg = self._find_grid_access()

        # A GOcean 1.0 kernel always requires the [i,j] indices of the
        # grid-point that is to be updated
        arguments = ["i", "j"]
        for arg in self._arguments.args:

            if arg.type == "scalar":
                # Scalar arguments require no de-referencing
                arguments.append(arg.name)
            elif arg.type == "field":
                # Field objects are Fortran derived-types
                arguments.append(arg.name + "%data")
            elif arg.type == "grid_property":
                # Argument is a property of the grid which we can access via
                # the grid member of any field object.
                # We use the most suitable field as chosen above.
                if grid_arg is None:
                    raise GenerationError(
                        "Error: kernel {0} requires grid property {1} but "
                        "does not have any arguments that are fields".
                        format(self._name, arg.name))
                else:
                    arguments.append(grid_arg.name+"%grid%"+arg.name)
            else:
                raise GenerationError("Kernel {0}, argument {1} has "
                                      "unrecognised type: {2}".
                                      format(self._name, arg.name, arg.type))

        parent.add(CallGen(parent, self._name, arguments))
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name, only=True,
                              funcnames=[self._name]))

    @property
    def index_offset(self):
        ''' The grid index-offset convention that this kernel expects '''
        return self._index_offset


class GOKernelArguments(Arguments):
    '''Provides information about GOcean kernel-call arguments
        collectively, as specified by the kernel argument
        metadata. This class ensures that initialisation is performed
        correctly. It also overrides the iteration_space_arg method to
        supply a GOcean-specific dictionary for the mapping of
        argument-access types.

    '''
    def __init__(self, call, parent_call):
        if False:  # pylint: disable=using-constant-test
            self._0_to_n = GOKernelArgument(None, None, None)  # for pyreverse
        Arguments.__init__(self, parent_call)

        self._args = []
        # Loop over the kernel arguments obtained from the meta data
        for (idx, arg) in enumerate(call.ktype.arg_descriptors):
            # arg is a GO1p0Descriptor object
            if arg.type == "grid_property":
                # This is an argument supplied by the psy layer
                self._args.append(GOKernelGridArgument(arg))
            elif arg.type == "scalar" or arg.type == "field":
                # This is a kernel argument supplied by the Algorithm layer
                self._args.append(GOKernelArgument(arg, call.args[idx],
                                                   parent_call))
            else:
                raise ParseError("Invalid kernel argument type. Found '{0}' "
                                 "but must be one of {1}".
                                 format(arg.type, ["grid_property", "scalar",
                                                   "field"]))
        self._dofs = []

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this makes no
            sense for GOcean. Need to refactor the invoke class and pull out
            dofs into the gunghoproto api '''
        return self._dofs

    def iteration_space_arg(self, mapping=None):
        if mapping:
            my_mapping = mapping
        else:
            # We provide an empty mapping for inc as it is not supported
            # in the GOcean 1.0 API. However, the entry has to be there
            # in the dictionary as a field that has read access causes
            # the code (that checks that a kernel has at least one argument
            # that is written to) to attempt to lookup "inc".
            my_mapping = {"write": "write", "read": "read",
                          "readwrite": "readwrite", "inc": ""}
        arg = Arguments.iteration_space_arg(self, my_mapping)
        return arg


class GOKernelArgument(KernelArgument):
    ''' Provides information about individual GOcean kernel call arguments
        as specified by the kernel argument metadata. '''
    def __init__(self, arg, arg_info, call):

        self._arg = arg
        KernelArgument.__init__(self, arg, arg_info, call)

    @property
    def type(self):
        ''' Return the type of this kernel argument - whether it is a field,
            a scalar or a grid_property (to be supplied by the PSy layer) '''
        return self._arg.type

    @property
    def function_space(self):
        ''' Returns the expected finite difference space for this
            argument as specified by the kernel argument metadata.'''
        return self._arg.function_space


class GOKernelGridArgument(Argument):
    ''' Describes arguments that supply grid properties to a kernel.
        These arguments are provided by the PSy layer rather than in
        the Algorithm layer. '''

    def __init__(self, arg):

        if arg.grid_prop in GRID_PROPERTY_DICT:
            self._name = GRID_PROPERTY_DICT[arg.grid_prop]
        else:
            raise GenerationError("Unrecognised grid property specified. "
                                  "Expected one of {0} but found '{1}'".
                                  format(str(GRID_PROPERTY_DICT.keys()),
                                         arg.grid_prop))

        # This object always represents an argument that is a grid_property
        self._type = "grid_property"

    @property
    def name(self):
        ''' Returns the Fortran name of the grid property. This name is
            used in the generated code like so: <fld>%grid%name '''
        return self._name

    @property
    def type(self):
        ''' The type of this argument. We have this for compatibility with
            GOKernelArgument objects since, for this class, it will always be
            "grid_property". '''
        return self._type

    @property
    def text(self):
        ''' The raw text used to pass data from the algorithm layer
            for this argument. Grid properties are not passed from the
            algorithm layer so None is returned.'''
        return None

    def forward_dependence(self):
        '''
        A grid-property argument is read-only and supplied by the
        PSy layer so has no dependencies

        :returns: None to indicate no dependencies
        :rtype: NoneType
        '''
        return None

    def backward_dependence(self):
        '''
        A grid-property argument is read-only and supplied by the
        PSy layer so has no dependencies

        :returns: None to indicate no dependencies
        :rtype: NoneType
        '''
        return None


class GOStencil(object):
    '''GOcean 1.0 stencil information for a kernel argument as obtained by
    parsing the kernel meta-data. The expected structure of the
    metadata and its meaning is provided in the description of the
    load method

    '''
    def __init__(self):
        ''' Set up any internal variables. '''
        self._has_stencil = None
        self._stencil = [[0 for _ in range(3)] for _ in range(3)]
        self._name = None
        self._initialised = False

    def load(self, stencil_info, kernel_name):
        '''Take parsed stencil metadata information, check it is valid and
        store it in a convenient form. The kernel_name argument is
        only used to provide the location if there is an error.

        The stencil information should either be a name which
        indicates a particular type of stencil or in the form
        stencil(xxx,yyy,zzz) which explicitly specifies a stencil
        shape where xxx, yyy and zzz are triplets of integers
        indicating whether there is a stencil access in a particular
        direction and the depth of that access. For example:

        stencil(010,  !   N
                212,  !  W E
                010)  !   S

        indicates that there is a stencil access of depth 1 in the
        "North" and "South" directions and stencil access of depth 2
        in the "East" and "West" directions. The value at the centre
        of the stencil will not be used by PSyclone but can be 0 or 1
        and indicates whether the local field value is accessed.

        The convention is for the associated arrays to be
        2-dimensional. If we denote the first dimension as "i" and the
        second dimension as "j" then the following directions are
        assumed:

        j
        ^
        |
        |
        ---->i

        For example a stencil access like:

        a(i,j) + a(i+1,j) + a(i,j-1)

        would be stored as:

        stencil(000,
                011,
                010)

        :param stencil_info: contains the appropriate part of the parser AST
        :type stencil_info: :py:class:`psyclone.expression.FunctionVar`
        :param string kernel_name: the name of the kernel from where
        this stencil information came from.
        :raises ParseError: if the supplied stencil information is invalid

        '''
        self._initialised = True

        if not isinstance(stencil_info, expr.FunctionVar):
            # the stencil information is not in the expected format
            raise ParseError(
                "Meta-data error in kernel '{0}': 3rd descriptor (stencil) of "
                "field argument is '{1}' but expected either a name or the "
                "format 'stencil(...)'".format(kernel_name, str(stencil_info)))

        # Get the name
        name = stencil_info.name.lower()

        if stencil_info.args:
            # The stencil info is of the form 'name(a,b,...), so the
            # name should be 'stencil' and there should be 3
            # arguments'
            self._has_stencil = True
            args = stencil_info.args
            if name != "stencil":
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument is '{1}' but must be "
                    "'stencil(...)".format(kernel_name, name))
            if len(args) != 3:
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument with format "
                    "'stencil(...)', has {1} arguments but should have "
                    "3".format(kernel_name, len(args)))
            # Each of the 3 args should be of length 3 and each
            # character should be a digit from 0-9. Whilst we are
            # expecting numbers, the parser represents these numbers
            # as strings so we have to perform string manipulation to
            # check and that extract them
            for arg_idx in range(3):
                arg = args[arg_idx]
                if not isinstance(arg, str):
                    raise ParseError(
                        "Meta-data error in kernel '{0}': 3rd descriptor "
                        "(stencil) of field argument with format "
                        "'stencil(...)'. Argument index {1} should be a "
                        "number but found "
                        "'{2}'.".format(kernel_name, arg_idx, str(arg)))
                if len(arg) != 3:
                    raise ParseError(
                        "Meta-data error in kernel '{0}': 3rd descriptor "
                        "(stencil) of field argument with format "
                        "'stencil(...)'. Argument index {1} should "
                        "consist of 3 digits but found "
                        "{2}.".format(kernel_name, arg_idx, len(arg)))
            # The central value is constrained to be 0 or 1
            if args[1][1] not in ["0", "1"]:
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument with format "
                    "'stencil(...)'. Argument index 1 position 1 "
                    "should be a number from 0-1 "
                    "but found {1}.".format(kernel_name, args[1][1]))
            # It is not valid to specify a zero stencil. This is
            # indicated by the 'pointwise' name
            if args[0] == "000" and \
               (args[1] == "000" or args[1] == "010") and \
               args[2] == "000":
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument with format "
                    "'stencil(...)'. A zero sized stencil has been "
                    "specified. This should be specified with the "
                    "'pointwise' keyword.".format(kernel_name))
            # store the values in an internal array as integers in i,j
            # order
            for idx0 in range(3):
                for idx1 in range(3):
                    self._stencil[idx0][idx1] = int(args[idx1][idx0])
        else:
            # stencil info is of the form 'name' so should be one of
            # our valid names
            if name not in VALID_STENCIL_NAMES:
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument is '{1}' but must be one "
                    "of {2} or stencil(...)".format(kernel_name, name,
                                                    VALID_STENCIL_NAMES))
            self._name = name
            # We currently only support one valid name ('pointwise')
            # which indicates that there is no stencil
            self._has_stencil = False

    def _check_init(self):
        '''Internal method which checks that the stencil information has been
        loaded.

        :raises GenerationError: if the GOStencil object has not been
        initialised i.e. the load() method has not been called

        '''
        if not self._initialised:
            raise GenerationError(
                "Error in class GOStencil: the object has not yet been "
                "initialised. Please ensure the load() method is called.")

    @property
    def has_stencil(self):
        '''Specifies whether this argument has stencil information or not. The
        only case when this is False is when the stencil information
        specifies 'pointwise' as this indicates that there is no
        stencil access.

        :return Bool: True if this argument has stencil information
        and False if not.

        '''
        self._check_init()
        return self._has_stencil

    @property
    def name(self):
        '''Provides the stencil name if one is provided

        :return string: the name of the type of stencil if this is
        provided and 'None' if not.

        '''
        self._check_init()
        return self._name

    def depth(self, index0, index1):
        '''Provides the depth of the stencil in the 8 possible stencil
        directions in a 2d regular grid (see the description in the
        load class for more information). Values must be between -1
        and 1 as they are considered to be relative to the centre of
        the stencil For example:

        stencil(234,
                915,
                876)

        returns

        depth(-1,0) = 9
        depth(1,1) = 4

        :param int index0: the relative stencil offset for the first
        index of the associated array. This value must be between -1
        and 1
        :param int index1: the relative stencil offset for the second
        index of the associated array. This value must be between -1
        and 1
        :return int: the depth of the stencil in the specified direction
        :raises GenerationError: if the indices are out-of-bounds

        '''
        self._check_init()
        if index0 < -1 or index0 > 1 or index1 < -1 or index1 > 1:
            raise GenerationError(
                "The indices arguments to the depth method in the GOStencil "
                "object must be between -1 and 1 but found "
                "({0},{1})".format(index0, index1))
        return self._stencil[index0+1][index1+1]


class GO1p0Descriptor(Descriptor):
    '''Description of a GOcean 1.0 kernel argument, as obtained by
        parsing the kernel meta-data

    '''

    def __init__(self, kernel_name, kernel_arg):
        '''Test and extract the required kernel metadata

        :param str kernel_name: the name of the kernel metadata type
        that contains this metadata
        :param kernel_arg: the relevant part of the parser's AST
        :type kernel_arg: :py:class:`psyclone.expression.FunctionVar`

        '''

        nargs = len(kernel_arg.args)
        stencil_info = None

        if nargs == 3:
            # This kernel argument is supplied by the Algorithm layer
            # and is either a field or a scalar

            access = kernel_arg.args[0].name
            funcspace = kernel_arg.args[1].name
            stencil_info = GOStencil()
            stencil_info.load(kernel_arg.args[2],
                              kernel_name)

            # Valid values for the grid-point type that a kernel argument
            # may have. (We use the funcspace argument for this as it is
            # similar to the space in Finite-Element world.)
            valid_func_spaces = VALID_FIELD_GRID_TYPES + VALID_SCALAR_TYPES

            self._grid_prop = ""
            if funcspace.lower() in VALID_FIELD_GRID_TYPES:
                self._type = "field"
            elif funcspace.lower() in VALID_SCALAR_TYPES:
                self._type = "scalar"
            else:
                raise ParseError("Meta-data error in kernel {0}: argument "
                                 "grid-point type is '{1}' but must be one "
                                 "of {2} ".format(kernel_name, funcspace,
                                                  valid_func_spaces))

        elif nargs == 2:
            # This kernel argument is a property of the grid
            access = kernel_arg.args[0].name
            grid_var = kernel_arg.args[1].name
            funcspace = ""
            stencil = ""

            self._grid_prop = grid_var
            self._type = "grid_property"

            if grid_var.lower() not in GRID_PROPERTY_DICT:
                raise ParseError(
                    "Meta-data error in kernel {0}: un-recognised grid "
                    "property '{1}' requested. Must be one of {2}".
                    format(kernel_name,
                           grid_var,
                           str(GRID_PROPERTY_DICT.keys())))
        else:
            raise ParseError(
                "Meta-data error in kernel {0}: 'arg' type expects 2 or 3 "
                "arguments but found '{1}' in '{2}'".
                format(kernel_name,
                       str(len(kernel_arg.args)),
                       kernel_arg.args))

        if access.lower() not in VALID_ARG_ACCESSES:
            raise ParseError("Meta-data error in kernel {0}: argument "
                             "access  is given as '{1}' but must be "
                             "one of {2}".
                             format(kernel_name, access, VALID_ARG_ACCESSES))

        # Finally we can call the __init__ method of our base class
        Descriptor.__init__(self, access, funcspace, stencil_info)

    def __str__(self):
        return repr(self)

    @property
    def grid_prop(self):
        ''' The name of the grid-property that this argument is to supply
            to the kernel '''
        return self._grid_prop

    @property
    def type(self):
        ''' The type of this argument - whether it is a scalar, a field or
            a grid-property. The latter are special because they must be
            supplied by the PSy layer. '''
        return self._type


class GOKernelType1p0(KernelType):
    ''' Description of a kernel including the grid index-offset it
        expects and the region of the grid that it expects to
        operate upon '''

    def __str__(self):
        return ('GOcean 1.0 kernel ' + self._name + ', index-offset = ' +
                self._index_offset + ', iterates-over = ' +
                self._iterates_over)

    def __init__(self, ast, name=None):
        # Initialise the base class
        KernelType.__init__(self, ast, name=name)

        # What grid offset scheme this kernel expects
        self._index_offset = self._ktype.get_variable('index_offset').init

        if self._index_offset is None:
            raise ParseError("Meta-data error in kernel {0}: an INDEX_OFFSET "
                             "must be specified and must be one of {1}".
                             format(name, VALID_OFFSET_NAMES))

        if self._index_offset.lower() not in VALID_OFFSET_NAMES:
            raise ParseError("Meta-data error in kernel {0}: INDEX_OFFSET "
                             "has value '{1}' but must be one of {2}".
                             format(name,
                                    self._index_offset,
                                    VALID_OFFSET_NAMES))

        # Check that the meta-data for this kernel is valid
        if self._iterates_over is None:
            raise ParseError("Meta-data error in kernel {0}: ITERATES_OVER "
                             "is missing. (Valid values are: {1})".
                             format(name, VALID_ITERATES_OVER))

        if self._iterates_over.lower() not in VALID_ITERATES_OVER:
            raise ParseError("Meta-data error in kernel {0}: ITERATES_OVER "
                             "has value '{1}' but must be one of {2}".
                             format(name,
                                    self._iterates_over.lower(),
                                    VALID_ITERATES_OVER))

        # The list of kernel arguments
        self._arg_descriptors = []
        have_grid_prop = False
        for init in self._inits:
            if init.name != 'arg':
                raise ParseError("Each meta_arg value must be of type " +
                                 "'arg' for the gocean1.0 api, but " +
                                 "found '{0}'".format(init.name))
            # Pass in the name of this kernel for the purposes
            # of error reporting
            new_arg = GO1p0Descriptor(name, init)
            # Keep track of whether this kernel requires any
            # grid properties
            have_grid_prop = (have_grid_prop or
                              (new_arg.type == "grid_property"))
            self._arg_descriptors.append(new_arg)

        # If this kernel expects a grid property then check that it
        # has at least one field object as an argument (which we
        # can use to access the grid)
        if have_grid_prop:
            have_fld = False
            for arg in self.arg_descriptors:
                if arg.type == "field":
                    have_fld = True
                    break
            if not have_fld:
                raise ParseError(
                    "Kernel {0} requires a property of the grid but does "
                    "not have any field objects as arguments.".format(name))

    # Override nargs from the base class so that it returns the no.
    # of args specified in the algorithm layer (and thus excludes those
    # that must be added in the PSy layer). This is done to simplify the
    # check on the no. of arguments supplied in any invoke of the kernel.
    @property
    def nargs(self):
        ''' Count and return the number of arguments that this kernel
            expects the Algorithm layer to provide '''
        count = 0
        for arg in self.arg_descriptors:
            if arg.type != "grid_property":
                count += 1
        return count

    @property
    def index_offset(self):
        ''' Return the grid index-offset that this kernel expects '''
        return self._index_offset
