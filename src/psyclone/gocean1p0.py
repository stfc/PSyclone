# pylint: disable=too-many-lines
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# Modified work Copyright (c) 2018 by J. Henrichs, Bureau of Meteorology


'''This module implements the PSyclone GOcean 1.0 API by specialising
    the required base classes for both code generation (PSy, Invokes,
    Invoke, InvokeSchedule, Loop, Kern, Arguments and KernelArgument)
    and parsing (Descriptor and KernelType). It adds a
    GOKernelGridArgument class to capture information on kernel arguments
    that supply properties of the grid (and are generated in the PSy
    layer).

'''

from __future__ import print_function
from psyclone.configuration import Config
from psyclone.parse.kernel import Descriptor, KernelType
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, \
    Loop, CodedKern, Arguments, Argument, KernelArgument, \
    GenerationError, InternalError, args_filter, NameSpaceFactory, \
    KernelSchedule, SymbolTable, Node, Fparser2ASTProcessor, AccessType, \
    Literal, Reference, ACCEnterDataDirective
import psyclone.expression as expr

# The different grid-point types that a field can live on
VALID_FIELD_GRID_TYPES = ["go_cu", "go_cv", "go_ct", "go_cf", "go_every"]

# The two scalar types we support
VALID_SCALAR_TYPES = ["go_i_scalar", "go_r_scalar"]

# Index-offset schemes (for the Arakawa C-grid)
VALID_OFFSET_NAMES = ["go_offset_se", "go_offset_sw",
                      "go_offset_ne", "go_offset_nw", "go_offset_any"]

# The offset schemes for which we can currently generate constant
# loop bounds in the PSy layer
SUPPORTED_OFFSETS = ["go_offset_ne", "go_offset_sw", "go_offset_any"]

# The sets of grid points that a kernel may operate on
VALID_ITERATES_OVER = ["go_all_pts", "go_internal_pts", "go_external_pts"]

# The list of valid stencil properties. We currently only support
# pointwise. This property could probably be removed from the
# GOcean API altogether.
VALID_STENCIL_NAMES = ["go_pointwise"]

# A dictionary giving the mapping from meta-data names for
# properties of the grid to their names in the Fortran grid_type.
GRID_PROPERTY_DICT = {"go_grid_area_t": "area_t",
                      "go_grid_area_u": "area_u",
                      "go_grid_area_v": "area_v",
                      "go_grid_mask_t": "tmask",
                      "go_grid_dx_t": "dx_t",
                      "go_grid_dx_u": "dx_u",
                      "go_grid_dx_v": "dx_v",
                      "go_grid_dy_t": "dy_t",
                      "go_grid_dy_u": "dy_u",
                      "go_grid_dy_v": "dy_v",
                      "go_grid_lat_u": "gphiu",
                      "go_grid_lat_v": "gphiv",
                      "go_grid_dx_const": "dx",
                      "go_grid_dy_const": "dy",
                      "go_grid_x_min_index": "subdomain%internal%xstart",
                      "go_grid_x_max_index": "subdomain%internal%xstop",
                      "go_grid_y_min_index": "subdomain%internal%ystart",
                      "go_grid_y_max_index": "subdomain%internal%ystop"}

# The valid types of loop. In this API we expect only doubly-nested
# loops.
VALID_LOOP_TYPES = ["inner", "outer"]


class GOPSy(PSy):
    '''
    The GOcean 1.0 specific PSy class. This creates a GOcean specific
    invokes object (which controls all the required invocation calls).
    Also overrides the PSy gen method so that we generate GOcean-
    specific PSy module code.

    :param invoke_info: An object containing the required invocation \
                        information for code optimisation and generation.
    :type invoke_info: :py:class:`psyclone.parse.FileInfo`
    '''
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
        self.invokes.gen_code(psy_module)
        # inline kernels where requested
        self.inline(psy_module)
        return psy_module.root


class GOInvokes(Invokes):
    '''
    The GOcean specific invokes class. This passes the GOcean specific
    invoke class to the base class so it creates the one we require.
    :param alg_calls: The Invoke calls discovered in the Algorithm layer.
    :type alg_calls: OrderedDict of :py:class:`psyclone.parse.InvokeCall` \
                     objects.
    '''
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
            for kern_call in invoke.schedule.coded_kernels():
                # We only care if the index offset is not offset_any (since
                # that is compatible with any other offset)
                if kern_call.index_offset != "go_offset_any":
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
    '''
    The GOcean specific invoke class. This passes the GOcean specific
    schedule class to the base class so it creates the one we require.
    A set of GOcean infrastructure reserved names are also passed to
    ensure that there are no name clashes. Also overrides the gen_code
    method so that we generate GOcean specific invocation code and
    provides three methods which separate arguments that are arrays from
    arguments that are {integer, real} scalars.

    :param alg_invocation: Node in the AST describing the invoke call.
    :type alg_invocation: :py:class:`psyclone.parse.InvokeCall`
    :param int idx: The position of the invoke in the list of invokes \
                    contained in the Algorithm.

    '''
    def __init__(self, alg_invocation, idx):
        if False:  # pylint: disable=using-constant-test
            self._schedule = GOInvokeSchedule(None)  # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, GOInvokeSchedule)

    @property
    def unique_args_arrays(self):
        ''' find unique arguments that are arrays (defined as those that are
            field objects as opposed to scalars or properties of the grid). '''
        result = []
        for call in self._schedule.kernels():
            for arg in call.arguments.args:
                if arg.type == 'field' and arg.name not in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_rscalars(self):
        '''
        :returns: the unique arguments that are scalars of type real \
                  (defined as those that are go_r_scalar 'space').
        :rtype: list of str.

        '''
        result = []
        for call in self._schedule.kernels():
            for arg in args_filter(call.arguments.args, arg_types=["scalar"],
                                   is_literal=False):
                if arg.space.lower() == "go_r_scalar" and \
                   arg.name not in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_iscalars(self):
        '''
        :returns: the unique arguments that are scalars of type integer \
                  (defined as those that are i_scalar 'space').
        :rtype: list of str.

        '''
        result = []
        for call in self._schedule.kernels():
            for arg in args_filter(call.arguments.args, arg_types=["scalar"],
                                   is_literal=False):
                if arg.space.lower() == "go_i_scalar" and \
                   arg.name not in result:
                    result.append(arg.name)
        return result

    def gen_code(self, parent):
        '''
        Generates GOcean specific invocation code (the subroutine called
        by the associated invoke call in the algorithm layer). This
        consists of the PSy invocation subroutine and the declaration of
        its arguments.

        :param parent: the node in the generated AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`
        '''
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

        # If we're generating an OpenCL routine then the arguments must
        # have the target attribute as we pass pointers to them in to
        # the OpenCL run-time.
        target = bool(self.schedule.opencl)

        # add the subroutine argument declarations for fields
        if self.unique_args_arrays:
            my_decl_arrays = TypeDeclGen(invoke_sub, datatype="r2d_field",
                                         intent="inout", target=target,
                                         entity_decls=self.unique_args_arrays)
            invoke_sub.add(my_decl_arrays)

        # add the subroutine argument declarations for real scalars
        if self.unique_args_rscalars:
            my_decl_rscalars = DeclGen(invoke_sub, datatype="REAL",
                                       intent="inout", kind="go_wp",
                                       entity_decls=self.unique_args_rscalars)
            invoke_sub.add(my_decl_rscalars)
        # add the subroutine argument declarations for integer scalars
        if self.unique_args_iscalars:
            my_decl_iscalars = DeclGen(invoke_sub, datatype="INTEGER",
                                       intent="inout",
                                       entity_decls=self.unique_args_iscalars)
            invoke_sub.add(my_decl_iscalars)

        if self._schedule.const_loop_bounds and self.unique_args_arrays:

            # Look-up the loop bounds using the first field object in the
            # list
            sim_domain = self.unique_args_arrays[0] +\
                "%grid%subdomain%internal%"
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


class GOInvokeSchedule(InvokeSchedule):
    ''' The GOcean specific InvokeSchedule sub-class. We call the base class
    constructor and pass it factories to create GO-specific calls to both
    user-supplied kernels and built-ins. '''

    def __init__(self, alg_calls):
        InvokeSchedule.__init__(self, GOKernCallFactory, GOBuiltInCallFactory,
                                alg_calls)

        # Configuration of this InvokeSchedule - we default to having
        # constant loop bounds. If we end up having a long list
        # of configuration member variables here we may want
        # to create a a new ScheduleConfig object to manage them.
        self._const_loop_bounds = True

    def view(self, indent=0):
        '''Print a representation of this GOInvokeSchedule.
        :param int indent: optional argument indicating the level of
        indentation to add before outputting the class information.'''
        print(self.indent(indent) + self.coloured_text + "[invoke='" +
              self.invoke.name + "',Constant loop bounds=" +
              str(self._const_loop_bounds) + "]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this GOInvokeSchedule '''
        result = "GOInvokeSchedule(Constant loop bounds=" + \
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
        return colored("GOInvokeSchedule", SCHEDULE_COLOUR_MAP["Schedule"])

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
        ''' Set whether the InvokeSchedule will use constant loop bounds or
        will look them up from the field object for every loop '''
        self._const_loop_bounds = obj


# pylint: disable=too-many-instance-attributes
class GOLoop(Loop):
    ''' The GOcean specific Loop class. This passes the GOcean specific
        single loop information to the base class so it creates the one we
        require. Adds a GOcean specific setBounds method which tells the loop
        what to iterate over. Need to harmonise with the topology_name method
        in the Dynamo api. '''

    _bounds_lookup = {}

    def __init__(self, parent=None,
                 topology_name="", loop_type=""):
        '''Constructs a GOLoop instance.
        :param parent: Optional parent node (default None).
        :type parent: :py:class:`psyclone.psyGen.node`
        :param str topology_name: Optional opology of the loop (unused atm).
        :param str loop_type: Loop type - must be 'inner' or 'outer'.'''

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

        if not GOLoop._bounds_lookup:
            GOLoop.setup_bounds()

    # -------------------------------------------------------------------------
    @staticmethod
    def setup_bounds():
        '''Populates the GOLoop._bounds_lookup dictionary. This is
        used by PSyclone to look up the loop boundaries for each loop
        it creates.'''

        for grid_offset in SUPPORTED_OFFSETS:
            GOLoop._bounds_lookup[grid_offset] = {}
            for gridpt_type in VALID_FIELD_GRID_TYPES:
                GOLoop._bounds_lookup[grid_offset][gridpt_type] = {}
                for itspace in VALID_ITERATES_OVER:
                    GOLoop._bounds_lookup[grid_offset][gridpt_type][
                        itspace] = {}

        # Loop bounds for a mesh with NE offset
        GOLoop._bounds_lookup['go_offset_ne']['go_ct']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_ct']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}"},
             'outer': {'start': "{start}", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cu']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cu']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}-1"},
             'outer': {'start': "{start}", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cv']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cv']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}"},
             'outer': {'start': "{start}", 'stop': "{stop}-1"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cf']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}"},
             'outer': {'start': "{start}-1", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cf']['go_internal_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}-1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}-1"}}
        # Loop bounds for a mesh with SE offset
        GOLoop._bounds_lookup['go_offset_sw']['go_ct']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_ct']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}"},
             'outer': {'start': "{start}", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cu']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cu']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}+1"},
             'outer': {'start': "{start}", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cv']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cv']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}"},
             'outer': {'start': "{start}", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cf']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cf']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}+1"},
             'outer': {'start': "{start}", 'stop': "{stop}+1"}}
        # For offset 'any'
        for gridpt_type in VALID_FIELD_GRID_TYPES:
            for itspace in VALID_ITERATES_OVER:
                GOLoop._bounds_lookup['go_offset_any'][gridpt_type][itspace] =\
                    {'inner': {'start': "{start}-1", 'stop': "{stop}"},
                     'outer': {'start': "{start}-1", 'stop': "{stop}"}}
        # For 'every' grid-point type
        for offset in SUPPORTED_OFFSETS:
            for itspace in VALID_ITERATES_OVER:
                GOLoop._bounds_lookup[offset]['go_every'][itspace] = \
                    {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
                     'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}

    # -------------------------------------------------------------------------
    @staticmethod
    def add_bounds(bound_info):
        '''
        Adds a new iteration space to PSyclone. An iteration space in the
        gocean1.0 API is for a certain offset type and field type. It defines
        the loop boundaries for the outer and inner loop. The format is a
        ":" separated tuple:
        bound_info = offset-type:field-type:iteration-space:outer-start:
                      outer-stop:inner-start:inner-stop
        Example:
        bound_info = go_offset_ne:go_ct:go_all_pts\
                     :{start}-1:{stop}+1:{start}:{stop}

        The expressions {start} and {stop} will be replaced with the loop
        indices that correspond to the inner points (i.e. non-halo or
        boundary points) of the field. So the index {start}-1 is actually
        on the halo / boundary.

        :param str bound_info: A string that contains a ":" separated \
               tuple with the iteration space definition.
        :raises ValueError: if bound_info is not a string.
        :raises ConfigurationError: if bound_info is not formatted correctly.
        '''

        if not isinstance(bound_info, str):
            raise InternalError("The parameter 'bound_info' must be a string, "
                                "got '{0}' (type {1})"
                                .format(bound_info, type(bound_info)))

        data = bound_info.split(":")
        if len(data) != 7:
            from psyclone.configuration import ConfigurationError
            raise ConfigurationError("An iteration space must be in the form "
                                     "\"offset-type:field-type:"
                                     "iteration-space:outer-start:"
                                     "outer-stop:inner-start:inner-stop\"\n"
                                     "But got \"{0}\"".format(bound_info))

        if not GOLoop._bounds_lookup:
            GOLoop.setup_bounds()

        # Check that all bound specifications (min and max index) are valid.
        # ------------------------------------------------------------------
        import re
        # Regular expression that finds stings surrounded by {}
        bracket_regex = re.compile("{[^}]+}")
        for bound in data[3:7]:
            all_expr = bracket_regex.findall(bound)
            for bracket_expr in all_expr:
                if bracket_expr not in ["{start}", "{stop}"]:
                    from psyclone.configuration import ConfigurationError
                    raise ConfigurationError("Only '{{start}}' and '{{stop}}' "
                                             "are allowed as bracketed "
                                             "expression in an iteration "
                                             "space. But got "
                                             "{0}".format(bracket_expr))

        # Test if a loop with the given boundaries can actually be parsed.
        from fparser.two.Fortran2003 import NoMatchError, Nonlabel_Do_Stmt
        from fparser.two.parser import ParserFactory
        # Necessary to setup the parser
        ParserFactory().create(std="f2003")

        # Test both the outer loop indices (index 3 and 4) and inner
        # indices (index 5 and 6):
        for bound in data[3:7]:
            do_string = "do i=1, {0}".format(bound)
            # Now replace any {start}/{stop} expression in the loop
            # with a valid integer value:
            do_string = do_string.format(start='15', stop='25')
            # Check if the do loop can be parsed as a nonlabel do loop
            try:
                _ = Nonlabel_Do_Stmt(do_string)
            except NoMatchError as err:
                from psyclone.configuration import ConfigurationError
                raise ConfigurationError("Expression '{0}' is not a "
                                         "valid do loop boundary. Error "
                                         "message: '{1}'."
                                         .format(bound, str(err)))

        # All tests successful, so add the new bounds:
        # --------------------------------------------
        current_bounds = GOLoop._bounds_lookup   # Shortcut
        # Check offset-type exists
        if not data[0] in current_bounds:
            current_bounds[data[0]] = {}

        # Check field-type exists
        if not data[1] in current_bounds[data[0]]:
            current_bounds[data[0]][data[1]] = {}

        # Check iteration space exists:
        if not data[2] in current_bounds[data[0]][data[1]]:
            current_bounds[data[0]][data[1]][data[2]] = {}
            VALID_ITERATES_OVER.append(data[2])

        current_bounds[data[0]][data[1]][data[2]] = \
            {'outer': {'start': data[3], 'stop': data[4]},
             'inner': {'start': data[5], 'stop': data[6]}}

    # -------------------------------------------------------------------------
    # pylint: disable=too-many-branches
    def _upper_bound(self):
        ''' Returns the upper bound of this loop as a string.
        This takes the field type and usage of const_loop_bounds
        into account. In case of const_loop_bounds it will be
        using the data in GOLoop._bounds_lookup to find the appropriate
        indices depending on offset, field type, and iteration space.
        All occurences of {start} and {stop} in _bounds_loopup will
        be replaced with the constant loop boundary variable, e.g.
        "{stop}+1" will become "istop+1" (or "jstop+1 depending on
        loop type).'''
        schedule = self.ancestor(GOInvokeSchedule)
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
                # This strange line splitting was the only way I could find
                # to avoid pep8 warnings: using [..._space]\ keeps on
                # complaining about a white space
                bounds = GOLoop._bounds_lookup[index_offset][self.field_space][
                    self._iteration_space][self._loop_type]
                stop = bounds["stop"].format(start='2', stop=stop)
                # Remove all white spaces
                stop = "".join(stop.split())
                # This common cases is a bit of compile-time computation
                # but it helps fixing all test cases.
                if stop == "2-1":
                    stop = "1"
            else:
                stop = "not yet set"
        else:
            if self.field_space == "go_every":
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

                if self._iteration_space.lower() == "go_internal_pts":
                    stop += "%internal"
                elif self._iteration_space.lower() == "go_all_pts":
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

    # -------------------------------------------------------------------------
    # pylint: disable=too-many-branches
    def _lower_bound(self):
        ''' Returns the lower bound of this loop as a string.
        This takes the field type and usage of const_loop_bounds
        into account. In case of const_loop_bounds it will be
        using the data in GOLoop._bounds_lookup to find the appropriate
        indices depending on offset, field type, and iteration space.
        All occurences of {start} and {stop} in _bounds_loopup will
        be replaced with the constant loop boundary variable, e.g.
        "{stop}+1" will become "istop+1" (or "jstop+1" depending on
        loop type).'''

        schedule = self.ancestor(GOInvokeSchedule)
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
                # This strange line splitting was the only way I could find
                # to avoid pep8 warnings: using [..._space]\ keeps on
                # complaining about a white space
                bounds = GOLoop._bounds_lookup[index_offset][self.field_space][
                    self._iteration_space][self._loop_type]
                start = bounds["start"].format(start='2', stop=stop)
                # Remove all white spaces
                start = "".join(start.split())
                # This common cases is a bit of compile-time computation
                # but it helps fixing all test cases.
                if start == "2-1":
                    start = "1"
            else:
                start = "not yet set"
        else:
            if self.field_space == "go_every":
                # Bounds are independent of the grid-offset convention in use
                start = "1"
            else:
                # loop bounds are pulled from the field object which
                # is more straightforward for us but provides the
                # Fortran compiler with less information.
                start = self.field_name
                if self._iteration_space.lower() == "go_internal_pts":
                    start += "%internal"
                elif self._iteration_space.lower() == "go_all_pts":
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

    # -------------------------------------------------------------------------
    def __str__(self):
        ''' Returns a string describing this Loop object '''
        try:
            step = str(self.step_expr)
        except InternalError:
            step = "1"

        result = ("Loop[" + self._id + "]: " + self._variable_name +
                  "=" + self._id + " lower=" + self._lower_bound() +
                  "," + self._upper_bound() + "," + step + "\n")
        for entity in self._children:
            result += str(entity)+"\n"
        result += "EndLoop"
        return result

    # -------------------------------------------------------------------------
    def gen_code(self, parent):
        ''' Generate the Fortran source for this loop '''
        # Our schedule holds the names to use for the loop bounds.
        # Climb up the tree looking for our enclosing GOInvokeSchedule
        schedule = self.ancestor(GOInvokeSchedule)
        if schedule is None or not isinstance(schedule, GOInvokeSchedule):
            raise GenerationError("Internal error: cannot find parent"
                                  " GOInvokeSchedule for this Do loop")

        # Walk down the tree looking for a kernel so that we can
        # look-up what index-offset convention we are to use
        go_kernels = self.walk(self.children, GOKern)
        if not go_kernels:
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
        self.start_expr = Literal(self._lower_bound(), parent=self)
        self.stop_expr = Literal(self._upper_bound(), parent=self)

        Loop.gen_code(self, parent)


# pylint: disable=too-few-public-methods
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


# pylint: disable=too-few-public-methods
class GOKernCallFactory(object):
    ''' A GOcean-specific kernel-call factory. A standard kernel call in
    GOcean consists of a doubly-nested loop (over i and j) and a call to
    the user-supplied kernel routine. '''
    @staticmethod
    def create(call, parent=None):
        ''' Create a new instance of a call to a GO kernel. Includes the
        looping structure as well as the call to the kernel itself. '''
        outer_loop = GOLoop(parent=parent, loop_type="outer")
        inner_loop = GOLoop(parent=outer_loop.children[3], loop_type="inner")
        outer_loop.loop_body.addchild(inner_loop)
        gocall = GOKern()
        gocall.load(call, parent=inner_loop.children[3])
        inner_loop.loop_body.addchild(gocall)
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


class GOKern(CodedKern):
    '''
    Stores information about GOcean Kernels as specified by the Kernel
    metadata. Uses this information to generate appropriate PSy layer
    code for the Kernel instance. Specialises the gen_code method to
    create the appropriate GOcean specific kernel call.

    '''
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
        # Get a reference to the namespace manager
        self._name_space_manager = NameSpaceFactory().create()

    def load(self, call, parent=None):
        '''
        Populate the state of this GOKern object.

        :param call: information on the way in which this kernel is called \
                     from the Algorithm layer.
        :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
        :param parent: the parent of this Kernel node in the PSyIR.
        :type parent: :py:class:`psyclone.gocean1p0.GOLoop`

        '''
        super(GOKern, self).__init__(GOKernelArguments, call, parent,
                                     check=False)

        # Pull out the grid index-offset that this kernel expects and
        # store it here. This is used to check that all of the kernels
        # invoked by an application are using compatible index offsets.
        self._index_offset = call.ktype.index_offset

    def local_vars(self):
        '''Return a list of the variable (names) that are local to this loop
        (and must therefore be e.g. threadprivate if doing OpenMP)

        '''
        return []

    def gen_code(self, parent):
        '''
        Generates GOcean v1.0 specific psy code for a call to the
        kernel instance. Also ensures that the kernel is written to file
        if it has been transformed.

        :param parent: parent node in the f2pygen AST being created.
        :type parent: :py:class:`psyclone.f2pygen.LoopGen`

        :raises GenerationError: if the kernel requires a grid property but \
                                 does not have any field arguments.
        :raises GenerationError: if it encounters a kernel argument of \
                                 unrecognised type.
        '''
        from psyclone.f2pygen import CallGen, UseGen

        # If the kernel has been transformed then we rename it. If it
        # is *not* being module inlined then we also write it to file.
        self.rename_and_write()

        if self.root.opencl:
            # OpenCL is completely different so has its own gen method.
            self.gen_ocl(parent)
            return

        arguments = self._arguments.raw_arg_list()
        parent.add(CallGen(parent, self._name, arguments))
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name, only=True,
                              funcnames=[self._name]))

    def gen_ocl(self, parent):
        '''
        Generates code for the OpenCL invocation of this kernel.

        :param parent: Parent node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import CallGen, DeclGen, AssignGen, CommentGen
        # Create the array used to specify the iteration space of the kernel
        garg = self._arguments.find_grid_access()
        glob_size = self._name_space_manager.create_name(
            root_name="globalsize", context="PSyVars", label="globalsize")
        parent.add(DeclGen(parent, datatype="integer", target=True,
                           kind="c_size_t", entity_decls=[glob_size + "(2)"]))
        parent.add(AssignGen(
            parent, lhs=glob_size,
            rhs="(/{0}%grid%nx, {0}%grid%ny/)".format(garg.name)))

        base = "kernel_" + self._name
        kernel = self._name_space_manager.create_name(root_name=base,
                                                      context="PSyVars",
                                                      label=base)
        # Generate code to ensure data is on device
        self.gen_data_on_ocl_device(parent)

        # Then we set the kernel arguments
        arguments = [kernel, garg.name+"%grid%nx"]
        for arg in self._arguments.args:
            if arg.type == "scalar":
                arguments.append(arg.name)
            elif arg.type == "field":
                arguments.append(arg.name + "%device_ptr")
            elif arg.type == "grid_property":
                # TODO (dl_esm_inf/#18) the dl_esm_inf library stores
                # the pointers to device memory for grid properties in
                # "<grid-prop-name>_device" which is a bit hacky but
                # works for now.
                arguments.append(garg.name+"%grid%"+arg.name+"_device")
        sub_name = self._name_space_manager.create_name(
            root_name=self.name+"_set_args", context=self.name+"ArgSetter",
            label=self.name+"_set_args")
        parent.add(CallGen(parent, sub_name, arguments))

        # Get the name of the list of command queues (set in
        # psyGen.InvokeSchedule)
        qlist = self._name_space_manager.create_name(
            root_name="cmd_queues", context="PSyVars", label="cmd_queues")
        flag = self._name_space_manager.create_name(
            root_name="ierr", context="PSyVars", label="ierr")

        # Then we call clEnqueueNDRangeKernel
        parent.add(CommentGen(parent, " Launch the kernel"))
        cnull = "C_NULL_PTR"
        cmd_queue = qlist + "(1)"

        args = ", ".join([cmd_queue, kernel, "2", cnull,
                          "C_LOC({0})".format(glob_size),
                          cnull, "0", cnull, cnull])
        parent.add(AssignGen(parent, lhs=flag,
                             rhs="clEnqueueNDRangeKernel({0})".format(args)))
        parent.add(CommentGen(parent, ""))

    @property
    def index_offset(self):
        ''' The grid index-offset convention that this kernel expects '''
        return self._index_offset

    def gen_arg_setter_code(self, parent):
        '''
        Creates a Fortran routine to set the arguments of the OpenCL
        version of this kernel.

        :param parent: Parent node of the set-kernel-arguments routine
        :type parent: :py:class:`psyclone.f2pygen.moduleGen`
        '''
        from psyclone.f2pygen import SubroutineGen, UseGen, DeclGen, \
            AssignGen, CommentGen
        # Currently literal arguments are checked for and rejected by
        # the OpenCL transformation.
        kobj = self._name_space_manager.create_name(
            root_name="kernel_obj", context="ArgSetter", label="kernel_obj")
        nx_name = self._name_space_manager.create_name(
            root_name="nx", context="ArgSetter", label="nx")
        args = [kobj, nx_name] + [arg.name for arg in self._arguments.args]

        sub_name = self._name_space_manager.create_name(
            root_name=self.name+"_set_args", context=self.name+"ArgSetter",
            label=self.name+"_set_args")
        sub = SubroutineGen(parent, name=sub_name, args=args)
        parent.add(sub)
        sub.add(UseGen(sub, name="ocl_utils_mod", only=True,
                       funcnames=["check_status"]))
        sub.add(UseGen(sub, name="iso_c_binding", only=True,
                       funcnames=["c_sizeof", "c_loc", "c_intptr_t"]))
        sub.add(UseGen(sub, name="clfortran", only=True,
                       funcnames=["clSetKernelArg"]))
        # Declare arguments
        sub.add(DeclGen(sub, datatype="integer", target=True,
                        entity_decls=[nx_name]))
        sub.add(DeclGen(sub, datatype="integer", kind="c_intptr_t",
                        target=True, entity_decls=[kobj]))

        # Arrays (grid properties and fields)
        args = args_filter(self._arguments.args,
                           arg_types=["field", "grid_property"])
        if args:
            sub.add(DeclGen(sub, datatype="integer", kind="c_intptr_t",
                            target=True,
                            entity_decls=[arg.name for arg in args]))
        # Scalars
        args = args_filter(self._arguments.args,
                           arg_types=["scalar"],
                           is_literal=False)
        for arg in args:
            if arg.space.lower() == "go_r_scalar":
                sub.add(DeclGen(
                    sub, datatype="REAL", intent="in", kind="go_wp",
                    target=True, entity_decls=[arg.name]))
            else:
                sub.add(DeclGen(sub, datatype="INTEGER", intent="in",
                                target=True, entity_decls=[arg.name]))

        # Declare local variables
        err_name = self._name_space_manager.create_name(
            root_name="ierr", context="PSyVars", label="ierr")
        sub.add(DeclGen(sub, datatype="integer", entity_decls=[err_name]))
        sub.add(CommentGen(
            sub,
            " Set the arguments for the {0} OpenCL Kernel".format(self.name)))
        # We must always pass "nx" (the horizontal dimension of the grid) into
        # a kernel
        index = 0
        sub.add(AssignGen(
            sub, lhs=err_name,
            rhs="clSetKernelArg({0}, {1}, C_SIZEOF({2}), C_LOC({2}))".
            format(kobj, index, nx_name)))
        # Now all of the 'standard' kernel arguments
        for arg in self.arguments.args:
            index += 1
            arg.set_kernel_arg(sub, index, self.name)

    def gen_data_on_ocl_device(self, parent):
        '''
        Generate code to create data buffers on OpenCL device.

        :param parent: Parent subroutine in f2pygen AST of generated code.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import UseGen, CommentGen, IfThenGen, DeclGen, \
            AssignGen
        grid_arg = self._arguments.find_grid_access()
        # Ensure the fields required by this kernel are on device. We must
        # create the buffers for them if they're not.
        parent.add(UseGen(parent, name="fortcl", only=True,
                          funcnames=["create_rw_buffer"]))
        parent.add(CommentGen(parent, " Ensure field data is on device"))
        for arg in self._arguments.args:
            if arg.type == "field" or arg.type == "grid_property":

                if arg.type == "field":
                    # fields have a 'data_on_device' property for keeping
                    # track of whether they are on the device
                    condition = ".NOT. {0}%data_on_device".format(arg.name)
                    device_buff = "{0}%device_ptr".format(arg.name)
                    host_buff = "{0}%data".format(arg.name)
                else:
                    # grid properties do not have such an attribute (because
                    # they are just arrays) so we check whether the device
                    # pointer is NULL.
                    device_buff = "{0}%grid%{1}_device".format(grid_arg.name,
                                                               arg.name)
                    condition = device_buff + " == 0"
                    host_buff = "{0}%grid%{1}".format(grid_arg.name, arg.name)
                # Name of variable to hold no. of bytes of storage required
                nbytes = self._name_space_manager.create_name(
                    root_name="size_in_bytes", context="PSyVars",
                    label="size_in_bytes")
                # Variable to hold write event returned by OpenCL runtime
                wevent = self._name_space_manager.create_name(
                    root_name="write_event", context="PSyVars",
                    label="write_event")
                ifthen = IfThenGen(parent, condition)
                parent.add(ifthen)
                parent.add(DeclGen(parent, datatype="integer", kind="c_size_t",
                                   entity_decls=[nbytes]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind="c_intptr_t", target=True,
                                   entity_decls=[wevent]))
                # Use c_sizeof() on first element of array to be copied over in
                # order to cope with the fact that some grid properties are
                # integer.
                size_expr = ("int({0}%grid%nx*{0}%grid%ny, 8)*c_sizeof("
                             "{1}(1,1))".format(grid_arg.name, host_buff))
                ifthen.add(AssignGen(ifthen, lhs=nbytes, rhs=size_expr))
                ifthen.add(CommentGen(ifthen, " Create buffer on device"))
                # Get the name of the list of command queues (set in
                # psyGen.InvokeSchedule)
                qlist = self._name_space_manager.create_name(
                    root_name="cmd_queues", context="PSyVars",
                    label="cmd_queues")
                flag = self._name_space_manager.create_name(
                    root_name="ierr", context="PSyVars", label="ierr")

                ifthen.add(AssignGen(ifthen, lhs=device_buff,
                                     rhs="create_rw_buffer(" + nbytes + ")"))
                ifthen.add(
                    AssignGen(ifthen, lhs=flag,
                              rhs="clEnqueueWriteBuffer({0}(1), {1}, CL_TRUE, "
                              "0_8, {2}, C_LOC({3}), 0, C_NULL_PTR, "
                              "C_LOC({4}))".format(qlist, device_buff,
                                                   nbytes, host_buff, wevent)))
                if arg.type == "field":
                    ifthen.add(AssignGen(
                        ifthen, lhs="{0}%data_on_device".format(arg.name),
                        rhs=".true."))

        # Ensure data copies have finished
        parent.add(CommentGen(parent,
                              " Block until data copies have finished"))
        parent.add(AssignGen(parent, lhs=flag,
                             rhs="clFinish(" + qlist + "(1))"))

    def get_kernel_schedule(self):
        '''
        Returns a PSyIR Schedule representing the GOcean kernel code.

        :return: Schedule representing the kernel code.
        :rtype: :py:class:`psyclone.psyGen.GOKernelSchedule`
        '''
        if self._kern_schedule is None:
            astp = GOFparser2ASTProcessor()
            self._kern_schedule = astp.generate_schedule(self.name, self.ast)
        return self._kern_schedule


class GOFparser2ASTProcessor(Fparser2ASTProcessor):
    '''
    Sub-classes the Fparser2ASTProcessor with GOcean 1.0 specific
    functionality.
    '''
    @staticmethod
    def _create_schedule(name):
        '''
        Create an empty KernelSchedule.

        :param str name: Name of the subroutine represented by the kernel.
        :returns: New GOKernelSchedule empty object.
        :rtype: py:class:`psyclone.gocean1p0.GOKernelSchedule`
        '''
        return GOKernelSchedule(name)


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

    def raw_arg_list(self):
        '''
        :returns: a list of all of the actual arguments to the \
                  kernel call.
        :rtype: list of str

        :raises GenerationError: if the kernel requires a grid property \
                                 but has no field arguments.
        :raises InternalError: if we encounter a kernel argument with an \
                               unrecognised type.
        '''
        if self._raw_arg_list:
            return self._raw_arg_list

        # Before we do anything else, go through the arguments and
        # determine the best one from which to obtain the grid properties.
        grid_arg = self.find_grid_access()

        # A GOcean 1.0 kernel always requires the [i,j] indices of the
        # grid-point that is to be updated
        arguments = ["i", "j"]
        for arg in self._args:

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
                        format(self._parent_call.name, arg.name))
                else:
                    arguments.append(grid_arg.name+"%grid%"+arg.name)
            else:
                raise InternalError("Kernel {0}, argument {1} has "
                                    "unrecognised type: '{2}'".
                                    format(self._parent_call.name, arg.name,
                                           arg.type))
        self._raw_arg_list = arguments
        return self._raw_arg_list

    def find_grid_access(self):
        '''
        Determine the best kernel argument from which to get properties of
        the grid. For this, an argument must be a field (i.e. not
        a scalar) and must be supplied by the algorithm layer
        (i.e. not a grid property). If possible it should also be
        a field that is read-only as otherwise compilers can get
        confused about data dependencies and refuse to SIMD
        vectorise.
        :returns: the argument object from which to get grid properties.
        :rtype: :py:class:`psyclone.gocean1p0.GOKernelArgument` or None
        '''
        for access in [AccessType.READ, AccessType.READWRITE,
                       AccessType.WRITE]:
            for arg in self._args:
                if arg.type == "field" and arg.access == access:
                    return arg
        # We failed to find any kernel argument which could be used
        # to access the grid properties. This will only be a problem
        # if the kernel requires a grid-property argument.
        return None

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this makes no
            sense for GOcean. Need to refactor the Invoke base class and
            remove the need for this property (#279). '''
        return self._dofs

    @property
    def acc_args(self):
        '''
        Provide the list of references (both objects and arrays) that must
        be present on an OpenACC device before the kernel associated with
        this Arguments object may be launched.

        :returns: list of (Fortran) quantities
        :rtype: list of str
        '''
        arg_list = []

        # First off, specify the field object which we will de-reference in
        # order to get any grid properties (if this kernel requires them).
        # We do this as some compilers do less optimisation if we get (read-
        # -only) grid properties from a field object that has read-write
        # access.
        grid_fld = self.find_grid_access()
        grid_ptr = grid_fld.name + "%grid"
        arg_list.extend([grid_fld.name, grid_fld.name+"%data"])

        for arg in self._args:
            if arg.type == "scalar":
                arg_list.append(arg.name)
            elif arg.type == "field" and arg != grid_fld:
                # The remote device will need the reference to the field
                # object *and* the reference to the array within that object.
                arg_list.extend([arg.name, arg.name+"%data"])
            elif arg.type == "grid_property":
                if grid_ptr not in arg_list:
                    # This kernel needs a grid property and therefore the
                    # pointer to the grid object must be copied to the device.
                    arg_list.append(grid_ptr)
                arg_list.append(grid_ptr+"%"+arg.name)
        return arg_list

    @property
    def fields(self):
        '''
        Provides the list of names of field objects that are required by
        the kernel associated with this Arguments object.

        :returns: List of names of (Fortran) field objects.
        :rtype: list of str
        '''
        args = args_filter(self._args, arg_types=["field"])
        return [arg.name for arg in args]

    @property
    def scalars(self):
        '''
        Provides the list of names of scalar arguments required by the
        kernel associated with this Arguments object. If there are none
        then the returned list is empty.

        :returns: A list of the names of scalar arguments in this object.
        :rtype: list of str
        '''
        args = args_filter(self._args, arg_types=["scalar"])
        return [arg.name for arg in args]


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
    '''
    Describes arguments that supply grid properties to a kernel.
    These arguments are provided by the PSy layer rather than in
    the Algorithm layer.

    :param arg: the meta-data entry describing the required grid property.
    :type arg: :py:class:`psyclone.gocean1p0.GO1p0Descriptor`

    :raises GenerationError: if the grid property is not recognised.

    '''
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
        # Access to the name-space manager
        self._name_space_manager = NameSpaceFactory().create()

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

    # pylint: disable=too-many-branches
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

        go_stencil(010,  !   N
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

        go_stencil(000,
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
                "format 'go_stencil(...)'".format(kernel_name,
                                                  str(stencil_info)))

        # Get the name
        name = stencil_info.name.lower()

        if stencil_info.args:
            # The stencil info is of the form 'name(a,b,...), so the
            # name should be 'stencil' and there should be 3
            # arguments'
            self._has_stencil = True
            args = stencil_info.args
            if name != "go_stencil":
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument is '{1}' but must be "
                    "'go_stencil(...)".format(kernel_name, name))
            if len(args) != 3:
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument with format "
                    "'go_stencil(...)', has {1} arguments but should have "
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
                        "'go_stencil(...)'. Argument index {1} should be a "
                        "number but found "
                        "'{2}'.".format(kernel_name, arg_idx, str(arg)))
                if len(arg) != 3:
                    raise ParseError(
                        "Meta-data error in kernel '{0}': 3rd descriptor "
                        "(stencil) of field argument with format "
                        "'go_stencil(...)'. Argument index {1} should "
                        "consist of 3 digits but found "
                        "{2}.".format(kernel_name, arg_idx, len(arg)))
            # The central value is constrained to be 0 or 1
            if args[1][1] not in ["0", "1"]:
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument with format "
                    "'go_stencil(...)'. Argument index 1 position 1 "
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
                    "'go_stencil(...)'. A zero sized stencil has been "
                    "specified. This should be specified with the "
                    "'go_pointwise' keyword.".format(kernel_name))
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
                    "of {2} or go_stencil(...)".format(kernel_name, name,
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

        api_config = Config.get().api_conf("gocean1.0")
        access_mapping = api_config.get_access_mapping()
        try:
            access_type = access_mapping[access]
        except KeyError:
            valid_names = api_config.get_valid_accesses_api()
            raise ParseError("Meta-data error in kernel {0}: argument "
                             "access  is given as '{1}' but must be "
                             "one of {2}".
                             format(kernel_name, access, valid_names))

        # Finally we can call the __init__ method of our base class
        Descriptor.__init__(self, access_type, funcspace, stencil_info)

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
            if init.name != 'go_arg':
                raise ParseError("Each meta_arg value must be of type " +
                                 "'go_arg' for the gocean1.0 api, but " +
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


class GOACCEnterDataDirective(ACCEnterDataDirective):
    '''
    Sub-classes ACCEnterDataDirective to provide an API-specific implementation
    of data_on_device().

    '''
    def data_on_device(self, parent):
        '''
        Adds nodes into the f2pygen AST to flag that each of the
        objects required by the kernels in the data region is now on the
        device. We do this by setting the data_on_device attribute to .true.

        :param parent: The node in the f2pygen AST to which to add the \
                       assignment nodes.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        '''
        from psyclone.f2pygen import AssignGen
        obj_list = []
        for pdir in self._acc_dirs:
            for var in pdir.fields:
                if var not in obj_list:
                    parent.add(AssignGen(parent,
                                         lhs=var+"%data_on_device",
                                         rhs=".true."))
                    obj_list.append(var)
        return


class GOSymbolTable(SymbolTable):
    '''
    Sub-classes SymbolTable to provide an API-specific implementation of the
    OpenCL generation methods.
    '''

    def _check_gocean_conformity(self):
        '''
        Checks that the Symbol Table has at least 2 arguments which represent
        the iteration indices (are scalar integers).

        :raises GenerationError: if the Symbol Table does not conform to the \
                rules for a GOcean 1.0 kernel.
        '''
        # Get the kernel name if available for better error messages
        kname_str = ""
        if self._kernel:
            kname_str = " for kernel '{0}'".format(self._kernel.name)

        # Check that there are at least 2 arguments
        if len(self.argument_list) < 2:
            raise GenerationError(
                "GOcean 1.0 API kernels should always have at least two "
                "arguments representing the iteration indices but the "
                "Symbol Table{0} has only {1} argument(s)."
                "".format(kname_str,
                          str(len(self.argument_list)))
                )

        # Check that first 2 arguments are scalar integers
        for pos, posstr in [(0, "first"), (1, "second")]:
            dtype = self.argument_list[pos].datatype
            shape_len = len(self.argument_list[pos].shape)
            if (dtype != "integer" or shape_len != 0):
                if shape_len == 0:
                    shape_str = "a scalar"
                else:
                    shape_str = "an array"
                raise GenerationError(
                    "GOcean 1.0 API kernels {0} argument should be a scalar "
                    "integer but got {1} of type '{2}'{3}."
                    "".format(posstr, shape_str, str(dtype), kname_str))

    def gen_ocl_argument_list(self, indent=0):
        '''
        Generate kernel arguments: in OpenCL we ignore the iteration
        indices which in GOcean are the first two arguments.

        :param indent: Depth of indent for the output string.
        :return: OpenCL argument list for the Symbol Table.
        :rtype: str
        '''
        self._check_gocean_conformity()

        arglist = []
        for symbol in self.argument_list[2:]:
            prefix = Node.indent(indent)
            # If argument is an array, it is allocated to the OpenCL global
            # address space.
            if symbol.shape:
                prefix += "__global "
            arglist.append(prefix + symbol.gen_c_definition())

        return ",\n".join(arglist)  # Remove last ",\n"

    def gen_ocl_iteration_indices(self, indent=0):
        '''
        Generate OpenCL iteration indices using the names of the first 2
        arguments (e.g. "int i = get_global_id(0);")

        :return: OpenCL iteration indices definition and initialisation.
        :rtype: str
        '''
        self._check_gocean_conformity()

        code = ""
        for index, symbol in enumerate(self.argument_list[:2]):
            code += Node.indent(indent) + "int " + symbol.name
            code += " = get_global_id(" + str(index) + ");\n"
        return code

    def gen_ocl_array_length(self, indent=0):
        '''
        Generate a <name>LEN<DIM> variable for each array dimension of
        each array argument.
        In OpenCL the sizes are retrived from the kernel global_work_size
        (e.g. "int arrayLEN1 = get_global_size(1);")

        :return: OpenCL code to define and initialise variables for all array \
                lengths.
        :rtype: str
        :raises GenerationError: if the array length variable name clashes \
                with another symbol name.
        '''
        self._check_gocean_conformity()

        code = ""
        for symbol in self.argument_list[2:]:
            dimensions = len(symbol.shape)
            for dim in range(1, dimensions + 1):
                code += Node.indent(indent) + "int "
                varname = symbol.name + "LEN" + str(dim)

                # Check there is no clash with other variables
                if varname in self:
                    kname = ""
                    if self._kernel:
                        kname = "'{0}'".format(self._kernel.name)
                    raise GenerationError(
                        "Unable to declare the variable '{0}' to store the "
                        "length of '{1}' because the kernel {2} already "
                        "contains a symbol with the same name."
                        "".format(varname, symbol.name, kname))

                code += varname + " = get_global_size("
                code += str(dim - 1) + ");\n"
        return code


class GOKernelSchedule(KernelSchedule):
    '''
    Sub-classes KernelSchedule to provide an API-specific implementation of the
    OpenCL generation method.

    :param str name: Kernel subroutine name
    '''
    def __init__(self, name):
        super(GOKernelSchedule, self).__init__(name)
        self._symbol_table = GOSymbolTable(self)

    def gen_ocl(self, indent=0):
        '''
        Generate a string representation of this node in the OpenCL language.

        :param int indent: Depth of indent for the output string.
        :return: OpenCL language code representing the node.
        :rtype: string
        '''

        # OpenCL implementation assumptions:
        # - All array have the same size and it is given by the
        #   global_work_size argument to clEnqueueNDRangeKernel.
        # - Assumes no dependencies among kernels called concurrently.

        # TODO: At the moment, the method caller is responsible to ensure
        # these assumptions. KernelSchedule access to the kernel
        # meta-arguments could be used to check them and also improve the
        # generated code. (Issue #288)

        # Start OpenCL kernel definition
        code = self.indent(indent) + "__kernel void " + self._name + "(\n"
        code += self.symbol_table.gen_ocl_argument_list(indent + 1)
        code += "\n" + self.indent(indent + 1) + "){\n"

        # Declare local variables.
        code += self.symbol_table.gen_c_local_variables(indent + 1)

        # Declare array length
        code += self.symbol_table.gen_ocl_array_length(indent + 1)

        # Declare iteration indices
        code += self.symbol_table.gen_ocl_iteration_indices(indent + 1)

        # Generate kernel body
        for child in self._children:
            code += child.gen_c_code(indent + 1) + "\n"

        # Close kernel definition
        code += "}\n"

        return code
