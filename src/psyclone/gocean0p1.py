# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-19, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module implements the emerging PSyclone GOcean API by specialising
    the required base classes (PSy, Invokes, Invoke, Schedule, Loop, Kern,
    Arguments and KernelArgument). '''

from __future__ import absolute_import
from psyclone.psyGen import PSy, Invokes, Invoke, Schedule, Loop, Kern, \
    Arguments, KernelArgument


class GOPSy(PSy):
    '''
    The GOcean specific PSy class. This creates a GOcean specific
    invokes object (which controls all the required invocation calls).
    Also overrides the PSy gen method so that we generate GOceaen
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
        Generate PSy code for the GOcean api.

        :rtype: ast

        '''
        from psyclone.f2pygen import ModuleGen, UseGen

        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include the kind_params module
        psy_module.add(UseGen(psy_module, name="kind_params_mod"))
        # include the topology_mod module for loop bounds
        psy_module.add(UseGen(psy_module, name="topology_mod"))
        # include the field_mod module in case we have any r-space variables
        psy_module.add(UseGen(psy_module, name="field_mod",
                              only=["scalar_field_type"]))
        # add in the subroutines for each invocation
        self.invokes.gen_code(psy_module)
        # inline kernels where requested
        self.inline(psy_module)
        return psy_module.root


class GOInvokes(Invokes):
    ''' The GOcean specific invokes class. This passes the GOcean specific
        invoke class to the base class so it creates the one we require. '''
    def __init__(self, alg_calls):
        # pylint: disable=using-constant-test
        if False:
            self._0_to_n = GOInvoke(None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, GOInvoke)


class GOInvoke(Invoke):
    ''' The GOcean specific invoke class. This passes the GOcean specific
        schedule class to the base class so it creates the one we require.
        A set of GOcean infrastructure reserved names are also passed to
        ensure that there are no name clashes. Also overrides the gen_code
        method so that we generate GOcean specific invocation code and
        provides to methods which separate arguments that are arrays from
        arguments that are scalars. '''
    def __init__(self, alg_invocation, idx):
        # pylint: disable=using-constant-test
        if False:
            self._schedule = GOSchedule(None)  # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, GOSchedule,
                        reserved_names=["cf", "ct", "cu", "cv"])

    @property
    def unique_args_arrays(self):
        ''' find unique arguments that are arrays (defined as those that are
            not rspace). GOcean needs to kow this as we are dealing with
            arrays directly so need to declare them correctly. '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if(not arg.is_literal and
                   not arg.space.lower() == "r" and
                   arg.name not in result):
                    result.append(arg.name)
        return result

    @property
    def unique_args_scalars(self):
        ''' find unique arguments that are scalars (defined as those that are
            rspace). GOcean needs to kow this as we are dealing with arrays
            directly so need to declare them correctly. '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if(not arg.is_literal and
                   arg.space.lower() == "r" and
                   arg.name not in result):
                    result.append(arg.name)
        return result

    def gen_code(self, parent):
        ''' Generates GOcean specific invocation code (the subroutine called
            by the associated invoke call in the algorithm layer). This
            consists of the PSy invocation subroutine and the declaration of
            its arguments.'''
        from psyclone.f2pygen import SubroutineGen, DeclGen, TypeDeclGen
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names)
        parent.add(invoke_sub)
        self.schedule.gen_code(invoke_sub)
        # add the subroutine argument declarations for arrays
        if len(self.unique_args_arrays) > 0:
            my_decl_arrays = DeclGen(invoke_sub, datatype="REAL",
                                     intent="inout", kind="go_wp",
                                     entity_decls=self.unique_args_arrays,
                                     dimension=":,:")
            invoke_sub.add(my_decl_arrays)
        # add the subroutine argument declarations for scalars
        if len(self.unique_args_scalars) > 0:
            my_decl_scalars = \
                TypeDeclGen(invoke_sub,
                            datatype="scalar_field_type",
                            entity_decls=self.unique_args_scalars,
                            intent="inout")
            invoke_sub.add(my_decl_scalars)


class GOSchedule(Schedule):
    ''' The GOcean specific schedule class. All we have to do is supply our
    API-specific factories to the base Schedule class constructor. '''

    def __init__(self, alg_calls):
        Schedule.__init__(self, GOKernCallFactory, GOBuiltInCallFactory,
                          alg_calls)


class GOLoop(Loop):
    ''' The GOcean specific Loop class. This passes the GOcean specific
        single loop information to the base class so it creates the one we
        require. Adds a GOcean specific setBounds method which tells the loop
        what to iterate over. Need to harmonise with the topology_name method
        in the Dynamo api. '''
    def __init__(self, parent=None,
                 topology_name="", loop_type=""):
        Loop.__init__(self, parent=parent,
                      valid_loop_types=["inner", "outer"])
        self.loop_type = loop_type

        if self._loop_type == "inner":
            self._variable_name = "i"
        elif self._loop_type == "outer":
            self._variable_name = "j"

    def gen_code(self, parent):

        if self.field_space == "every":
            from psyclone.f2pygen import DeclGen
            dim_var = DeclGen(parent, datatype="INTEGER",
                              entity_decls=[self._variable_name])
            parent.add(dim_var)

            # loop bounds
            self._start = "1"
            if self._loop_type == "inner":
                index = "1"
            elif self._loop_type == "outer":
                index = "2"
            self._stop = ("SIZE(" + self.field_name + ", " +
                          index + ")")

        else:  # one of our spaces so use values provided by the infrastructure

            # loop bounds
            if self._loop_type == "inner":
                self._start = self.field_space + "%istart"
                self._stop = self.field_space + "%istop"
            elif self._loop_type == "outer":
                self._start = self.field_space + "%jstart"
                self._stop = self.field_space + "%jstop"

        Loop.gen_code(self, parent)


class GOBuiltInCallFactory(object):
    ''' A GOcean-specific factory for calls to built-ins. No built-in
        calls are supported in GOcean 0.1 so we do nothing. '''
    @staticmethod
    def create():
        ''' Creates a specific built-in call. Currently just an
            empty stub. '''
        return None


class GOKernCallFactory(object):
    ''' A GOcean-specific kernel call factory. '''
    @staticmethod
    def create(call, parent=None):
        ''' Creates a kernel call and associated Loop structure '''
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
        inner_loop.field_space = gocall.arguments.\
            iteration_space_arg().function_space
        outer_loop.field_space = inner_loop.field_space
        inner_loop.field_name = gocall.arguments.\
            iteration_space_arg().name
        outer_loop.field_name = inner_loop.field_name
        return outer_loop


class GOKern(Kern):
    ''' Stores information about GOcean Kernels as specified by the Kernel
        metadata. Uses this information to generate appropriate PSy layer
        code for the Kernel instance. Specialises the gen_code method to
        create the appropriate GOcean specific kernel call. '''
    def __init__(self):
        # pylint: disable=using-constant-test
        if False:
            self._arguments = GOKernelArguments(None, None)  # for pyreverse

    def load(self, call, parent=None):
        ''' Load this GOKern object with state pulled from the call object '''
        Kern.__init__(self, GOKernelArguments, call, parent)

    def local_vars(self):
        return []

    def gen_code(self, parent):
        ''' Generates GOcean specific psy code for a call to the dynamo
            kernel instance. '''
        from psyclone.f2pygen import CallGen, UseGen
        arguments = ["i", "j"]
        for arg in self._arguments.args:
            if arg.space.lower() == "r":
                arguments.append(arg.name + "%data")
            else:
                arguments.append(arg.name)
        parent.add(CallGen(parent, self._name, arguments))
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name, only=True,
                              funcnames=[self._name]))


class GOKernelArguments(Arguments):
    ''' Provides information about GOcean kernel call arguments collectively,
        as specified by the kernel argument metadata. This class ensures that
        initialisation is performed correctly. It also adds three '''
    def __init__(self, call, parent_call):
        # pylint: disable=using-constant-test
        if False:
            self._0_to_n = GOKernelArgument(None, None, None)  # for pyreverse
        Arguments.__init__(self, parent_call)
        self._args = []
        for (idx, arg) in enumerate(call.ktype.arg_descriptors):
            self._args.append(GOKernelArgument(arg, call.args[idx],
                                               parent_call))
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
            my_mapping = {"write": "write", "read": "read",
                          "readwrite": "readwrite", "inc": "inc"}
        arg = Arguments.iteration_space_arg(self, my_mapping)
        return arg


class GOKernelArgument(KernelArgument):
    ''' Provides information about individual GOcean kernel call arguments
        as specified by the kernel argument metadata. Only passes information
        onto the base class. '''
    def __init__(self, arg, arg_info, call):
        self._arg = arg
        KernelArgument.__init__(self, arg, arg_info, call)

    @property
    def function_space(self):
        ''' Returns the expected finite difference space for this
            argument as specified by the kernel argument metadata.'''
        return self._arg.function_space
