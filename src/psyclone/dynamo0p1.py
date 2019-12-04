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
# -----------------------------------------------------------------------------

''' This module implements the PSyclone Dynamo 0.1 API by specialising the
    required base classes (PSy, Invokes, Invoke, InvokeSchedule, Loop, Kern,
    Arguments and Argument). '''

from __future__ import absolute_import
from psyclone.configuration import Config
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, Loop, \
    CodedKern, Arguments, Argument, GenerationError, Literal, Reference, \
    Schedule
from psyclone.parse.kernel import KernelType, Descriptor
from psyclone.parse.utils import ParseError


class DynDescriptor(Descriptor):
    '''This class captures the dynamo0.1 api metadata found in an argument
    descriptor within the kernel metadata.

    :param str access: An access descriptor describing how the \
    argument is used in the kernel
    :param str funcspace: A description of the function space that \
    this argument is assumed to be on.
    :param str stencil: The type of stencil access performed by the \
    kernel on this argument. Currently the value is limited to 'fe'.
    :param str basis: Whether or not basis information is required for \
    this field. The values can be '.true.' or '.false.'.
    :param str diff_basis: Whether or not basis information is \
    required for this field. The value can be '.true.' or '.false.'.
    :param str gauss_quad: Whether or not gaussian quadrature \
    information is required for this field. The value can be '.true.' \
    or '.false.'.

    '''
    def __init__(self, access, funcspace, stencil, basis, diff_basis,
                 gauss_quad):
        Descriptor.__init__(self, access, funcspace, stencil)
        self._basis = basis
        self._diff_basis = diff_basis
        self._gauss_quad = gauss_quad

    @property
    def basis(self):
        '''Return whether a basis function is required or not.

        :returns: '.true.' if a basis function is required and \
        '.false.' if not.
        :rtype: str

        '''
        return self._basis

    @property
    def diff_basis(self):
        '''Return whether a differential basis function is required or not.

        :returns: '.true.' if a differential basis function is \
        required and '.false.' if not.
        :rtype: str

        '''
        return self._diff_basis

    @property
    def gauss_quad(self):
        '''Return whether gaussian quadrature is required or not.

        :returns: '.true.' if a gaussian quadrature is required and \
        '.false.' if not.
        :rtype: str

        '''
        return self._gauss_quad


class DynKernelType(KernelType):
    '''This class captures the dynamo0.1 api kernel metadata by extracting
    it from the supplied AST.

    :param ast: fparser1 AST for the parsed gocean0.1 kernel \
    meta-data.
    :type ast: :py:class:`fparser.one.block_statements.BeginSource`
    :param name: name of the Fortran derived type describing the \
    kernel. This is an optional argument which defaults to `None`
    :type name: str or NoneType.

    '''

    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)
        self._arg_descriptors = []
        for init in self._inits:
            if init.name != 'arg_type':
                raise ParseError(
                    "dynamo0p1.py:DynKernelType:__init__: Each meta_arg "
                    "value must be of type 'arg_type' for the "
                    "dynamo0.1 api, but found '{0}'.".format(init.name))
            access = init.args[0].name
            funcspace = init.args[1].name
            stencil = init.args[2].name
            x1 = init.args[3].name
            x2 = init.args[4].name
            x3 = init.args[5].name
            self._arg_descriptors.append(DynDescriptor(access, funcspace,
                                                       stencil, x1, x2, x3))


class DynamoPSy(PSy):
    '''
    The Dynamo specific PSy class. This creates a Dynamo specific
    invokes object (which controls all the required invocation calls).
    Also overrides the PSy gen method so that we generate dynamo
    specific PSy module code.

    :param invoke_info: An object containing the required invocation \
                        information for code optimisation and generation.
    :type invoke_info: :py:class:`psyclone.parse.FileInfo`
    '''
    def __init__(self, invoke_info):
        PSy.__init__(self, invoke_info)
        self._invokes = DynamoInvokes(invoke_info.calls)

    @property
    def gen(self):
        '''
        Generate PSy code for the Dynamo0.1 api.

        :rtype: ast

        '''
        from psyclone.f2pygen import ModuleGen, UseGen

        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include the lfric module
        lfric_use = UseGen(psy_module, name="lfric")
        psy_module.add(lfric_use)
        # add all invoke specific information
        self.invokes.gen_code(psy_module)
        # inline kernel subroutines if requested
        self.inline(psy_module)
        return psy_module.root


class DynamoInvokes(Invokes):
    ''' The Dynamo specific invokes class. This passes the Dynamo specific
        invoke class to the base class so it creates the one we require. '''
    def __init__(self, alg_calls):
        if False:
            self._0_to_n = DynInvoke(None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, DynInvoke)


class DynInvoke(Invoke):
    ''' The Dynamo specific invoke class. This passes the Dynamo specific
        schedule class to the base class so it creates the one we require.
        Also overrides the gen_code method so that we generate dynamo
        specific invocation code. '''
    def __init__(self, alg_invocation, idx):
        if False:
            self._schedule = DynInvokeSchedule(None)  # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, DynInvokeSchedule)

    def gen_code(self, parent):
        ''' Generates Dynamo specific invocation code (the subroutine called
            by the associated invoke call in the algorithm layer). This
            consists of the PSy invocation subroutine and the declaration of
            its arguments.'''
        from psyclone.f2pygen import SubroutineGen, TypeDeclGen
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names)
        self.schedule.gen_code(invoke_sub)
        parent.add(invoke_sub)
        # add the subroutine argument declarations
        my_typedecl = TypeDeclGen(invoke_sub, datatype="field_type",
                                  entity_decls=self.psy_unique_var_names,
                                  intent="inout")
        invoke_sub.add(my_typedecl)


class DynInvokeSchedule(InvokeSchedule):
    ''' The Dynamo specific InvokeSchedule sub-class. This passes the Dynamo
        specific loop and infrastructure classes to the base class so it
        creates the ones we require. '''
    def __init__(self, arg):
        InvokeSchedule.__init__(self, DynKernCallFactory,
                                DynBuiltInCallFactory, arg)


class DynLoop(Loop):
    ''' The Dynamo specific Loop class. This passes the Dynamo specific
        loop information to the base class so it creates the one we require.
        Creates Dynamo specific loop bounds when the code is being generated.
    '''
    def __init__(self, parent=None, loop_type=""):
        Loop.__init__(self, parent=parent,
                      valid_loop_types=["", "colours", "colour"])
        self.loop_type = loop_type

        # Work out the variable name from  the loop type
        if self._loop_type == "colours":
            self._variable_name = "colour"
        elif self._loop_type == "colour":
            self._variable_name = "cell"
        else:
            self._variable_name = "cell"

        # Pre-initialise the Loop children  # TODO: See issue #440
        self.addchild(Literal("NOT_INITIALISED", parent=self))  # start
        self.addchild(Literal("NOT_INITIALISED", parent=self))  # stop
        self.addchild(Literal("1", parent=self))  # step
        self.addchild(Schedule(parent=self))  # loop body

    def load(self, kern):
        ''' Load the state of this Loop using the supplied Kernel
        object. This method is provided so that we can individually
        construct Loop objects for a given kernel call. '''
        self._field = kern.arguments.iteration_space_arg()
        self._field_name = self._field.name
        self._field_space = self._field.function_space

    def gen_code(self, parent):
        ''' Work out the appropriate loop bounds and then call the base
            class to generate the code '''
        self.start_expr = Literal("1", parent=self)
        if self._loop_type == "colours":
            self.stop_expr = Reference("ncolour", parent=self)
        elif self._loop_type == "colour":
            self.stop_expr = ArrayReference("ncp_ncolour", parent=self)
            self.stop_expr.addchild(Reference("colour"), parent=self.stop_expr)
        else:
            self.stop_expr = Reference(self.field_name+"%get_ncell()",
                                       parent=self)
        Loop.gen_code(self, parent)


class DynBuiltInCallFactory(object):
    ''' A Dynamo 0.1 specific Built-In call factory. No built-in
        calls are supported in Dynamo 0.1 so we do nothing. '''
    @staticmethod
    def create(call, parent=None):
        ''' Creates a specific built-in call. Currently does
        nothing '''
        return None


class DynKernCallFactory(object):
    ''' A Dynamo 0.1 specific kernel call factory. '''
    @staticmethod
    def create(call, parent=None):
        # Loop over cells
        cloop = DynLoop(parent=parent)

        # The kernel itself
        kern = DynKern()
        kern.load(call, cloop)

        # Add the kernel as a child of the loop
        cloop.loop_body.addchild(kern)
        kern.parent = cloop.children[3]

        # Set-up the loop now we have the kernel object
        cloop.load(kern)

        # Return the outermost loop
        return cloop


class DynKern(CodedKern):
    ''' Stores information about Dynamo Kernels as specified by the Kernel
        metadata. Uses this information to generate appropriate PSy layer
        code for the Kernel instance. '''
    def __init__(self):
        if False:
            self._arguments = DynKernelArguments(None, None)  # for pyreverse

    def load(self, call, parent=None):
        '''
        Load this DynKern object with state pulled from the call object.

        :param call: details of the Algorithm-layer call of this Kernel.
        :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
        :param parent: parent of this kernel node in the PSyIR.
        :type parent: :py:class:`psyclone.dynamo0p1.DynLoop` or NoneType.

        '''
        super(DynKern, self).__init__(DynKernelArguments, call, parent)

    def local_vars(self):
        return ["cell", "map"]

    def gen_code(self, parent):
        ''' Generates dynamo version 0.1 specific psy code for a call to
            the dynamo kernel instance. '''
        from psyclone.f2pygen import CallGen, DeclGen, AssignGen, UseGen

        # TODO: we simply choose the first field as the lookup for the moment
        field_name = self.arguments.args[0].name

        # add a dofmap lookup using first field.
        # TODO: This needs to be generalised to work for multiple dofmaps
        parent.add(CallGen(parent, field_name+"%vspace%get_cell_dofmap",
                           ["cell", "map"]))
        parent.add(DeclGen(parent, datatype="integer",
                           entity_decls=["cell"]))
        parent.add(DeclGen(parent, datatype="integer", pointer=True,
                           entity_decls=["map(:)"]))

        # create the argument list on the fly so we can also create
        # appropriate variables and lookups
        arglist = []
        arglist.append("nlayers")
        arglist.append("ndf")
        arglist.append("map")

        found_gauss_quad = False
        gauss_quad_arg = None
        for arg in self._arguments.args:
            if arg.requires_basis:
                basis_name = arg.function_space+"_basis_"+arg.name
                arglist.append(basis_name)
                new_parent, position = parent.start_parent_loop()
                new_parent.add(CallGen(new_parent,
                                       field_name+"%vspace%get_basis",
                                       [basis_name]),
                               position=["before",
                                         position])
                parent.add(DeclGen(parent, datatype="real", kind="dp",
                                   pointer=True,
                                   entity_decls=[basis_name+"(:,:,:,:,:)"]))
            if arg.requires_diff_basis:
                raise GenerationError("differential basis has not yet "
                                      "been coded")
            if arg.requires_gauss_quad:
                if found_gauss_quad:
                    raise GenerationError("found more than one gaussian "
                                          "quadrature in this kernel")
                found_gauss_quad = True
                gauss_quad_arg = arg
            dataref = "%data"
            arglist.append(arg.name+dataref)

        if found_gauss_quad:
            gq_name = "gaussian_quadrature"
            arglist.append(gauss_quad_arg.name+"%"+gq_name)

        # generate the kernel call and associated use statement
        parent.add(CallGen(parent, self._name, arglist))
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name,
                              only=True, funcnames=[self._name]))

        # declare and initialise the number of layers and the number
        # of degrees of freedom. Needs to be generalised.
        parent.add(DeclGen(parent, datatype="integer",
                           entity_decls=["nlayers", "ndf"]))
        new_parent, position = parent.start_parent_loop()
        new_parent.add(AssignGen(new_parent, lhs="nlayers",
                                 rhs=field_name+"%get_nlayers()"),
                       position=["before", position])
        new_parent.add(AssignGen(new_parent, lhs="ndf",
                                 rhs=field_name+"%vspace%get_ndf()"),
                       position=["before", position])


class DynKernelArguments(Arguments):
    ''' Provides information about Dynamo kernel call arguments collectively,
        as specified by the kernel argument metadata. This class currently
        adds no additional functionality to its base class other than
        ensuring that initialisation is performed correctly. '''
    def __init__(self, call, parent_call):
        if False:
            self._0_to_n = DynKernelArgument(None, None, None)  # for pyreverse
        Arguments.__init__(self, parent_call)
        self._args = []
        for (idx, arg) in enumerate(call.ktype.arg_descriptors):
            self._args.append(DynKernelArgument(arg, call.args[idx],
                                                parent_call))
        self._dofs = []

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this makes no
            sense for dynamo. Need to refactor the Invoke class and remove the
            need for this dofs property (#279). '''
        return self._dofs


class DynKernelArgument(Argument):
    ''' Provides information about individual Dynamo kernel call arguments
        as specified by the kernel argument metadata. '''
    def __init__(self, arg, arg_info, call):
        self._arg = arg
        api_config = Config.get().api_conf("dynamo0.1")
        access_mapping = api_config.get_access_mapping()
        Argument.__init__(self, call, arg_info, access_mapping[arg.access])

    @property
    def function_space(self):
        ''' Returns the expected finite element function space for this
            argument as specified by the kernel argument metadata.'''
        return self._arg.function_space

    @property
    def requires_basis(self):
        ''' Returns true if the metadata for this argument specifies that
            its basis function values should be passed into the routine. '''
        if self._arg.basis.lower() == ".true.":
            return True
        if self._arg.basis.lower() == ".false.":
            return False
        raise GenerationError("error: basis is not set to .true. or .false.")

    @property
    def requires_diff_basis(self):
        ''' Returns true if the metadata for this argument specifies that
            its differential basis function values should be passed into
            the routine. '''
        if self._arg.diff_basis.lower() == ".true.":
            return True
        if self._arg.diff_basis.lower() == ".false.":
            return False
        raise GenerationError("error: diff_basis is not set to .true. "
                              "or .false.")

    @property
    def requires_gauss_quad(self):
        ''' Returns true if the metadata for this argument specifies that
            its gausian quadrature values should be passed into the
            routine. '''
        if self._arg.gauss_quad.lower() == ".true.":
            return True
        if self._arg.gauss_quad.lower() == ".false.":
            return False
        raise GenerationError("error: gaussian quadrature is not set to "
                              ".true. or .false.")
