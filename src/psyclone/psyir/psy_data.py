# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides support accessing. '''

from __future__ import absolute_import, print_function
from psyclone.f2pygen import CallGen, TypeDeclGen, UseGen
from psyclone.psyGen import Kern, NameSpace, \
     NameSpaceFactory, Node, BuiltIn


# =============================================================================
class PSyData(Node):
    # pylint: disable=too-many-instance-attributes, too-many-locals
    '''
    This class can be inserted into a schedule to instrument a set of nodes.
    Instrument means that calls to an external library will be inserted
    before and after the child nodes, which will give this library access
    to fields and the fact that a region is executed. This can be used as
    example to add performance profiling calls, in-situ visualisation
    of data, or for writing fields to a file (e.g. for creating test
    cases, or using driver to run a certain kernel only)

    :param children: a list of child nodes for this node. These will be made \
                     children of the child Schedule of this Profile Node.
    :type children: list of :py::class::`psyclone.psyGen.Node` \
                    or derived classes
    :param parent: the parent of this node in the PSyIR.
    :type parent: :py::class::`psyclone.psyGen.Node`

    '''
    # PSyData interface Fortran module
    fortran_module = "psy_data_mod"
    # The symbols we import from the profiling Fortran module
    symbols = ["PSyDataType"]

    # Root of the name to use for variables associated with profiling regions
    psy_data_var = "psy_data"

    # A namespace manager to make sure we get unique region names
    _namespace = NameSpace()

    def __init__(self, children=None, parent=None, options=None):

        # Store the name of the profile variable that is used for this
        # profile name. This allows to show the variable name in __str__
        # (and also if we would call create_name in gen(), the name would
        # change every time gen() is called).
        self._var_name = NameSpaceFactory().create().create_name("psy_data")

        # Name of the region. In general at constructor time we might not
        # have a parent subroutine or a child for the kernel, so we leave
        # the name empty for now. The region and module names are set the
        # first time gen() is called (and then remain unchanged).
        self._region_name = None
        self._module_name = None

        # Name and colour to use for this node
        self._text_name = "PSyData"
        self._colour_key = "PSyData"

        if options:
            # Create a copy
            self._options = dict(options)
        else:
            self._options = {}

        self._pre_variable_list = self._options.get("pre-var-list", [])
        self._post_variable_list = self._options.get("post-var-list", [])

        if children:
            node_parent = children[0].parent
            node_position = children[0].position

            # A PSyData node always contains a Schedule
            sched = self._insert_schedule(children)
            Node.__init__(self, children=[sched], parent=parent)

            # Correct the parent's list of children. Use a slice of the list
            # of nodes so that we're looping over a local copy of the list.
            # Otherwise things get confused when we remove children from
            # the list.
            for child in children[:]:
                # Remove child from the parent's list of children
                node_parent.children.remove(child)

            # Add this node as a child of the parent
            # of the nodes being enclosed and at the original location
            # of the first of these nodes
            node_parent.addchild(self, index=node_position)

    # -------------------------------------------------------------------------
    def __str__(self):
        ''' Returns a string representation of the subtree starting at
        this node. '''
        result = "PSyDataStart[var={0}]\n".format(self._var_name)
        for child in self.psy_data_body.children:
            result += str(child)+"\n"
        return result+"PSyDataEnd[var={0}]".format(self._var_name)

    # -------------------------------------------------------------------------
    @property
    def psy_data_body(self):
        '''
        :returns: the Schedule associated with this PSyData region.
        :rtype: :py:class:`psyclone.psyGen.Schedule`

        :raises InternalError: if this PSyData node does not have a Schedule \
                               as its one and only child.
        '''
        from psyclone.psyGen import Schedule, InternalError
        if len(self.children) != 1 or not \
           isinstance(self.children[0], Schedule):
            raise InternalError(
                "PSyData node malformed or incomplete. It should have a "
                "single Schedule as a child but found: {0}"
                .format([type(child).__name__ for child in self.children]))
        return self.children[0]

    # -------------------------------------------------------------------------
    def _add_call(self, name, parent, arguments):
        '''This function adds a call to the specified method of
        self._var_name to the parent.

        :param str name: name of the method to call.
        :param parent: parent node into which to insert the calls.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param arguments: arguments for the method call.
        :type arguments: list of str
        '''
        call = CallGen(parent,
                       "{0}%{1}".format(self._var_name, name),
                       arguments)
        parent.add(call)

    # -------------------------------------------------------------------------
    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''Creates the profile start and end calls, surrounding the children
        of this node.

        :param parent: the parent of this node.
        :type parent: :py:class:`psyclone.psyGen.Node`

        '''
        if self._module_name is None or self._region_name is None:
            # Find the first kernel and use its name. In an untransformed
            # Schedule there should be only one kernel, but if Profile is
            # invoked after e.g. a loop merge more kernels might be there.
            region_name = "unknown-kernel"
            module_name = "unknown-module"
            for kernel in self.walk(Kern):
                region_name = kernel.name
                if not isinstance(kernel, BuiltIn):
                    # If the kernel is not a builtin then it has a module name.
                    module_name = kernel.module_name
                break
            if self._region_name is None:
                self._region_name = PSyData._namespace.create_name(region_name)
            if self._module_name is None:
                self._module_name = module_name

        # Note that adding a use statement makes sure it is only
        # added once, so we don't need to test this here!
        use = UseGen(parent, self.fortran_module, only=True,
                     funcnames=PSyData.symbols)
        parent.add(use)
        var_decl = TypeDeclGen(parent, datatype="PSyDataType",
                               entity_decls=[self._var_name],
                               save=True)
        parent.add(var_decl)

        # Now determine the variables to write:
        from psyclone.psyir.tools.dependency_tools import DependencyTools

        dep = DependencyTools()
        input_list, output_list = dep.get_in_out_parameters(self)
        self._add_call("PreStart", parent,
                       ["\"{0}\"".format(self._module_name),
                        "\"{0}\"".format(self._region_name),
                        len(input_list), len(output_list)])

        for var_name in input_list+output_list:
            self._add_call("PreDeclareVariable", parent,
                           ["\"{0}\"".format(var_name),
                            "{0}".format(var_name)])

        self._add_call("PreEndDeclaration", parent,
                       ["\"{0}\"".format(self._module_name),
                        "\"{0}\"".format(self._region_name)])

        for var_name in input_list:
            self._add_call("WriteVariable", parent,
                           ["\"{0}\"".format(var_name),
                            "{0}".format(var_name)])

        self._add_call("PreEnd", parent,
                       ["\"{0}\"".format(self._module_name),
                        "\"{0}\"".format(self._region_name)])

        for child in self.psy_data_body:
            child.gen_code(parent)

        self._add_call("PostStart", parent,
                       ["\"{0}\"".format(self._module_name),
                        "\"{0}\"".format(self._region_name)])
        for var_name in output_list:
            self._add_call("WriteVariable", parent,
                           ["\"{0}\"".format(var_name),
                            "{0}".format(var_name)])

        self._add_call("PostEnd", parent,
                       ["\"{0}\"".format(self._module_name),
                        "\"{0}\"".format(self._region_name)])

    # -------------------------------------------------------------------------
    def gen_c_code(self, indent=0):
        '''
        Generates a string representation of this Node using C language
        (currently not supported).

        :param int indent: Depth of indent for the output string.
        :raises NotImplementedError: Not yet supported for profiling.
        '''
        raise NotImplementedError("Generation of C code is not supported "
                                  "for PSyData.")

    # -------------------------------------------------------------------------
    def update(self):
        '''
        Not yet supported for PSyData.
        '''
        raise NotImplementedError("Generation of code using PSyIR is not "
                                  "supported for PSyData.")
