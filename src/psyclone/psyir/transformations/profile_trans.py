# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2020, Science and Technology Facilities Council.
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
# Modified by A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module provides the Profile transformation.
'''

from psyclone.psyir.nodes import Schedule
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.undoredo import Memento


class ProfileTrans(RegionTrans):
    ''' Create a profile region around a list of statements. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory, GenerationError
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> p_trans = t.get_trans_name('ProfileTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> # Enclose all children within a single profile region
    >>> newschedule, _ = p_trans.apply(schedule.children)
    >>> newschedule.view()

    '''
    from psyclone import psyGen, profiler, psyir
    # Unlike other transformations we can be fairly relaxed about the nodes
    # that a region can contain as we don't have to understand them.
    valid_node_types = (psyir.nodes.Node,)

    def __str__(self):
        return "Insert a profile start and end call."

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "ProfileTrans"

    def validate(self, nodes, options=None):
        # pylint: disable=arguments-differ
        '''
        Calls the validate method of the base class and then checks that,
        for the NEMO API, the routine that will contain the profiling
        region already has a Specification_Part (because we've not yet
        implemented the necessary support if it doesn't).

        :param nodes: a list of nodes to be profiled.
        :type nodes: :py:class:`psyclone.psyir.nodes.Loop`

        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if we're using the NEMO API and the \
                                     target routine has no Specification_Part.
        '''
        from fparser.two import Fortran2003
        from fparser.two.utils import walk
        from psyclone.nemo import NemoInvoke

        super(ProfileTrans, self).validate(nodes, options)

        # pylint: disable=too-many-boolean-expressions
        if options:
            try:
                name = options["profile_name"]
                if not isinstance(name, tuple) or not len(name) == 2 or \
                   not name[0] or not isinstance(name[0], str) or \
                   not name[1] or not isinstance(name[1], str):
                    raise TransformationError(
                        "Error in {0}. User-supplied profile name must be a "
                        "tuple containing two non-empty strings."
                        "".format(str(self)))
            except KeyError:
                # profile name is not supplied
                pass
        # pylint: enable=too-many-boolean-expressions

        # The checks below are only for the NEMO API and can be removed
        # once #435 is done.
        invoke = nodes[0].root.invoke
        if not isinstance(invoke, NemoInvoke):
            return
        # Get the parse tree of the routine containing this region
        # pylint: disable=protected-access
        ptree = invoke._ast
        # pylint: enable=protected-access
        # Search for the Specification_Part
        if not walk(ptree, Fortran2003.Specification_Part):
            raise TransformationError(
                "For the NEMO API, profiling can only be added to routines "
                "which contain existing variable declarations (i.e. a "
                "Specification Part) but '{0}' does not have any.".format(
                    invoke.name))

    def apply(self, nodes, options=None):
        # pylint: disable=arguments-differ, too-many-locals
        '''Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Nodes in the
        schedule within a single profiler region.

        :param nodes: can be a single node or a list of nodes.
        :type nodes: :py:obj:`psyclone.psygen.Node` or list of\
                     :py:obj:`psyclone.psygen.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param (str, str) options["profile_name"]: an optional name to \
            use for this profile, provided as a 2-tuple containing a \
            location name followed by a local name.

        :returns: Tuple of the modified schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`)

        '''
        # Check whether we've been passed a list of nodes or just a
        # single node.
        from psyclone.psyGen import OMPDoDirective, ACCLoopDirective
        from psyclone.psyir.nodes import Node
        if isinstance(nodes, list) and isinstance(nodes[0], Node):
            node_list = nodes
        elif isinstance(nodes, Schedule):
            # We've been passed a Schedule so default to enclosing its
            # children.
            node_list = nodes.children
        elif isinstance(nodes, Node):
            # Single node that's not a Schedule
            node_list = [nodes]
        else:
            arg_type = str(type(nodes))
            raise TransformationError("Error in {1}. "
                                      "Argument must be a single Node in a "
                                      "schedule or a list of Nodes in a "
                                      "schedule but have been passed an "
                                      "object of type: {0}".
                                      format(arg_type, str(self)))

        # Keep a reference to the parent of the nodes that are to be
        # enclosed within a profile region. Also keep the index of
        # the first child to be enclosed as that will become the
        # position of the new Profile node
        node_parent = node_list[0].parent
        if isinstance(node_parent, Schedule) and \
           isinstance(node_parent.parent, (OMPDoDirective, ACCLoopDirective)):
            raise TransformationError("A ProfileNode cannot be inserted "
                                      "between an OpenMP/ACC directive and "
                                      "the loop(s) to which it applies!")

        # Perform validation checks
        self.validate(node_list, options)

        name = None
        if options:
            try:
                name = options["profile_name"]
            except KeyError:
                pass

        # create a memento of the schedule and the proposed
        # transformation
        schedule = node_list[0].root

        keep = Memento(schedule, self)

        # Create the ProfileNode. All of the supplied child nodes will have
        # the Profile's Schedule as their parent.

        from psyclone.psyir.nodes import ProfileNode
        ProfileNode(parent=node_parent, children=node_list[:], name=name)

        return schedule, keep
