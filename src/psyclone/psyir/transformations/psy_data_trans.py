# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: A. R. Porter, STFC Daresbury Laboratory

'''Contains the PSyData transformation.
'''

from psyclone.configuration import Config
from psyclone.psyir.nodes import Node, PSyDataNode, Schedule
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.undoredo import Memento


class PSyDataTrans(RegionTrans):
    ''' Create a PSyData region around a list of statements. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyir.transformations import PSyDataTrans
    >>> data_trans = PSyDataTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> # Enclose all children within a single PSyData region
    >>> newschedule, _ = data_trans.apply(schedule.children)
    >>> newschedule.view()
    >>> # Or to use a class-prefix string and different region name:
    >>> newschedule, _ = data_trans.apply(schedule.children,
    >>>                                   {"prefix": "my_prefix",
    >>>                                    "region_name": ("module","region")})

    :param node_class: The Node class of which an instance will be inserted \
        into the tree (defaults to PSyDataNode).
    :type node_class: :py:class:`psyclone.psyir.nodes.ExtractNode`

    '''
    # Unlike other transformations we can be fairly relaxed about the nodes
    # that a region can contain as we don't have to understand them.
    # TODO: #415 Support different classes of PSyData calls.
    valid_node_types = (Node,)

    def __init__(self, node_class=PSyDataNode):
        super(PSyDataTrans, self).__init__()
        self._node_class = node_class

    def __str__(self):
        return "Insert a PSyData node."

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "PSyDataTrans"

    def validate(self, node_list, options=None):
        '''
        Calls the validate method of the base class and then checks that,
        for the NEMO API, the routine that will contain the instrumented
        region already has a Specification_Part (because we've not yet
        implemented the necessary support if it doesn't).
        TODO: #435

        :param node_list: a list of nodes to be instrumented with \
            PSyData API calls.
        :type node_list: :py:class:`psyclone.psyir.nodes.Loop`

        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param str options["prefix"]: a prefix to use for the PSyData module \
            name (``PREFIX_psy_data_mod``) and the PSyDataType \
            (``PREFIX_PSYDATATYPE``) - a "_" will be added automatically. \
            It defaults to "".
        :param (str,str) options["region_name"]: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region unless aggregate information \
            is required (and is supported by the runtime library).

        :raises TransformationError: if we're using the NEMO API and the \
            target routine has no Specification_Part.
        :raises TransformationError: if the PSyData node is inserted \
            between an OpenMP/ACC directive and the loop(s) to which it \
            applies.
        '''
        from fparser.two import Fortran2003
        from fparser.two.utils import walk
        from psyclone.nemo import NemoInvoke
        from psyclone.psyGen import OMPDoDirective, ACCLoopDirective

        node_parent = node_list[0].parent
        if isinstance(node_parent, Schedule) and \
           isinstance(node_parent.parent, (OMPDoDirective, ACCLoopDirective)):
            raise TransformationError("A PSyData node cannot be inserted "
                                      "between an OpenMP/ACC directive and "
                                      "the loop(s) to which it applies!")

        if options:
            if "region_name" in options:
                name = options["region_name"]
                # pylint: disable=too-many-boolean-expressions
                if not isinstance(name, tuple) or not len(name) == 2 or \
                   not name[0] or not isinstance(name[0], str) or \
                   not name[1] or not isinstance(name[1], str):
                    raise TransformationError(
                        "Error in {0}. User-supplied region name must be a "
                        "tuple containing two non-empty strings."
                        "".format(self.name))
                # pylint: enable=too-many-boolean-expressions
            if "prefix" in options:
                prefix = options["prefix"]
                if prefix not in Config.get().valid_psy_data_prefixes:
                    raise TransformationError(
                        "Error in 'prefix' parameter: found '{0}', expected "
                        "one of {1} as defined in {2}"
                        .format(prefix, Config.get().valid_psy_data_prefixes,
                                Config.get().filename))

        super(PSyDataTrans, self).validate(node_list, options)

        # The checks below are only for the NEMO API and can be removed
        # once #435 is done.
        sched = node_list[0].ancestor(InvokeSchedule)
        invoke = sched.invoke
        if not isinstance(invoke, NemoInvoke):
            return

        # Get the parse tree of the routine containing this region
        # pylint: disable=protected-access
        ptree = invoke._ast
        # pylint: enable=protected-access
        # Search for the Specification_Part
        if not walk([ptree], Fortran2003.Specification_Part):
            raise TransformationError(
                "For the NEMO API, PSyData can only be added to routines "
                "which contain existing variable declarations (i.e. a "
                "Specification Part) but '{0}' does not have any.".format(
                    invoke.name))

    def apply(self, nodes, options=None):
        # pylint: disable=arguments-differ
        '''Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Nodes in the
        schedule within a single PSyData region.

        :param nodes: can be a single node or a list of nodes.
        :type nodes: :py:obj:`psyclone.psyir.nodes.Node` or list of \
                     :py:obj:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param str options["prefix"]: a prefix to use for the PSyData module \
            name (``PREFIX_psy_data_mod``) and the PSyDataType \
            (``PREFIX_PSYDATATYPE``) - a "_" will be added automatically. \
            It defaults to "".
        :param (str,str) options["region_name"]: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region unless aggregate information \
            is required (and is supported by the runtime library).

        :returns: Tuple of the modified schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`)

        '''
        node_list = self.get_node_list(nodes)

        # Perform validation checks
        self.validate(node_list, options)

        # create a memento of the schedule and the proposed
        # transformation
        schedule = node_list[0].root
        keep = Memento(schedule, self)

        # Create an instance of the required class that implements
        # the code extraction using the PSyData API, e.g. a
        # GOceanExtractNode. The base constructor of the extraction node
        # will insert the node into the PSyIR between the
        # nodes to be extracted and their parent. The nodes to
        # be extracted will become children of the extraction node.
        # We also pass the user-specified options to the constructor,
        # so that the behaviour of the code extraction can be controlled.
        # An example use case of this is the 'create_driver' flag, where
        # the calling program can control if a stand-alone driver program
        # should be created or not.
        self._node_class(parent=node_list[0].parent, children=node_list[:],
                         options=options)

        return schedule, keep
