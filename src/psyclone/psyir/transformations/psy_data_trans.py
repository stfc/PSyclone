# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
# Modified: A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab

'''Contains the PSyData transformation.
'''

from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.psyGen import InvokeSchedule, Kern
from psyclone.psyir.nodes import PSyDataNode, Schedule, Return, \
    OMPDoDirective, ACCDirective, ACCLoopDirective, Routine
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class PSyDataTrans(RegionTrans):
    ''' Create a PSyData region around a list of statements. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> ast, invoke_info = parse(SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invoke_info)
    >>>
    >>> from psyclone.psyir.transformations import PSyDataTrans
    >>> data_trans = PSyDataTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Enclose all children within a single PSyData region
    >>> data_trans.apply(schedule.children)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>> # Or to use custom region name:
    >>> data_trans.apply(schedule.children,
    ...                  {"region_name": ("module","region")})

    :param node_class: The Node class of which an instance will be inserted \
        into the tree (defaults to PSyDataNode).
    :type node_class: :py:class:`psyclone.psyir.nodes.ExtractNode`

    '''
    # Unlike other transformations we can be fairly relaxed about the nodes
    # that a region can contain as we don't have to understand them.
    excluded_node_types = (Return,)

    # This dictionary keeps track of region+module names that are already
    # used. For each key (which is module_name+"|"+region_name) it contains
    # how many regions with that name have been created. This number will
    # then be added as an index to create unique region identifiers.
    _used_kernel_names = {}

    def __init__(self, node_class=PSyDataNode):
        super(PSyDataTrans, self).__init__()
        self._node_class = node_class

    # ------------------------------------------------------------------------
    def __str__(self):
        return (f"Create a sub-tree of the PSyIR that has a node of type "
                f"{self._node_class.__name__} at its root.")

    # ------------------------------------------------------------------------
    @property
    def name(self):
        '''This function returns the name of the transformation.
        It uses the Python 2/3 compatible way of returning the
        class name as a string, which means that the same function can
        be used for all derived classes.

        :returns: the name of this transformation as a string.
        :rtype: str
        '''

        return self.__class__.__name__

    # ------------------------------------------------------------------------
    def get_unique_region_name(self, nodes, options):
        '''This function returns the region and module name. If they are
        specified in the user options, these names will just be returned (it
        is then up to the user to guarantee uniqueness). Otherwise a name
        based on the module and invoke will be created using indices to
        make sure the name is unique.

        :param nodes: a list of nodes.
        :type nodes: list of :py:obj:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param (str,str) options["region_name"]: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region unless aggregate information \
            is required (and is supported by the runtime library).

        '''
        # We don't use a static method here since it might be useful to
        # overwrite this functions in derived classes
        # pylint: disable=no-self-use
        name = options.get("region_name", None)
        if name:
            # pylint: disable=too-many-boolean-expressions
            if not isinstance(name, tuple) or not len(name) == 2 or \
               not name[0] or not isinstance(name[0], str) or \
               not name[1] or not isinstance(name[1], str):
                raise InternalError(
                    "Error in PSyDataTrans. The name must be a "
                    "tuple containing two non-empty strings.")
            # pylint: enable=too-many-boolean-expressions
            # Valid PSyData names have been provided by the user.
            return name

        invoke = nodes[0].ancestor(InvokeSchedule).invoke
        module_name = invoke.invokes.psy.name

        # Use the invoke name as a starting point.
        region_name = invoke.name
        kerns = []
        for node in nodes:
            kerns.extend(node.walk(Kern))

        if len(kerns) == 1:
            # This PSyData region only has one kernel within it,
            # so append the kernel name.
            region_name += f":{kerns[0].name}"

        # Add a region index to ensure uniqueness when there are
        # multiple regions in an invoke.
        key = module_name + "|" + region_name
        idx = PSyDataTrans._used_kernel_names.get(key, 0)
        PSyDataTrans._used_kernel_names[key] = idx + 1
        region_name += f":r{idx}"
        return (module_name, region_name)

    # ------------------------------------------------------------------------
    def validate(self, nodes, options=None):
        '''
        Checks that the supplied list of nodes is valid, that the location
        for this node is valid (not between a loop-directive and its loop),
        that there aren't any name clashes with symbols that must be
        imported from the appropriate PSyData library and finally, calls the
        validate method of the base class.

        :param nodes: a node or list of nodes to be instrumented with \
            PSyData API calls.
        :type nodes: (list of) :py:class:`psyclone.psyir.nodes.Loop`

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

        :raises TransformationError: if the supplied list of nodes is empty.
        :raises TransformationError: if the PSyData node is inserted \
            between an OpenMP/ACC directive and the loop(s) to which it \
            applies.
        :raises TransformationError: if the 'prefix' or 'region_name' options \
            are not valid.
        :raises TransformationError: if there will be a name clash between \
            any existing symbols and those that must be imported from the \
            appropriate PSyData library.

        '''
        # pylint: disable=too-many-branches
        node_list = self.get_node_list(nodes)

        if not node_list:
            raise TransformationError("Cannot apply transformation to an "
                                      "empty list of nodes.")

        node_parent = node_list[0].parent
        if isinstance(node_parent, Schedule) and \
           isinstance(node_parent.parent, (OMPDoDirective, ACCLoopDirective)):
            raise TransformationError("A PSyData node cannot be inserted "
                                      "between an OpenMP/ACC directive and "
                                      "the loop(s) to which it applies!")

        if node_list[0].ancestor(ACCDirective):
            raise TransformationError("A PSyData node cannot be inserted "
                                      "inside an OpenACC region.")

        if options:
            if "region_name" in options:
                name = options["region_name"]
                # pylint: disable=too-many-boolean-expressions
                if not isinstance(name, tuple) or not len(name) == 2 or \
                   not name[0] or not isinstance(name[0], str) or \
                   not name[1] or not isinstance(name[1], str):
                    raise TransformationError(
                        f"Error in {self.name}. User-supplied region name "
                        f"must be a tuple containing two non-empty strings.")
                # pylint: enable=too-many-boolean-expressions
            if "prefix" in options:
                prefix = options["prefix"]
                if prefix not in Config.get().valid_psy_data_prefixes:
                    raise TransformationError(
                        f"Error in 'prefix' parameter: found '{prefix}', while"
                        f" one of {Config.get().valid_psy_data_prefixes} was "
                        f"expected as defined in {Config.get().filename}")

        # We have to create an instance of the node that will be inserted in
        # order to find out what module name it will use.
        pdata_node = self._node_class(options=options)
        table = node_list[0].scope.symbol_table
        for name in ([sym.name for sym in pdata_node.imported_symbols] +
                     [pdata_node.fortran_module]):
            try:
                _ = table.lookup_with_tag(name)
            except KeyError as err:
                # The tag doesn't exist which means that we haven't already
                # added this symbol as part of a PSyData transformation. Check
                # for any clashes with existing symbols.
                try:
                    _ = table.lookup(name)
                    raise TransformationError(
                        f"Cannot add PSyData calls because there is already a "
                        f"symbol named '{name}' which clashes with one of "
                        f"those used by the PSyclone PSyData API. ") from err
                except KeyError:
                    pass

        super(PSyDataTrans, self).validate(node_list, options)

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

        '''
        node_list = self.get_node_list(nodes)

        # Perform validation checks
        self.validate(node_list, options)

        # Get useful references
        parent = node_list[0].parent
        position = node_list[0].position
        root = node_list[0].root

        # We always use the Routine symbol table
        table = node_list[0].ancestor(Routine).symbol_table

        # Create an instance of the required class that implements
        # the code extraction using the PSyData API, e.g. a
        # ExtractNode. We pass the user-specified options to the
        # create() method.  An example use case for this is the
        # 'create_driver' flag, where the calling program can control if
        # a stand-alone driver program should be created or not (when
        # performing kernel extraction).
        for node in node_list:
            node.detach()
        psy_data_node = self._node_class.create(
            node_list, symbol_table=table, options=options)
        parent.addchild(psy_data_node, position)


# =============================================================================
# For AutoAPI documentation generation
__all__ = ['PSyDataTrans']
