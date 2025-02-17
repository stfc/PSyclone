# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office

''' This module provides the ACCKernelsTrans transformation. '''

import re
from typing import List, Union

from psyclone import psyGen
from psyclone.psyir.nodes import (
    ACCKernelsDirective, Assignment, Call, CodeBlock, Loop,
    Node, PSyDataNode, Reference, Return, Routine, Statement, WhileLoop)
from psyclone.psyir.symbols import UnsupportedFortranType
from psyclone.psyir.transformations.arrayassignment2loops_trans import (
    ArrayAssignment2LoopsTrans)
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class ACCKernelsTrans(RegionTrans):
    '''
    Enclose a sub-set of nodes from a Schedule within an OpenACC kernels
    region (i.e. within "!$acc kernels" ... "!$acc end kernels" directives).

    For example:

    >>> from psyclone.psyir.frontend import FortranReader
    >>> psyir = FortranReader().psyir_from_source(NEMO_SOURCE_FILE)
    >>>
    >>> from psyclone.psyir.transformations import ACCKernelsTrans
    >>> ktrans = ACCKernelsTrans()
    >>>
    >>> schedule = psyir.children[0]
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>> kernels = schedule.children[9]
    >>> # Transform the kernel
    >>> ktrans.apply(kernels)

    '''
    excluded_node_types = (CodeBlock, Return, PSyDataNode,
                           psyGen.HaloExchange, WhileLoop)

    def apply(self, node: Union[Node, List[Node]], options: dict = None):
        '''
        Enclose the supplied list of PSyIR nodes within an OpenACC
        Kernels region.

        :param node: the node(s) in the PSyIR to enclose.
        :type node: :py:class:`psyclone.psyir.nodes.Node` |
                    list[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["default_present"]: whether or not the kernels
            region should have the 'default present' attribute (indicating
            that data is already on the accelerator). When using managed
            memory this option should be False.
        :param bool options["allow_string"]: whether to allow the
            transformation on assignments involving character types. Defaults
            to False.
        :param bool options["verbose"]: log the reason the validation failed,
            at the moment with a comment in the provided PSyIR node.

        '''
        # Ensure we are always working with a list of nodes, even if only
        # one was supplied via the `node` argument.
        node_list = self.get_node_list(node)

        self.validate(node_list, options)

        parent = node_list[0].parent
        start_index = node_list[0].position

        if not options:
            options = {}
        default_present = options.get("default_present", False)

        # Create a directive containing the nodes in node_list and insert it.
        directive = ACCKernelsDirective(
            parent=parent, children=[node.detach() for node in node_list],
            default_present=default_present)

        parent.children.insert(start_index, directive)

    def validate(self, nodes: Union[Node, List[Node]],
                 options: dict = None) -> None:
        # pylint: disable=signature-differs
        '''
        Check that we can safely enclose the supplied node or list of nodes
        within OpenACC kernels ... end kernels directives.

        :param nodes: the proposed PSyIR node or nodes to enclose in the
                      kernels region.
        :type nodes: (list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["disable_loop_check"]: whether to disable the
            check that the supplied region contains 1 or more loops. Default
            is False (i.e. the check is enabled).
        :param bool options["allow_string"]: whether to allow the
            transformation on assignments involving character types. Defaults
            to False.
        :param bool options["verbose"]: log the reason the validation failed,
            at the moment with a comment in the provided PSyIR node.

        :raises NotImplementedError: if the supplied Nodes belong to
            a GOInvokeSchedule.
        :raises TransformationError: if there is an access to an assumed-size
            character variable within the region.
        :raises TransformationError: if the proposed region contains a call to
            a routine that is not available on the accelerator.
        :raises TransformationError: if there are no Loops within the
            proposed region and options["disable_loop_check"] is not True.
        :raises TransformationError: if any assignments in the region contain a
            character type child and options["allow_string"] is not True.

        '''
        if not options:
            options = {}

        # Ensure we are always working with a list of nodes, even if only
        # one was supplied via the `nodes` argument.
        node_list = self.get_node_list(nodes)

        # Check that the front-end is valid
        # pylint: disable-next=import-outside-toplevel
        from psyclone.gocean1p0 import GOInvokeSchedule
        if node_list[0].ancestor(GOInvokeSchedule):
            raise NotImplementedError(
                "OpenACC kernels regions are not currently supported for "
                "GOcean InvokeSchedules")
        super().validate(node_list, options)

        # The regex we use to determine whether a character declaration is
        # of assumed size ('LEN=*' or '*(*)').
        # TODO #2612 - improve the fparser2 frontend support for character
        # declarations.
        assumed_size = re.compile(r"\(\s*len\s*=\s*\*\s*\)|\*\s*\(\s*\*\s*\)")

        # Construct a list of any symbols that correspond to assumed-size
        # character strings. These can only be routine arguments.
        char_syms = []
        parent_routine = node_list[0].ancestor(Routine)
        if parent_routine:
            arg_syms = parent_routine.symbol_table.argument_datasymbols
            for sym in arg_syms:
                # Currently the fparser2 frontend does not support any type
                # of LEN= specification on a character variable so we resort
                # to a regex to check whether it is assumed-size.
                if isinstance(sym.datatype, UnsupportedFortranType):
                    type_txt = sym.datatype.type_text.lower()
                    if (type_txt.startswith("character") and
                            assumed_size.search(type_txt)):
                        char_syms.append(sym)

        for node in node_list:
            # Check that there are no assumed-size character variables as these
            # cause an Internal Compiler Error with (at least) NVHPC <= 24.5.
            for ref in node.walk(Reference):
                if ref.symbol in char_syms:
                    stmt = ref.ancestor(Statement)
                    raise TransformationError(
                        f"Assumed-size character variables cannot be enclosed "
                        f"in an OpenACC region but found "
                        f"'{stmt.debug_string()}'")
            # Check there are no character assignments in the region as these
            # cause various problems with (at least) NVHPC <= 24.5
            if not options.get("allow_string", False):
                message = (
                    f"{self.name} does not permit assignments involving "
                    f"character variables by default (use the 'allow_string' "
                    f"option to include them)")
                for assign in node.walk(Assignment):
                    ArrayAssignment2LoopsTrans.validate_no_char(
                        assign, message, options)

            # Check that any called routines are supported on the device.
            for icall in node.walk(Call):
                if not icall.is_available_on_device():
                    raise TransformationError(
                        f"Cannot include '{icall.debug_string()}' in an "
                        f"OpenACC region because it is not available on GPU.")

        # Check that we have at least one loop or array range within
        # the proposed region unless this has been disabled.
        if options and options.get("disable_loop_check", False):
            return

        for node in node_list:
            if (any(assign for assign in node.walk(Assignment)
                    if assign.is_array_assignment) or node.walk(Loop)):
                break
        else:
            # Branch executed if loop does not exit with a break
            raise TransformationError(
                "A kernels transformation must enclose at least one loop or "
                "array range but none were found.")
