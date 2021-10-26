# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes import Schedule

class BlockedLoop(Loop):
    '''
    Node representing a blocked loop

    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param variable: optional reference to the loop iterator \
        variable. Defaults to None.
    :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol` or \
        `NoneType`
    :param valid_loop_types: a list of loop types that are specific \
        to a particular API.
    :type valid_loop_types: list of str
    :param annotations: One or more labels that provide additional information\
          about the node (primarily relating to the input code that it was \
          created from).
    :type annotations: list of str

    :raises InternalError: if the 'was_single_stmt' annotation is supplied \
                           without the 'was_where' annotation.
    '''
    def __init__(self, parent=None, variable=None, valid_loop_types=None,
                 annotations=None):
        super(BlockedLoop, self).__init__(parent=parent,
                                          variable=variable,
                                          valid_loop_types=valid_loop_types,
                                          annotations=annotations)
        if "blocked" not in self._valid_loop_types:
            self._valid_loop_types.append("blocked")
        self._loop_type = "blocked"

    @staticmethod
    def create(variable, start, stop, step, children):
        '''Create a BlockedLoop instance given valid instances of a variable,
        start, stop and step nodes, and a list of child nodes for the
        loop body.

        :param variable: the PSyIR node containing the variable \
            of the loop iterator.
        :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param start: the PSyIR node determining the value for the \
            start of the loop.
        :type start: :py:class:`psyclone.psyir.nodes.Node`
        :param end: the PSyIR node determining the value for the end \
            of the loop.
        :type end: :py:class:`psyclone.psyir.nodes.Node`
        :param step: the PSyIR node determining the value for the loop \
            step.
        :type step: :py:class:`psyclone.psyir.nodes.Node`
        :param children: a list of PSyIR nodes contained in the \
            loop.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a Loop instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        Loop._check_variable(variable)

        if not isinstance(children, list):
            raise GenerationError(
                "children argument in create method of Loop class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))

        loop = BlockedLoop(variable=variable)
        schedule = Schedule(parent=loop, children=children)
        loop.children = [start, stop, step, schedule]
        return loop
