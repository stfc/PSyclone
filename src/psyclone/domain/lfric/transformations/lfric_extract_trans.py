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
# Authors I. Kavcic, Met Office

'''This module contains the LFRic-specific implementation of the ExtractTrans
transformation.
'''

from psyclone.psyir.transformations import ExtractTrans, TransformationError


class LFRicExtractTrans(ExtractTrans):
    ''' Dynamo0.3 API application of ExtractTrans transformation \
    to extract code into a stand-alone program. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>>
    >>> API = "dynamo0.3"
    >>> FILENAME = "solver_alg.x90"
    >>> ast, invokeInfo = parse(FILENAME, api=API)
    >>> psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>>
    >>> from psyclone.domain.lfric.transformations import LFRicExtractTrans
    >>> etrans = LFRicExtractTrans()
    >>>
    >>> # Apply LFRicExtractTrans transformation to selected Nodes
    >>> newsched, _ = etrans.apply(schedule.children[0:3])
    >>> newsched.view()
    '''

    def __str__(self):
        return "Inserts an ExtractNode in an LFRic Schedule."

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "LFRicExtractTrans"

    def validate(self, node_list, options=None):
        ''' Perform Dynamo0.3 API specific validation checks before applying
        the transformation.

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if transformation is applied to a Loop \
                                     over cells in a colour without its \
                                     parent Loop over colours.
        '''

        # First check constraints on Nodes in the node_list inherited from
        # the parent classes (ExtractTrans and RegionTrans)
        super(LFRicExtractTrans, self).validate(node_list, options)

        # Check LFRicExtractTrans specific constraints
        from psyclone.dynamo0p3 import DynLoop
        for node in node_list:

            # Check that ExtractNode is not inserted between a Loop
            # over colours and a Loop over cells in a colour when
            # colouring is applied.
            ancestor = node.ancestor(DynLoop)
            if ancestor and ancestor.loop_type == 'colours':
                raise TransformationError(
                    "Error in {0} for Dynamo0.3 API: Extraction of a Loop "
                    "over cells in a colour without its ancestor Loop over "
                    "colours is not allowed.".format(str(self.name)))
