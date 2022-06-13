# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Author A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by R. W. Ford and S. Siso, STFC Daresbury Lab

'''
Module providing a transformation from a generic PSyIR Loop into a
NEMO Loop.

'''
from psyclone.transformations import Transformation, TransformationError
from psyclone.psyir.nodes import Loop
from psyclone.nemo import NemoLoop


class CreateNemoLoopTrans(Transformation):
    """
    Transform a generic PSyIR Loop into a NemoLoop. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.domain.nemo.transformations import CreateNemoLoopTrans
    >>> code = '''
    ... subroutine sub()
    ...   integer :: ji, tmp(10)
    ...   do ji=1, 10
    ...     tmp(ji) = 2*ji
    ...   end do
    ... end subroutine sub'''
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> loops = psyir.walk(Loop)
    >>> trans = CreateNemoLoopTrans()
    >>> trans.apply(loops[0])
    >>> print(psyir.view(colour=False))
    FileContainer[]
        Routine[name:'sub']
            0: Loop[type='lon', field_space='None', it_space='None']
                Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
                Literal[value:'10', Scalar<INTEGER, UNDEFINED>]
                Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
                Schedule[]
                    0: Assignment[]
                        ArrayReference[name:'tmp']
                            Reference[name:'ji']
                        BinaryOperation[operator:'MUL']
                            Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
                            Reference[name:'ji']
    <BLANKLINE>

    As shown above, the resulting Schedule now contains a NemoLoop, indicated
    by the "type='lon'" (for 'longitude') annotation for the Loop node.

    """
    @property
    def name(self):
        '''
        :returns: the name of the transformation.
        :rtype: str

        TODO #1214 remove this method.

        '''
        return type(self).__name__

    def validate(self, node, options=None):
        '''
        Check that the supplied node is a valid target for this transformation.

        :param node: the target of the transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        :raises TransformationError: if the supplied node is not a Routine.

        '''
        super(CreateNemoLoopTrans, self).validate(node, options=options)

        if not isinstance(node, Loop):
            raise TransformationError(
                "Error in CreateNemoLoopTrans transformation. The supplied "
                "node should be a PSyIR Loop but found '{0}'".format(
                    type(node).__name__))

    def apply(self, loop, options=None):
        '''
        Takes a generic PSyIR Loop node and replaces it with a NemoLoop.

        :param loop: the Loop node to be transformed.
        :type loop: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        self.validate(loop, options=options)

        # Convert a generic loop into a NEMO Loop by creating a new
        # NemoLoop object and inserting it into the PSyIR.
        table = loop.loop_body.symbol_table.detach()
        nodes = loop.pop_all_children()
        new_loop = NemoLoop.create(loop.variable,
                                   nodes[0], nodes[1], nodes[2],
                                   nodes[3].pop_all_children())
        # TODO #1377 the NemoLoop.create() interface needs extending to accept
        # a SymbolTable.
        new_loop.loop_body.symbol_table.detach()
        table.attach(new_loop.loop_body)

        loop.replace_with(new_loop)


# For AutoAPI documentation generation
__all__ = ['CreateNemoLoopTrans']
