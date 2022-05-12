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
# Modified: R. W. Ford and S. Siso, STFC Daresbury Lab

'''
Module providing a transformation from a generic PSyIR routine into a
NEMO InvokeSchedule.
'''

from psyclone.transformations import Transformation, TransformationError
from psyclone.psyir.nodes import Routine
from psyclone.nemo import NemoInvokeSchedule


class CreateNemoInvokeScheduleTrans(Transformation):
    """
    Transform a generic PSyIR Routine into a NEMO InvokeSchedule.
    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.domain.nemo.transformations import \
    CreateNemoInvokeScheduleTrans
    >>> code = '''
    ... subroutine sub()
    ...   integer :: ji
    ...   real :: tmp(10)
    ...   do ji=1, 10
    ...     tmp(ji) = 2.0*ji
    ...   end do
    ... end subroutine sub'''
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> loop = psyir.walk(Loop)[0]
    >>> trans = CreateNemoInvokeScheduleTrans()
    >>> trans.apply(psyir.children[0])
    >>> print(psyir.view(colour=False))
    FileContainer[]
        NemoInvokeSchedule[invoke='sub']
            0: Loop[type='None', field_space='None', it_space='None']
                Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
                Literal[value:'10', Scalar<INTEGER, UNDEFINED>]
                Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
                Schedule[]
                    0: Assignment[]
                        ArrayReference[name:'tmp']
                            Reference[name:'ji']
                        BinaryOperation[operator:'MUL']
                            Literal[value:'2.0', Scalar<REAL, UNDEFINED>]
                            Reference[name:'ji']
    <BLANKLINE>

    The root node of this example has been transformed from a Routine into a
    NemoInvokeSchedule.

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
        super(CreateNemoInvokeScheduleTrans, self).validate(node,
                                                            options=options)

        if not isinstance(node, Routine):
            raise TransformationError(
                "Error in NemoInvokeTrans transformation. The supplied node "
                "should be a PSyIR Routine but found '{0}'".format(
                    type(node).__name__))

    def apply(self, node, options=None):
        '''
        Takes a generic PSyIR Routine and replaces it with a
        NemoInvokeSchedule (in-place). Note that this may mean replacing
        the top-level node itself and therefore this routine returns the
        root of the modified tree.

        :param node: the routine node to be transformed.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of str:values or None

        '''
        self.validate(node, options=options)

        # If it's a function we need to provide the return_symbol_name to the
        # create method
        return_symbol_name = None
        if node.return_symbol:
            return_symbol_name = node.return_symbol.name

        new_node = NemoInvokeSchedule.create(
            node.name, node.symbol_table.detach(), node.pop_all_children(),
            is_program=node.is_program,
            return_symbol_name=return_symbol_name)

        # We need to replace the top node in the (possibly sub-) PSyIR
        # tree that we've been passed.
        if node.parent:
            node.replace_with(new_node)


# For AutoAPI documentation generation
__all__ = ['CreateNemoInvokeScheduleTrans']
