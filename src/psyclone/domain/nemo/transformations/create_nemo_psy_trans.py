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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford and S. Siso, STFC Daresbury Lab


'''Module providing a transformation from a generic PSyIR representation of
   a PSy layer into a NEMO-specific one.

'''

from psyclone.transformations import Transformation, TransformationError
from psyclone.psyir.nodes import Routine, Loop, Node
from psyclone.domain.nemo.transformations import \
    CreateNemoInvokeScheduleTrans, \
    CreateNemoKernelTrans, CreateNemoLoopTrans


class CreateNemoPSyTrans(Transformation):
    """
    Transform generic (language-level) PSyIR representation into a PSyclone
    version with specialised, NEMO-specific nodes. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.domain.nemo.transformations import CreateNemoPSyTrans
    >>> code = '''
    ... subroutine sub()
    ...   integer :: ji, tmp(10)
    ...   do ji=1, 10
    ...     tmp(ji) = 2*ji
    ...   end do
    ... end subroutine sub'''
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> loop = psyir.walk(Loop)[0]
    >>> trans = CreateNemoPSyTrans()
    >>> trans.apply(psyir)
    >>> print(psyir.view(colour=False, indent="   "))
    FileContainer[]
       NemoInvokeSchedule[invoke='sub']
          0: Loop[type='lon', field_space='None', it_space='None']
             Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
             Literal[value:'10', Scalar<INTEGER, UNDEFINED>]
             Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
             Schedule[]
                0: InlinedKern[]
                   Schedule[]
                      0: Assignment[]
                         ArrayReference[name:'tmp']
                            Reference[name:'ji']
                         BinaryOperation[operator:'MUL']
                            Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
                            Reference[name:'ji']
    <BLANKLINE>

    The result of this transformation is that the root `Routine` has
    been converted into a `NemoInvokeSchedule`, the `Loop` is now a
    `NemoLoop` (with type 'lon' [for longitude]) and the body of the loop
    is now an `InlinedKern`.

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

        :param node: the root of the PSyIR tree to be transformed.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        :raises TransformationError: if the supplied node is not a PSyIR node.

        '''
        if not isinstance(node, Node):
            raise TransformationError(
                "Error in CreateNemoPSyTrans transformation. The supplied node"
                " should be a PSyIR Node but found '{0}'".format(
                    type(node).__name__))

        super(CreateNemoPSyTrans, self).validate(node, options=options)

    def apply(self, psyir, options=None):
        '''
        Takes generic PSyIR and replaces recognised structures with
        NEMO-specific PSyIR (in-place). Note that this may mean replacing
        the top-level node itself and therefore this routine returns the
        root of the modified tree.

        :param psyir: the root node of the PSyIR tree to process.
        :type psyir: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        self.validate(psyir, options=options)

        invoke_trans = CreateNemoInvokeScheduleTrans()
        kern_trans = CreateNemoKernelTrans()
        loop_trans = CreateNemoLoopTrans()

        # Since the transformations replace nodes in the tree, we apply
        # them 'depth first':

        # First, transform suitable Loop bodies into Kernels
        loops = psyir.walk(Loop)
        # Reverse the list so that we transform the deepest loop bodies first
        # so as to try to reduce repeated walking of the tree.
        loops.reverse()

        for loop in loops:
            try:
                kern_trans.apply(loop.loop_body)
            except TransformationError:
                # Not all loop bodies are valid kernels (e.g. if they do IO)
                pass

        # Second, transform generic Loops into NemoLoops
        for loop in loops:
            loop_trans.apply(loop)

        # Third, transform any Routines into NemoInvokeSchedules. Have to
        # allow for the supplied top-level node being a Routine and therefore
        # being replaced.
        for routine in psyir.walk(Routine):
            invoke_trans.apply(routine)


# For AutoAPI documentation generation
__all__ = ['CreateNemoPSyTrans']
