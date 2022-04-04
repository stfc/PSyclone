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
# Modified by S. Siso and R. W. Ford, STFC Daresbury Lab

'''
Module providing a transformation from a generic PSyIR Schedule into a
NEMO Kernel.
'''

from psyclone.errors import LazyString
from psyclone.nemo import NemoKern
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Schedule, Loop, Call, CodeBlock, Assignment
from psyclone.transformations import Transformation, TransformationError


class CreateNemoKernelTrans(Transformation):
    """
    Transform a generic PSyIR Schedule representing a loop body into a NEMO
    Kernel. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.domain.nemo.transformations import CreateNemoKernelTrans
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
    >>> trans = CreateNemoKernelTrans()
    >>> trans.apply(loop.loop_body)
    >>> print(psyir.view(colour=False, indent="  "))
    FileContainer[]
      Routine[name:'sub']
        0: Loop[type='None', field_space='None', it_space='None']
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
                    Literal[value:'2.0', Scalar<REAL, UNDEFINED>]
                    Reference[name:'ji']
    <BLANKLINE>

    The resulting Schedule contains a NemoKern (displayed as an
    'InlinedKern' by the view() method).

    """
    @property
    def name(self):
        '''
        :returns: the name of the transformation.
        :rtype: str

        TODO #1214 - replace this method with Transformation.name()

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

        :raises TransformationError: if the supplied node is not a Schedule, \
            is not within a loop or cannot be represented as a Kernel.

        '''
        super(CreateNemoKernelTrans, self).validate(node, options=options)

        if not isinstance(node, Schedule):
            raise TransformationError(
                "Error in NemoKernelTrans transformation. The supplied node "
                "should be a PSyIR Schedule but found '{0}'".format(
                    type(node).__name__))

        # A Kernel must be within a Loop
        if not isinstance(node.parent, Loop):
            raise TransformationError(
                "Error in NemoKernelTrans transformation. The supplied "
                "Schedule must be within a Loop.")

        # A kernel cannot contain loops, calls, other kernels or unrecognised
        # code (including IO operations). We must also check for array
        # assignments. Since this walk can be very expensive if a routine
        # contains a lot of code, we do it just once and get it to stop early
        # if it encounters one of the forbidden node types. In that case the
        # last node in the returned list will be of a forbidden type.
        nodes = node.walk((Assignment, CodeBlock, Loop, Call, NemoKern),
                          stop_type=(CodeBlock, Loop, Call, NemoKern))
        if nodes and isinstance(nodes[-1], (CodeBlock, Loop, Call, NemoKern)):
            raise TransformationError(
                "Error in NemoKernelTrans transformation. A NEMO Kernel cannot"
                " contain a node of type: '{0}'".format(
                    type(nodes[-1]).__name__))

        # Check for array assignments
        assigns = [assign for assign in nodes if
                   (isinstance(assign, Assignment) and assign.is_array_range)]
        if assigns:
            fwriter = FortranWriter()
            # Using LazyString to improve performance when using
            # exceptions to skip invalid regions.
            raise TransformationError(LazyString(
                lambda: "A NEMO Kernel cannot contain array assignments "
                "but found: {0}".format(
                    [fwriter(node).rstrip("\n") for node in nodes])))

    def apply(self, sched, options=None):
        '''
        Takes a generic PSyIR Schedule and replaces it with a NEMO Kernel.

        :param sched: the Schedule node to be transformed.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        self.validate(sched, options=options)

        nemokern = NemoKern(sched.pop_all_children(), parent=sched)
        sched.addchild(nemokern)


# For AutoAPI documentation generation
__all__ = ['CreateNemoKernelTrans']
