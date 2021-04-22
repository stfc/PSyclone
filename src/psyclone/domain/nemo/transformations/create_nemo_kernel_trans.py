# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''
Module providing a transformation from a generic PSyIR Schedule into a
NEMO Kernel.
'''

from psyclone.transformations import Transformation, TransformationError
from psyclone.psyir.nodes import Schedule, Loop, Call, CodeBlock, Assignment
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.nemo import NemoKern


class CreateNemoKernelTrans(Transformation):
    """
    Transform a generic PSyIR Schedule into a NEMO Kernel. For example:

    >>> from fparser.common.readfortran import FortranStringReader
    >>> from fparser.two.parser import ParserFactory
    >>> from psyclone.psyir.frontend.fparser2 import Fparser2Reader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.domain.nemo.transformations import CreateNemoKernelTrans
    >>> reader = FortranStringReader('''
    subroutine sub()
      integer :: ji
      real :: tmp(10)
      do ji=1, 10
        tmp(ji) = 2.0*ji
      end do
    end subroutine sub''')
    >>> parser = ParserFactory().create()
    >>> psyir = Fparser2Reader().generate_psyir(parser(reader))
    >>> loop = psyir.walk(Loop)[0]
    >>> trans = CreateNemoKernelTrans()
    >>> trans.apply(loop.loop_body)
    >>> psyir.view()

    The resulting Schedule will contain a NemoKern (displayed as an
    'InlinedKern' by the view() method.

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

        # Check for array assignments
        nodes = [assign for assign in node.walk(Assignment)
                 if assign.is_array_range]
        if nodes:
            fwriter = FortranWriter()
            raise TransformationError(
                "A NEMO Kernel cannot contain array assignments but found: "
                "{0}".format([fwriter(node).rstrip("\n") for node in nodes]))

        # A kernel cannot contain loops, calls or unrecognised code (including
        # IO operations. So if there is any node in the result of
        # the walk, this node cannot be represented as a NEMO kernel.
        nodes = node.walk((CodeBlock, Loop, Call))
        if nodes:
            raise TransformationError(
                "Error in NemoKernelTrans transformation. A NEMO Kernel cannot"
                " contain nodes of type: {0}".format(
                    [type(node).__name__ for node in nodes]))

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
