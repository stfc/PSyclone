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
# Author A. B. G. Chalk STFC Daresbury Lab
''' This module provides the OMPTaskTrans transformation.'''
from psyclone.errors import GenerationError
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyGen import Kern
from psyclone.psyir.transformations.fold_conditional_return_expressions_trans \
        import FoldConditionalReturnExpressionsTrans
from psyclone.psyir.transformations.parallel_loop_trans import\
    ParallelLoopTrans
from psyclone.psyir.nodes import CodeBlock, Call
from psyclone.psyir.nodes import DynamicOMPTaskDirective
from psyclone.psyir.transformations.inline_trans import InlineTrans
from psyclone.psyir.transformations.transformation_error import \
        TransformationError


class OMPTaskTrans(ParallelLoopTrans):
    ''' Apply an OpenMP Task Transformation to a Loop. The Loop must
    be within an OpenMP Serial region (Single or Master) at codegen time.
    '''

    def __str__(self):
        return "Adds an 'OMP TASK' directive to a statement"

    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str
        '''
        return "OMPTaskTrans"

    def validate(self, node, options=None):
        '''
        Validity checks for input arguments.

        :param node: the Loop node to validate.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None
        '''
        # Disallow CodeBlocks inside the region
        if len(node.walk(CodeBlock)) > 0:
            raise GenerationError(
                "OMPTaskTransformation cannot be applied to a region "
                "containing a code block")
        super().validate(node, options)

    def _directive(self, children, collapse=None):
        '''
        Creates the type of directive needed for this sub-class of
        transformation.

        :param children: list of Nodes that will be the children of \
                         the created directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param collapse: A required parameter from parent class. Must
                         never be set for TaskTrans (is None).
        :type collapse: None.

        :raises TransformationError: if the collapse attribute is set.

        :returns: The directive created for the OpenMP Task Directive
        :rtype: :py:class:`psyclone.psyGen.DynamicOMPTaskDirective`

        '''
        if collapse is not None:
            raise TransformationError("Collapse attribute should not be set "
                                      "for OMPTaskTrans")

        _directive = DynamicOMPTaskDirective(children=children)
        return _directive

    def _inline_kernels(self, node):
        '''
        Searches the PsyIR tree inside the directive and inlines any kern
        objects found.
        '''

        kerns = node.walk(Kern)
        kintrans = KernelModuleInlineTrans()
        cond_trans = FoldConditionalReturnExpressionsTrans()
        intrans = InlineTrans()
        for kern in kerns:
            kintrans.apply(kern)
            cond_trans.apply(kern.get_kernel_schedule())
            kern.lower_to_language_level()

        calls = node.walk(Call)
        for call in calls:
            intrans.apply(call)

    def apply(self, node, options=None):
        '''Apply the OMPTaskTrans to the specified node in a Schedule.

        Can only be applied to a Loop.

        The specified node is wrapped by directives during code generation
        like so:

        .. code-block:: fortran

          !$OMP TASK
          ...
          !$OMP END TASK

        At code-generation time (when
        :py:meth:`OMPTaskDirective.gen_code` is called), this node must be
        within (i.e. a child of) an OpenMP Serial region (OpenMP Single or
        OpenMP Master)

        Any kernels or Calls will be inlined into the region before the task
        transformation is applied.

        :param node: the supplied node to which we will apply the \
                     OMPTaskTrans transformation
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: dictionary of string:values or None
        '''
        if not options:
            options = {}
        self._inline_kernels(node)
        super(OMPTaskTrans, self).apply(node, options)