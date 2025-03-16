# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Author: H. Brunie, University of Grenoble Alpes

"""Module providing a transformation that removes IfBlock if the condition is
known to be constant for the whole runtime execution."""
from typing import Dict, Optional, Union

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import IfBlock, Routine, Node, Literal

from psyclone.psyir.symbols import ScalarType
from psyclone.psyir.transformations.transformation_error import (
    TransformationError,
)


class RemoveIfBlockTrans(Transformation):

    def __init__(self, dict_of_bool: Optional[dict[str, Union[bool, int]]] = None) -> None:
        super().__init__()
        self._known_variables: dict[str, Union[bool, int]] = dict_of_bool or {}

    def _if_else_replace(self, main_schedule, if_block, if_body_schedule):
        """This code is extracted from Martin Schreiber MR#2801.
        Little helper routine to eliminate one branch of an IfBlock
        :param main_schedule: Schedule where if-branch is used
        :type main_schedule: Schedule
        :param if_block: If-else block itself
        :type if_block: IfBlock
        :param if_body_schedule: The body of the if or else block
        :type if_body_schedule: Schedule
        """

        from psyclone.psyir.nodes import Schedule

        assert isinstance(main_schedule, Schedule)
        assert isinstance(if_body_schedule, Schedule)

        # Obtain index in main schedule
        idx = main_schedule.children.index(if_block)

        # Detach it
        if_block.detach()

        # Insert childreen of if-body schedule
        for child in if_body_schedule.children:
            main_schedule.addchild(child.copy(), idx)
            idx += 1

    def if_else_replace(self, if_block: IfBlock, is_true: bool):
        if is_true:
            self._if_else_replace(if_block.parent, if_block, if_block.if_body)
        else:
            if if_block.else_body:
                self._if_else_replace(
                    if_block.parent, if_block, if_block.else_body
                )
            else:
                if_block.detach()

    def _eliminate_ifblock_if_const_condition(self, if_block: IfBlock) -> None:
        """Eliminate if-block if conditions are constant booleans.
        :rtype: None
        """

        condition = if_block.condition
        if isinstance(condition, Literal) and condition.datatype.intrinsic is ScalarType.Intrinsic.BOOLEAN:
            if condition.value == "true":
                self.if_else_replace(if_block, is_true=True)
            else:
                self.if_else_replace(if_block, is_true=False)
        else:
            from psyclone.psyir.tools.evaluate_condition import EvaluateCondition, EvaluationError

            evaluate_condition = EvaluateCondition(self._known_variables)
            try:
                is_true: bool = evaluate_condition.evaluate(condition)
            except EvaluationError:
                return None
            self.if_else_replace(if_block, is_true)

    def apply(self, node: Routine, options=None):
        self.validate(node, options)
        for if_block in node.walk(IfBlock):
            self._eliminate_ifblock_if_const_condition(if_block)

    def validate(self, node: Node, options=None):
        if not isinstance(node, Routine):
            raise TransformationError("Only handles Routine node.")
