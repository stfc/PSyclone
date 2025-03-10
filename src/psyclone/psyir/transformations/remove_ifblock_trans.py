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

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import IfBlock, Routine, Node
from psyclone.psyir.symbols import ScalarType
from psyclone.psyir.transformations.transformation_error import (
    TransformationError,
)

from typing import Dict, Optional


class RemoveIfBlockTrans(Transformation):

    def __init__(self, json_file_abspath: Optional[str] = None) -> None:
        super().__init__()
        self._known_reference_bool: Dict[str, bool] = {}
        self._known_reference_int: Dict[str, bool] = {}
        if json_file_abspath is not None:
            with open(json_file_abspath, "r") as inf:
                import json

                json_data = json.load(inf)
                if (
                    json_data.get("known_reference_bool") is None
                    or json_data.get("known_reference_int") is None
                ):
                    raise TransformationError(
                        f"Wrong json data content: {json_file_abspath}."
                    )
                else:
                    self._known_reference_bool = json_data[
                        "known_reference_bool"
                    ]
                    self._known_reference_int = json_data[
                        "known_reference_int"
                    ]

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

    def _evaluate(self, condition: Node) -> bool:
        from psyclone.psyir.frontend.sympy_reader import SymPyReader
        from psyclone.psyir.backend.sympy_writer import SymPyWriter
        import sympy

        expr_sympy = SymPyWriter(condition)
        new_expr = sympy.simplify(expr_sympy)
        psyir_expr: Node = SymPyReader(new_expr)
        print(psyir_expr.debug_string())
        return True

    def _eliminate_ifblock_if_const_condition(self, if_block: IfBlock):
        """Eliminate if-block if conditions are constant booleans.
        :rtype: None
        """

        condition = if_block.condition
        if condition.datatype.intrinsic is ScalarType.Intrinsic.BOOLEAN:
            if condition.value == "true":
                self.if_else_replace(if_block, is_true=True)
            else:
                self.if_else_replace(if_block, is_true=False)
        else:
            is_true = self._evaluate(condition)
            self.if_else_replace(if_block, is_true)

    def apply(self, node: Routine, options=None):
        for if_block in node.walk(IfBlock):
            self._eliminate_ifblock_if_const_condition(if_block)

    def validate(self, node: Node, options=None):
        if not isinstance(node, Routine):
            raise TransformationError("Only handles Routine node.")
