# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author S. Siso, STFC Daresbury Lab
# Modified: A. R. Porter, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' PSyIR nodes package module '''

from psyclone.psyir.nodes.node import colored, Node, SCHEDULE_COLOUR_MAP
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.return_stmt import Return
from psyclone.psyir.nodes.assignment import Assignment
from psyclone.psyir.nodes.operation import Operation, UnaryOperation, \
    BinaryOperation, NaryOperation
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.ifblock import IfBlock
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.array_reference import ArrayReference
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.container import Container
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.extract_node import ExtractNode
from psyclone.psyir.nodes.kernel_schedule import KernelSchedule
from psyclone.psyir.nodes.nan_test_node import NanTestNode
from psyclone.psyir.nodes.profile_node import ProfileNode
from psyclone.psyir.nodes.psy_data_node import PSyDataNode
from psyclone.psyir.nodes.read_only_verify_node import ReadOnlyVerifyNode
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.call import Call

# The entities in the __all__ list are made available to import directly from
# this package e.g. 'from psyclone.psyir.nodes import Literal'
__all__ = [
        'colored',
        'SCHEDULE_COLOUR_MAP',
        'ArrayReference',
        'Assignment',
        'BinaryOperation',
        'Call',
        'CodeBlock',
        'Container',
        'DataNode',
        'ExtractNode',
        'IfBlock',
        'KernelSchedule',
        'Literal',
        'Loop',
        'NanTestNode',
        'NaryOperation',
        'Node',
        'Operation',
        'ProfileNode',
        'PSyDataNode',
        'Range',
        'ReadOnlyVerifyNode',
        'Reference',
        'Return',
        'Routine',
        'Schedule',
        'Statement',
        'UnaryOperation']
