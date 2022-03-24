# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Modified: A. R. Porter and R. W. Ford, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology
# Modified: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' PSyIR nodes package module '''

from psyclone.psyir.nodes.node import colored, Node
from psyclone.psyir.nodes.scoping_node import ScopingNode
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.return_stmt import Return
from psyclone.psyir.nodes.assignment import Assignment
from psyclone.psyir.nodes.array_member import ArrayMember
from psyclone.psyir.nodes.array_of_structures_member import \
    ArrayOfStructuresMember
from psyclone.psyir.nodes.operation import Operation, UnaryOperation, \
    BinaryOperation, NaryOperation
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.ifblock import IfBlock
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.array_reference import ArrayReference
from psyclone.psyir.nodes.array_of_structures_reference import \
    ArrayOfStructuresReference
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.container import Container
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.extract_node import ExtractNode
from psyclone.psyir.nodes.kernel_schedule import KernelSchedule
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.nan_test_node import NanTestNode
from psyclone.psyir.nodes.profile_node import ProfileNode
from psyclone.psyir.nodes.psy_data_node import PSyDataNode
from psyclone.psyir.nodes.read_only_verify_node import ReadOnlyVerifyNode
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir.nodes.structure_member import StructureMember
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.file_container import FileContainer
from psyclone.psyir.nodes.directive import Directive, StandaloneDirective, \
    RegionDirective
from psyclone.psyir.nodes.acc_directives import ACCDirective, \
    ACCLoopDirective, ACCEnterDataDirective, ACCParallelDirective, \
    ACCKernelsDirective, ACCDataDirective, ACCUpdateDirective, \
    ACCStandaloneDirective, ACCRegionDirective, ACCRoutineDirective
from psyclone.psyir.nodes.omp_directives import OMPDirective, OMPDoDirective, \
    OMPParallelDirective, OMPParallelDoDirective, OMPSingleDirective, \
    OMPMasterDirective, OMPSerialDirective, OMPTaskloopDirective, \
    OMPTaskwaitDirective, OMPStandaloneDirective, OMPRegionDirective, \
    OMPTargetDirective, OMPLoopDirective, OMPDeclareTargetDirective
from psyclone.psyir.nodes.clause import Clause
from psyclone.psyir.nodes.omp_clauses import OMPGrainsizeClause, \
    OMPNogroupClause, OMPNowaitClause, OMPNumTasksClause


# The entities in the __all__ list are made available to import directly from
# this package e.g. 'from psyclone.psyir.nodes import Literal'
__all__ = [
        'colored',
        'ArrayMember',
        'ArrayReference',
        'ArrayOfStructuresMember',
        'ArrayOfStructuresReference',
        'Assignment',
        'BinaryOperation',
        'Call',
        'Clause',
        'CodeBlock',
        'Container',
        'DataNode',
        'FileContainer',
        'IfBlock',
        'Literal',
        'Loop',
        'Member',
        'NaryOperation',
        'Node',
        'Operation',
        'Range',
        'Reference',
        'Return',
        'Routine',
        'Schedule',
        'Statement',
        'StructureMember',
        'StructureReference',
        'UnaryOperation',
        'ScopingNode',
        # PSyclone-specific nodes
        'KernelSchedule',
        # PSyData Nodes
        'PSyDataNode',
        'ExtractNode',
        'ProfileNode',
        'NanTestNode',
        'ReadOnlyVerifyNode',
        # Directive Nodes
        'Directive',
        'RegionDirective',
        'StandaloneDirective',
        'ACCDirective',
        'ACCRegionDirective',
        'ACCStandaloneDirective',
        'ACCDataDirective',
        'ACCEnterDataDirective',
        'ACCParallelDirective',
        'ACCLoopDirective',
        'ACCKernelsDirective',
        'ACCUpdateDirective',
        'ACCRoutineDirective',
        'OMPDirective',
        'OMPRegionDirective',
        'OMPStandaloneDirective',
        'OMPParallelDirective',
        'OMPSerialDirective',
        'OMPSingleDirective',
        'OMPMasterDirective',
        'OMPTaskloopDirective',
        'OMPDoDirective',
        'OMPParallelDoDirective',
        'OMPTaskwaitDirective',
        'OMPTargetDirective',
        'OMPLoopDirective',
        'OMPDeclareTargetDirective',
        # OMP Clause Nodes
        'OMPGrainsizeClause',
        'OMPNogroupClause',
        'OMPNowaitClause',
        'OMPNumTasksClause'
        ]
