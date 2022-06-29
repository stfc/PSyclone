# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
''' This module contains the implementation of the various Otter nodes.'''

from __future__ import absolute_import
import abc
import six


from psyclone.configuration import Config
from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import (AssignGen, UseGen, DeclGen, DirectiveGen,
                              CommentGen)
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.directive import StandaloneDirective, \
    RegionDirective
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, CHARACTER_TYPE, \
    RoutineSymbol, ContainerSymbol, ImportInterface


class OtterNode(Statement, metaclass=abc.ABCMeta):
    '''
    This class can be inserted into a schedule to instrument a set of nodes.
    Instrument means that calls to a Otter,
    https://github.com/Otter-Taskification/otter will be inserted before
    and after the list of child nodes, which will be used to generate
    data for use with Otter and pyOtter.

    :param children: the psyIR nodes that are children of this node. These \
                     will be made children of the child Schedule of otterNode.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    _children_valid_format = "Schedule"
    _text_name = "otterNode"
    _colour = "green"

    _start_subroutine_call = ""
    _end_subroutine_call = ""


    def __init__(self, children=None, parent=None):
        super().__init__(parent=parent)
        sched = Schedule(children=children)
        self.addchild(sched)


    def lower_to_language_level(self, options=None):
        '''TODO'''
        # TODO Add otter module into use modules in routine
        for child in self.children:
            child.lower_to_language_level()
        routine_schedule = self.ancestor(Routine)
        if routine_schedule is None:
            raise GenerationError(
                f"An OtterNode must be inside a Routine context when "
                f"lowering but '{self}' is not.")

        routine_name = routine_schedule.name

        # Add the otter module
        symtab = routine_schedule.symbol_table
        csymbol = None
        try:
            csymbol = symtab.lookup("otter_serial")
        except KeyError:
#            csymbol = ContainerSymbol("otter_serial", wildcard_import=True)
            csymbol = ContainerSymbol("otter_serial") # TODO Better if this works
            symtab.add(csymbol)

        if self._start_subroutine_call != "":
            file_symbol = DataSymbol("__FILE__", CHARACTER_TYPE)
            routine = RoutineSymbol(self._start_subroutine_call,
                                    interface=ImportInterface(csymbol))
            try:
                symtab.lookup(self._start_subroutine_call)
            except KeyError:
                symtab.add(routine)
            line_symbol = DataSymbol("__LINE__", INTEGER_TYPE)
            argument_list = []
            argument_list.append(Reference(file_symbol))
            argument_list.append(Literal(routine_name, CHARACTER_TYPE))
            argument_list.append(Reference(line_symbol))
            start_call = Call.create(routine, argument_list)

            self.parent.children.insert(self.position, start_call)

        # Insert the body of the profiled region between the start and
        # end calls
        for child in self.children[0].pop_all_children():
            self.parent.children.insert(self.position, child)

        if self._end_subroutine_call != "":
            routine = RoutineSymbol(self._end_subroutine_call,
                                    interface=ImportInterface(csymbol))
            try:
                symtab.lookup(self._end_subroutine_call)
            except KeyError:
                symtab.add(routine)
            argument_list = []
            end_call = Call.create(routine, argument_list)

            self.parent.children.insert(self.position, end_call)

        # Finally we can detach this node
        self.detach()

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position == 0 and isinstance(child, Schedule)


class OtterTraceSetupNode(OtterNode):
    '''
    Node to represent the OtterTraceInitialise and OtterTraceFinalise
    calls.
    '''
    _children_valid_format = "Schedule"
    _text_name = "otterTraceSetupNode"
    _colour = "green"

    _start_subroutine_call = "fortran_otterTraceInitialise_i"
    _end_subroutine_call = "fortran_otterTraceFinalise"


class OtterParallelNode(OtterNode):
    '''
    Node to represent OtterParallelBegin and OtterParallelEnd calls.
    '''
    _children_valid_format = "Schedule"
    _text_name = "otterParallelNode"
    _colour = "green"

    _start_subroutine_call = "fortran_otterThreadsBegin_i"
    _end_subroutine_call = "fortran_otterThreadsEnd"


class OtterTaskNode(OtterNode):
    '''
    Node to represent OtterTaskBegin and OtterTaskEnd calls.
    '''
    _children_valid_format = "Schedule"
    _text_name = "otterTaskNode"
    _colour = "green"

    _start_subroutine_call = "fortran_otterTaskBegin_i"
    _end_subroutine_call = "fortran_otterTaskEnd"


class OtterLoopNode(OtterNode):
    '''
    Node to represent OtterLoopBegin and OtterLoopEnd calls.
    '''
    _children_valid_format = "Schedule"
    _text_name = "otterLoopNode"
    _colour = "green"

    _start_subroutine_call = "fortran_otterLoopBegin_i"
    _end_subroutine_call = "fortran_otterLoopEnd"


class OtterLoopIterationNode(OtterNode):
    '''
    Node to represent OtterLoopIterationBegin and OtterLoopIterationEnd
    cals.
    '''
    _children_valid_format = "Schedule"
    _text_name = "otterLoopIterationNode"
    _colour = "green"

    _start_subroutine_call = "fortran_otterLoopIterationBegin_i"
    _end_subroutine_call = "fortran_otterLoopIterationEnd"


class OtterSynchroniseChildrenNode(OtterNode):
    '''
    Node to represent OtterSynchroniseChildTasks call.
    '''
    _children_valid_format = "Schedule"
    _text_name = "otterSynchroniseChildrenNode"
    _colour = "green"

    _start_subroutine_call = "fortran_otterSynchroniseTasks_i"


class OtterSynchroniseDescendantTasksNode(OtterNode):
    '''
    Node to represent OtterSynchroniseDescendantTasksBegin and 
    OtterSynchroniseDescendantTasksEnd calls.
    '''
    _children_valid_format = "Schedule"
    _text_name = "otterSynchroniseDescendantTasksNode"
    _colour = "green"

    _start_subroutine_call = "fortran_otterSynchroniseDescendantTasksBegin_i"
    _end_subroutine_call = "fortran_otterSynchroniseDescendantTasksEnd"


class OtterTraceNode(OtterNode):
    '''
    Node to represent OtterTraceStart and OtterTraceEnd calls.
    '''
    _children_valid_format = "Schedule"
    _text_name = "otterTraceNode"
    _colour = "green"

    _start_subroutine_call = "fortran_otterTraceStart"
    _end_subroutine_call = "fortran_otterTraceStop"

    def lower_to_language_level(self, options=None):
        '''TODO'''
        # TODO Add otter module into use modules in routine
        for child in self.children:
            child.lower_to_language_level()
        routine_schedule = self.ancestor(Routine)
        if routine_schedule is None:
            raise GenerationError(
                f"An OtterNode must be inside a Routine context when "
                f"lowering but '{self}' is not.")

        routine_name = routine_schedule.name

        # Add the otter module
        symtab = routine_schedule.symbol_table
        try:
            csymbol = symtab.lookup("otter_serial")
        except KeyError:
            csymbol = ContainerSymbol("otter_serial")
            symtab.add(csymbol)

        if self._start_subroutine_call != "":
            routine = RoutineSymbol(self._start_subroutine_call,
                                    interface=ImportInterface(csymbol))
            try:
                symtab.lookup(self._start_subroutine_call)
            except KeyError:
                symtab.add(routine)
            argument_list = []
            start_call = Call.create(routine, argument_list)

            self.parent.children.insert(self.position, start_call)

        # Insert the body of the profiled region between the start and
        # end calls
        for child in self.children[0].pop_all_children():
            self.parent.children.insert(self.position, child)

        if self._end_subroutine_call != "":
            routine = RoutineSymbol(self._end_subroutine_call,
                                    interface=ImportInterface(csymbol))
            try:
                symtab.lookup(self._end_subroutine_call)
            except KeyError:
                symtab.add(routine)
            argument_list = []
            end_call = Call.create(routine, argument_list)

            self.parent.children.insert(self.position, end_call)

        # Finally we can detach this node
        self.detach()
