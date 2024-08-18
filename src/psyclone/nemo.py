# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified work Copyright (c) 2019 by J. Henrichs, Bureau of Meteorology

'''This module implements the PSyclone NEMO API by specialising
   the required base classes for code generation (PSy, Invokes,
   Invoke, InvokeSchedule, Loop, CodedKern, Arguments and KernelArgument).

'''

from fparser.two.utils import walk
from fparser.two import Fortran2003
from psyclone.configuration import Config
from psyclone.core import Signature
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.domain.nemo import NemoConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, InlinedKern
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (ACCEnterDataDirective, ACCUpdateDirective,
                                  Schedule, Routine)
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


class NemoInvoke(Invoke):
    '''
    Represents a NEMO 'Invoke' which, since NEMO is existing code, means
    an existing program unit, e.g. a subroutine.

    :param sched: PSyIR node representing the program unit.
    :type sched: :py:class:`psyclone.psyir.nodes.Routine`
    :param str name: the name of this Invoke (program unit).
    :param invokes: the Invokes object that holds this Invoke.
    :type invokes: :py:class:`psyclone.nemo.NemoInvokes`

    '''
    def __init__(self, sched, name, invokes):
        # pylint: disable=super-init-not-called
        self._invokes = invokes
        self._schedule = sched
        self._name = name
        self._schedule.invoke = self


class NemoInvokes(Invokes):
    '''
    Class capturing information on all 'Invokes' (program units) within
    a single NEMO source file.

    :param psyir: the language-level PSyIR for the whole Fortran source file.
    :type psyir: :py:class:`psyclone.psyir.nodes.Container`
    :param psy: the PSy object containing all information for this source file.
    :type psy: :py:class:`psyclone.nemo.NemoPSy`

    '''
    def __init__(self, psyir, psy):
        # pylint: disable=super-init-not-called
        self._psy = psy
        self.invoke_map = {}
        self.invoke_list = []

        # Transform the language-level PSyIR into NEMO-specific PSyIR
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.nemo.transformations import CreateNemoPSyTrans
        CreateNemoPSyTrans().apply(psyir)
        routines = psyir.walk(Routine)

        # Create an Invoke for each routine we've found
        for subroutine in routines:

            my_invoke = NemoInvoke(subroutine, subroutine.name, self)
            self.invoke_map[subroutine.name] = my_invoke
            self.invoke_list.append(my_invoke)


class NemoPSy(PSy):
    '''
    The NEMO-specific PSy class. This creates a NEMO-specific
    invokes object (which controls all the required invocation calls).

    :param ast: the fparser2 AST for this PSy layer (i.e. NEMO routine)
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
               :py:class:`fparser.two.Fortran2003.Function_Subprogram`.
    :raises InternalError: if no Fortran2003.Name nodes are found in the \
                           supplied AST.
    '''
    def __init__(self, ast):
        # pylint: disable=super-init-not-called
        names = walk(ast.content, Fortran2003.Name)
        # The name of the program unit will be the first in the list
        if not names:
            raise InternalError("Found no names in supplied Fortran - should "
                                "be impossible!")
        self._name = str(names[0]) + "_psy"

        processor = Fparser2Reader()
        psyir = processor.generate_psyir(ast)

        self._invokes = NemoInvokes(psyir, self)
        self._container = psyir

    @property
    def gen(self):
        '''
        Generate the Fortran for the NEMO code represented by this
        NemoPSy object.

        :returns: the Fortran code.
        :rtype: str

        '''
        enable_checks = Config.get().backend_checks_enabled
        fwriter = FortranWriter(check_global_constraints=enable_checks)
        return fwriter(self._container)


class NemoInvokeSchedule(InvokeSchedule):
    '''
    The NEMO-specific InvokeSchedule sub-class. This is the top-level node in
    PSyclone's IR of a NEMO program unit (program, subroutine etc).

    :param str name: the name of this NemoInvokeSchedule (Routine).
    :param invoke: the Invoke to which this NemoInvokeSchedule belongs.
    :type invoke: :py:class:`psyclone.nemo.NemoInvoke`
    :param kwargs: additional keyword arguments provided to the super class.
    :type kwargs: unwrapped dict.

    '''
    _text_name = "NemoInvokeSchedule"

    def __init__(self, name, invoke=None, **kwargs):
        super().__init__(name, None, None, **kwargs)

        self._invoke = invoke

    def coded_kernels(self):
        '''
        Returns a list of all of the user-supplied kernels (as opposed to
        builtins) that are beneath this node in the PSyIR. In the NEMO API
        this means all instances of InlinedKern.

        :returns: all user-supplied kernel calls below this node.
        :rtype: list of :py:class:`psyclone.psyGen.CodedKern`
        '''
        return self.walk(InlinedKern)


class NemoLoop(PSyLoop):
    '''
    Class representing a PSyLoop in NEMO.

    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    '''
    def __init__(self, **kwargs):
        const = NemoConstants()
        super().__init__(valid_loop_types=const.VALID_LOOP_TYPES, **kwargs)

    @staticmethod
    def create(variable, start, stop, step, children):
        '''Create a NemoLoop instance given valid instances of a variable,
        start, stop and step nodes, and a list of child nodes for the
        loop body.

        :param variable: the PSyIR node containing the variable \
            of the loop iterator.
        :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param start: the PSyIR node determining the value for the \
            start of the loop.
        :type start: :py:class:`psyclone.psyir.nodes.Node`
        :param end: the PSyIR node determining the value for the end \
            of the loop.
        :type end: :py:class:`psyclone.psyir.nodes.Node`
        :param step: the PSyIR node determining the value for the loop \
            step.
        :type step: :py:class:`psyclone.psyir.nodes.Node`
        :param children: a list of PSyIR nodes contained in the \
            loop.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a NemoLoop instance.
        :rtype: :py:class:`psyclone.nemo.NemoLoop`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        NemoLoop._check_variable(variable)

        if not isinstance(children, list):
            raise GenerationError(
                f"children argument in create method of NemoLoop class "
                f"should be a list but found '{type(children).__name__}'.")

        # Create the loop
        loop = NemoLoop(variable=variable)
        schedule = Schedule(children=children)
        loop.children = [start, stop, step, schedule]

        # Indicate the type of loop
        loop_type_mapping = Config.get().api_conf("nemo") \
                                        .get_loop_type_mapping()
        loop.loop_type = loop_type_mapping.get(variable.name, "unknown")
        return loop


# TODO #1872: Avoid the duplication below and move to src/psyclone/domain/nemo.
# Alternatively, bring removal of loop variables to the transformation using
# lifetime analysis and remove these sub-classes.
class NemoACCEnterDataDirective(ACCEnterDataDirective):
    '''
    NEMO-specific support for the OpenACC enter data directive.

    '''
    def lower_to_language_level(self):
        '''
        In-place replacement of this directive concept into language-level
        PSyIR constructs.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        lowered = super().lower_to_language_level()

        # Remove known loop variables from the set of variables to transfer
        loop_var = Config.get().api_conf("nemo").get_loop_type_mapping().keys()
        self._sig_set.difference_update({Signature(var) for var in loop_var})
        return lowered


class NemoACCUpdateDirective(ACCUpdateDirective):
    '''
    NEMO-specific support for the OpenACC update directive.

    '''
    def lower_to_language_level(self):
        '''
        In-place replacement of this directive concept into language-level
        PSyIR constructs.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        lowered = super().lower_to_language_level()

        # Remove known loop variables from the set of variables to transfer
        loop_var = Config.get().api_conf("nemo").get_loop_type_mapping().keys()
        self._sig_set.difference_update({Signature(var) for var in loop_var})
        return lowered
