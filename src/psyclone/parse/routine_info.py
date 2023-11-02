# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology

'''This module contains the RoutineInfo class, which is used to store
and cache information about a routine (i.e. a subroutine or a function) in a
module.
'''

from psyclone.core import VariablesAccessInfo


# ============================================================================
class RoutineInfoBase:

    '''This is the base class for all classes that store information about
    subroutines, functions or generic interfaces. It stores the
    ModuleInformation object to which the routine belongs, the name and
    provides an interface to store the PSyIR (if it is created by the
    ModuleInfo object).

    :param module_info: the ModuleInfo object to which this routine belongs.
    :type module_info: :py:class:`psyclone.parse.ModuleInfo`
    :param str name: the name of the routine or generic interface.

    '''
    def __init__(self, module_info, name):
        self._module_info = module_info
        self._name = name
        self._psyir = None

    # -------------------------------------------------------------------------
    @property
    def name(self):
        ''':returns: the name of the routine.
        :rtype: str

        '''
        return self._name

    # -------------------------------------------------------------------------
    def set_psyir(self, psyir):
        '''Sets the PSyIR representation of this routine. This is called from
        the module info object that this object is managed by.

        :param psyir: the PSyIR of this routine.
        :type psyir: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._psyir = psyir

    # -------------------------------------------------------------------------
    def get_psyir(self):
        ''':returns: the PSyIR of this routine.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if self._psyir is None:
            # Parsing the PSyIR in the parent ModuleInfo will populate the
            # PSyIR information for each routine, including this one:
            self.module_info.get_psyir()

        return self._psyir

    # -------------------------------------------------------------------------
    @property
    def module_info(self):
        ''':returns: the ModuleInfo object to which this Routine belongs.
        :rtype: :py:class:`psyclone.parse.ModuleInfo`

        '''
        return self._module_info

    # -------------------------------------------------------------------------
    def get_non_local_symbols(self):
        '''This function returns a list of non-local accesses in the given
        routine. It returns a list of triplets, each one containing:
        - the type ('routine', 'function', 'reference', 'unknown').
          The latter is used for array references or function calls,
          which we cannot distinguish till #1314 is done.
        - the name of the module (lowercase). This can be 'None' if no
          module information is available.
        - the name of the symbol (lowercase)
        - the access information for the given variable

        :returns: the non-local accesses in this routine.
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
                          :py:class:`psyclone.core.SingleVariableAccessInfo`]]

        '''

    # ------------------------------------------------------------------------
    def get_var_accesses(self):
        ''':returns: the variable access information for this routine.
        :rtype: :py:class:`psyclone.core.VariablesAccessInfo`

        '''


# ============================================================================
class RoutineInfo(RoutineInfoBase):
    '''This class stores information about a routine (function, subroutine).

    :param module_info: the ModuleInfo object which manages this object.
    :type module_info: :py:class:`psyclone.parse.ModuleInfo`
    :param ast: the AST of this routine.
    :type ast: Union[:py:class:`fparser.two.Fortran2003.Function_Subprogram`,
                     :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram`]

    '''
    def __init__(self, module_info, ast):
        name = str(ast.content[0].items[1])
        super().__init__(module_info, name)
        self._ast = ast

        # List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
        #           :py:class:`psyclone.core.SingleVariableAccessInfo`]]
        self._non_locals = None
        self._var_accesses = None

    # ------------------------------------------------------------------------
    def get_var_accesses(self):
        ''':returns: the variable access information for this routine.
        :rtype: :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        if self._var_accesses is None:
            self._var_accesses = \
                VariablesAccessInfo(self.get_psyir(),
                                    options={"USE-ORIGINAL-NAMES": True})

        return self._var_accesses

    # ------------------------------------------------------------------------
    def get_non_local_symbols(self):
        '''This function returns a list of non-local accesses in this
        routine. It returns a list of triplets, each one containing:
        - the type ('routine', 'function', 'reference', 'unknown').
          The latter is used for array references or function calls,
          which we cannot distinguish till #1314 is done.
        - the name of the module (lowercase). This can be 'None' if no
          module information is available.
        - the Signature of the symbol
        - the access information for the given variable

        :returns: the non-local accesses in this routine.
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
                          :py:class:`psyclone.core.SingleVariableAccessInfo`]]

        '''
        return self.get_psyir().get_non_local_symbols()


# ============================================================================
class GenericRoutineInfo(RoutineInfoBase):
    '''This class provides information about a generic function. It uses
    the RoutineInfo objects of the various routines it applies to to
    query the required data.

    :param module_info: the ModuleInfo object which manages this object.
    :type module_info: :py:class:`psyclone.parse.ModuleInfo`
    :param str name: name of the generic routine.
    :param List[str] list_of_routines: the list of routine names which are \
        part of this generic interface.

    '''
    def __init__(self, module_info, name, list_of_routines):
        super().__init__(module_info, name)

        self._all_routines = []
        for routine_name in list_of_routines:
            routine_info = module_info.get_routine_info(routine_name)
            self._all_routines.append(routine_info)

    # ------------------------------------------------------------------------
    def get_non_local_symbols(self):
        '''This function returns a list of non-local accesses in the functions
        that are part of this generic interface. Since PSyclone in general has
        no information about which specific function will be called (since this
        can depend on compile-time information like precision of variables),
        it will combine the information from each specific routine.
        It returns a list of triplets, each one containing:
        - the type ('routine', 'function', 'reference', 'unknown').
          The latter is used for array references or function calls,
          which we cannot distinguish till #1314 is done.
        - the name of the module (lowercase). This can be 'None' if no
          module information is available.
        - the name of the symbol (lowercase)
        - the access information for the given variable

        :returns: the non-local accesses for all specific routines of this \
            generic interface.
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
                          :py:class:`psyclone.core.SingleVariableAccessInfo`]]

        '''
        non_locals = []

        for routine_info in self._all_routines:
            non_locals.extend(routine_info.get_non_local_symbols())

        return non_locals
