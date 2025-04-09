# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module creates an LFRic-specific Invokes object which controls all
    the required invocation calls. It also overrides the PSy gen method so
    that LFRic-specific PSy module code is generated.
    '''

from collections import OrderedDict

from psyclone.configuration import Config
from psyclone.domain.lfric import (LFRicConstants, LFRicSymbolTable,
                                   LFRicInvokes)
from psyclone.f2pygen import InterfaceDeclGen, ModuleGen, UseGen, PSyIRGen
from psyclone.psyGen import PSy, InvokeSchedule
from psyclone.psyir.nodes import ScopingNode
from psyclone.psyir.symbols import GenericInterfaceSymbol


class LFRicPSy(PSy):
    '''
    The LFRic-specific PSy class. This creates an LFRic-specific
    Invokes object (which controls all the required invocation calls).
    It also overrides the PSy gen method so that we generate
    LFRic-specific PSy module code.

    :param invoke_info: object containing the required invocation information
                        for code optimisation and generation.
    :type invoke_info: :py:class:`psyclone.parse.algorithm.FileInfo`

    '''
    def __init__(self, invoke_info):
        # Make sure the scoping node creates LFRicSymbolTables
        # TODO #1954: Remove the protected access using a factory
        ScopingNode._symbol_table_class = LFRicSymbolTable
        Config.get().api = "lfric"
        PSy.__init__(self, invoke_info)
        self._invokes = LFRicInvokes(invoke_info.calls, self)
        # Initialise the dictionary that holds the names of the required
        # LFRic constants, data structures and data structure proxies for
        # the "use" statements in modules that contain PSy-layer routines.
        const = LFRicConstants()
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        infmod_list = [const_mod]
        # Add all field and operator modules that might be used in the
        # algorithm layer. These do not appear in the code unless a
        # variable is added to the "only" part of the
        # '_infrastructure_modules' map.
        for data_type_info in const.DATA_TYPE_MAP.values():
            infmod_list.append(data_type_info["module"])

        # This also removes any duplicates from infmod_list
        self._infrastructure_modules = OrderedDict(
            (k, set()) for k in infmod_list)

        kind_names = set()

        # The infrastructure declares integer types with default
        # precision so always add this.
        api_config = Config.get().api_conf("lfric")
        kind_names.add(api_config.default_kind["integer"])

        # Datatypes declare precision information themselves. However,
        # that is not the case for literals. Therefore deal
        # with these separately here.
        for invoke in self.invokes.invoke_list:
            schedule = invoke.schedule
            for kernel in schedule.kernels():
                for arg in kernel.args:
                    if arg.is_literal:
                        kind_names.add(arg.precision)
        # Add precision names to the dictionary storing the required
        # LFRic constants.
        self._infrastructure_modules[const_mod] = kind_names

    @property
    def name(self):
        '''
        :returns: a name for the PSy layer. This is used as the PSy module
                  name. We override the default value as the Met Office
                  prefer "_psy" to be appended, rather than prepended.
        :rtype: str

        '''
        return self._name + "_psy"

    @property
    def orig_name(self):
        '''
        :returns: the unmodified PSy-layer name.
        :rtype: str

        '''
        return self._name

    @property
    def infrastructure_modules(self):
        '''
        :returns: the dictionary that holds the names of the required
                  LFRic infrastructure modules to create "use"
                  statements in the PSy-layer modules.
        :rtype: dict of set

        '''
        return self._infrastructure_modules

    @property
    def gen(self):
        '''
        Generate PSy code for the LFRic API.

        :returns: root node of generated Fortran AST.
        :rtype: :py:class:`psyir.nodes.Node`

        '''
        # Create an empty PSy layer module
        psy_module = ModuleGen(self.name)

        # If the container has a Routine that is not an InvokeSchedule
        # it should also be added to the generated module.
        for routine in self.container.children:
            if not isinstance(routine, InvokeSchedule):
                psy_module.add(PSyIRGen(psy_module, routine))

        # Similarly, we have to take care of any Interface symbols (which
        # we may have if we've inlined a polymorphic Kernel).
        for sym in self.container.symbol_table.symbols:
            if isinstance(sym, GenericInterfaceSymbol):
                names = [rt.symbol.name for rt in sym.routines]
                psy_module.add(
                    InterfaceDeclGen(psy_module, sym.name, names))

        # Add all invoke-specific information
        self.invokes.gen_code(psy_module)

        # Include required constants and infrastructure modules. The sets of
        # required LFRic data structures and their proxies are updated in
        # the relevant field and operator subclasses of LFRicCollection.
        # Here we sort the inputs in reverse order to have "_type" before
        # "_proxy_type" and "operator_" before "columnwise_operator_".
        # We also iterate through the dictionary in reverse order so the
        # "use" statements for field types are before the "use" statements
        # for operator types.
        for infmod in reversed(self._infrastructure_modules):
            if self._infrastructure_modules[infmod]:
                infmod_types = sorted(
                    list(self._infrastructure_modules[infmod]), reverse=True)
                psy_module.add(UseGen(psy_module, name=infmod,
                                      only=True, funcnames=infmod_types))

        # Return the root node of the generated code
        return psy_module.root


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicPSy']
