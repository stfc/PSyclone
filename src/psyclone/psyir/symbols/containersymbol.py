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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the ContainerSymbol and its interfaces.'''

from psyclone.psyir.symbols.symbol import Symbol, SymbolError
from psyclone.psyir.symbols.interfaces import SymbolInterface
from psyclone.configuration import Config


class ContainerSymbol(Symbol):
    ''' Symbol that represents a reference to a Container. The reference
    is lazy evaluated, this means that the Symbol will be created without
    parsing and importing the referenced container, but this can be imported
    when needed.

    :param str name: name of the symbol.
    :param bool wildcard_import: if all public Symbols of the Container are
        imported into the current scope. Defaults to False.
    :param bool is_intrinsic: if the module is an intrinsic import. Defauts
        to False.
    :param kwargs: additional keyword arguments provided by
        :py:class:`psyclone.psyir.symbols.Symbol`.
    :type kwargs: unwrapped dict.

    '''
    def __init__(self, name, **kwargs):
        super(ContainerSymbol, self).__init__(name)

        self._reference = None
        self._has_wildcard_import = False
        self._is_intrinsic = False

        self._process_arguments(**kwargs)

    def _process_arguments(self, **kwargs):
        ''' Process the arguments for the constructor and the specialise
        methods. In this case the wildcard_import and a change in default
        value for the interface.

        :param kwargs: keyword arguments which can be:\n
            :param bool wildcard_import: if all public Symbols of the
                Container are imported into the current scope. Defaults to
                False.\n
            :param bool is_intrinsic: if the module is an intrinsic import.
            Defaults to False.
            and the arguments in :py:class:`psyclone.psyir.symbols.Symbol`
        :type kwargs: unwrapped dict.

        :raises TypeError: if it is provided with an interface argument other
                then FortranModuleInterface.

        '''
        if not hasattr(self, '_reference'):
            self._reference = None

        if "wildcard_import" in kwargs:
            self.wildcard_import = kwargs.pop("wildcard_import")
        elif not hasattr(self, '_has_wildcard_import'):
            self._has_wildcard_import = False

        if "is_intrinsic" in kwargs:
            self.is_intrinsic = kwargs.pop("is_intrinsic")

        # TODO #1298: ContainerSymbol currently defaults to
        # FortranModuleInterface expecting externally defined containers
        # which can be imported, but this is not always true.
        if "interface" not in kwargs or kwargs["interface"] is None:
            kwargs["interface"] = FortranModuleInterface()
        elif not isinstance(kwargs["interface"], FortranModuleInterface):
            raise TypeError(f"A ContainerSymbol interface must be of type '"
                            f"FortranModuleInterface' but found "
                            f"'{type(kwargs['interface']).__name__}' for "
                            f"Container '{self.name}'.")
        super(ContainerSymbol, self)._process_arguments(**kwargs)

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: A symbol object with the same properties as this \
                  symbol object.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        # Use the generic Symbol copy and add the wildcard import value
        new_symbol = super(ContainerSymbol, self).copy()
        new_symbol.wildcard_import = self.wildcard_import
        new_symbol.is_intrinsic = self.is_intrinsic
        return new_symbol

    def find_container_psyir(self, local_node=None):
        ''' Searches for the Container that this Symbol refers to. If it is
        not available, use the interface to import the container. If
        `local_node` is supplied then the PSyIR tree below it is searched for
        the container first.

        :param local_node: root of PSyIR sub-tree to include in search for
                           the container.
        :type local_node: Optional[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: referenced container.
        :rtype: :py:class:`psyclone.psyir.nodes.Container`

        '''
        if not self._reference:
            # First check in the current PSyIR tree (if supplied).
            if local_node:
                lowered_name = self.name.lower()
                from psyclone.psyir.nodes.container import Container
                from psyclone.psyir.nodes.routine import Routine
                for local in local_node.root.walk(Container,
                                                  stop_type=Routine):
                    if lowered_name == local.name.lower():
                        self._reference = local
                        return self._reference
            # We didn't find it so now attempt to import the container.
            self._reference = self._interface.get_container(self._name)
        return self._reference

    def __str__(self):
        string = self._name + ": ContainerSymbol<"
        if self._reference:
            string += "linked>"
        else:
            string += "not linked>"
        return string

    @property
    def wildcard_import(self):
        '''
        :returns: whether or not there is a wildcard import of all public \
                  symbols from this Container.
        :rtype: bool

        '''
        return self._has_wildcard_import

    @wildcard_import.setter
    def wildcard_import(self, value):
        '''
        Set whether or not there is a wildcard import of all public symbols
        from this Container symbol.

        :param bool value: whether there is or is not a wildcard import.

        :raises TypeError: if the supplied `value` is not a bool.

        '''
        if not isinstance(value, bool):
            raise TypeError(f"wildcard_import must be a bool but got: "
                            f"'{type(value).__name__}'")
        self._has_wildcard_import = value

    @property
    def is_intrinsic(self):
        '''
        :returns: whether or not this module is an intrinsic module.
        :rtype: bool
        '''
        return self._is_intrinsic

    @is_intrinsic.setter
    def is_intrinsic(self, value):
        '''
        :param bool value: whether or not this is an intrinsic module.

        :raises TypeError: if the supplied `value` is not a bool.
        '''
        if not isinstance(value, bool):
            raise TypeError(f"is_intrinsic must be a bool but got: "
                            f"'{type(value).__name__}'.")
        self._is_intrinsic = value


class ContainerSymbolInterface(SymbolInterface):
    ''' Abstract implementation of the ContainerSymbol Interface '''

    @staticmethod
    def get_container(name):
        ''' Abstract method to import an external container, the specific
        implementation depends on the language used.

        :param str name: name of the external entity to be imported.

        :raises NotImplementedError: this is an abstract method.
        '''
        raise NotImplementedError("Abstract method")


class FortranModuleInterface(ContainerSymbolInterface):
    ''' Implementation of ContainerSymbolInterface for Fortran modules '''

    @staticmethod
    def get_container(name):
        ''' Imports a Fortran module as a PSyIR Container (via the
        ModuleManager) and returns it.

        :param str name: name of the module to be imported.

        :returns: container associated with the given name.
        :rtype: :py:class:`psyclone.psyir.nodes.Container`

        :raises SymbolError: the given Fortran module is not found on the
            import path.

        '''
        # pylint: disable-next=import-outside-toplevel
        from psyclone.parse import ModuleManager
        mod_manager = ModuleManager.get()

        # TODO #2011 - rationalise how this interacts with the kernel search
        # path set in generate().
        mod_manager.add_search_path(Config.get().include_paths,
                                    recursive=False)

        minfo = None
        try:
            minfo = mod_manager.get_module_info(name)
        except FileNotFoundError:
            pass
        if not minfo:
            raise SymbolError(
                f"Module '{name}' not found in any of the include_paths "
                f"directories {Config.get().include_paths}.")
        return minfo.get_psyir()


# For Sphinx AutoAPI documentation generation
__all__ = ['ContainerSymbol', 'ContainerSymbolInterface',
           'FortranModuleInterface']
