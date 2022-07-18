# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab.

''' Module containing the implementation of the LFRic-specific kernel-functor
    classes.

'''

from psyclone.domain.common.algorithm import KernelFunctor
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP_CAPITALISED
from psyclone.psyir.symbols import DataTypeSymbol, StructureType, Symbol


class LFRicFunctor(KernelFunctor):
    '''Base functor class for all LFRic user-supplied and built-in kernels.

    '''
    _text_name = "LFRicFunctor"


class LFRicKernelFunctor(LFRicFunctor):
    '''Object containing a call to a user-provided LFRic kernel, a description
    of its required interface and the arguments to be passed to it.

    '''
    _text_name = "LFRicKernelFunctor"


class LFRicBuiltinFunctor(LFRicFunctor):
    ''' Base class which all LFRic builtins subclass. Contains a builtin call,
    a description of its required interface and the arguments to be passed
    to it.

    '''
    _text_name = "LFRicBuiltinFunctor"
    _builtin_name = ""

    @classmethod
    def create(cls, table, arguments):
        '''
        An appropriate DataTypeSymbol is created and added to the supplied
        symbol table (if it does not already contain one). This is then
        passed to the create() method in the base class to create an instance
        of an LFRic builtin call with the supplied list of arguments.

        :param table: the symbol table to which to add the symbol for this \
                      functor.
        :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arguments: the arguments to this routine. These are \
                          added as child nodes.
        :type arguments: List[:py:class:`psyclone.psyir.nodes.DataNode`]

        :returns: a functor object describing an LFRic builtin.
        :rtype: :py:class:`psyclone.domain.lfric.algorithm.LFRicBuiltinFunctor`

        :raises InternalError: if a symbol is found that has the same as a \
            built-in but does not have the correct properties.
        '''
        # We can't use find_or_create() here as that raises an Exception if
        # the symbol that is found is not of the correct type.
        try:
            sym = table.lookup(cls._builtin_name)
            # pylint: disable=unidiomatic-typecheck
            if type(sym) is Symbol:
                if not sym.is_unresolved:
                    raise InternalError(
                        f"An entry with the same name as builtin '{sym.name}' "
                        f"exists but has an interface of '{sym.interface}' "
                        f"instead of being unresolved.")
                sym.specialise(DataTypeSymbol)
                sym.datatype = StructureType()
            elif not isinstance(sym, DataTypeSymbol):
                raise InternalError(
                    f"An entry with the same name as builtin '{sym.name}' "
                    f"exists but it is a '{type(sym).__name__}' and not a "
                    f"DataTypeSymbol.")
        except KeyError:
            sym = table.new_symbol(cls._builtin_name,
                                   symbol_type=DataTypeSymbol,
                                   datatype=StructureType())

        return super().create(sym, arguments)

    def lower_to_language_level(self):
        ''' Removes the symbol representing this BuiltIn as it only
        exists in the DSL. '''
        table = self.scope.symbol_table
        try:
            sym = table.lookup(self._builtin_name)
            table = sym.find_symbol_table(self)
            # TODO #898 SymbolTable.remove() does not yet support
            # DataTypeSymbols.
            # pylint: disable=protected-access
            del table._symbols[self._builtin_name]
        except KeyError:
            pass


class LFRicBuiltinFunctorFactory():
    '''
    This class generates a Functor class for each built-in supported by
    LFRic. These are accessed using the `get_builtin_class()` method.

    '''
    #: Classes representing LFRic built-in functors, indexed by name.
    _builtin_functor_map = {}

    @staticmethod
    def _create_classes():
        '''
        Generate classes representing LFRic BuiltIn Functors by using
        the type() function. The classes are stored in a class variable
        so that this construction is only ever performed once.

        '''
        if LFRicBuiltinFunctorFactory._builtin_functor_map:
            return

        for name in BUILTIN_MAP_CAPITALISED:
            lname = name.lower()
            LFRicBuiltinFunctorFactory._builtin_functor_map[lname] = type(
                f"LFRic_{name}_Functor",
                (LFRicBuiltinFunctor,),
                {"_builtin_name": name.lower()})
        
    @staticmethod
    def _get_builtin_class(name):
        '''
        Look-up the class associated with the named built-in.

        :param str name: name of an LFRic built-in kernel.

        :returns: the class representing the functor for the named built-in.
        :rtype: type
        '''
        if not LFRicBuiltinFunctorFactory._builtin_functor_map:
            LFRicBuiltinFunctorFactory._create_classes()
        return LFRicBuiltinFunctorFactory._builtin_functor_map[name]

    @staticmethod
    def create(name, table, arguments):
        '''
        '''
        cls = LFRicBuiltinFunctorFactory._get_builtin_class(name)
        return cls.create(table, arguments)


# For AutoAPI documentation generation.
__all__ = ['LFRicBuiltinFunctor',
           'LFRicKernelFunctor',
           'LFRicBuiltinFunctorFactory']
