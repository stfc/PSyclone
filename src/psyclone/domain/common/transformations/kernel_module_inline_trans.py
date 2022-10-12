# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the KernelModuleInlineTrans transformation. '''


from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import RoutineSymbol, DataSymbol, \
    DataTypeSymbol, Symbol, ContainerSymbol
from psyclone.psyir.nodes import Container, ScopingNode, Reference, Routine, \
    Literal, CodeBlock, Call


class KernelModuleInlineTrans(Transformation):
    ''' Module-inlines (bring the subroutine to the same compiler-unit) the
    subroutine pointed by this Kernel. For example:

    .. code-block:: python

        from psyclone.domain.common.transformations import \\
                KernelModuleInlineTrans

        inline_trans = KernelModuleInlineTrans()
        inline_trans.apply(schedule.walk(CodedKern)[0])

        print(schedule.parent.view())


    .. warning ::
        Not all kernel subroutines can be module-inlined. This transformation
        will reject attempts to in-line kernels that access global data in the
        original module.

    '''

    def __str__(self):
        return "Inline a kernel subroutine into the PSy module"

    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a Kernel and that it is possible to
        inline its PSyIR.

        :param kern: the kernel which is the target of the transformation.
        :type kern: :py:class:`psyclone.psyGen.CodedKern`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the target node is not a sub-class of \
                                     psyGen.CodedKern.
        :raises TransformationError: if the subroutine containing the \
                                     implementation of the kernel cannot be \
                                     retrieved with 'get_kernel_schedule'.
        :raises TransformationError: if the kernel cannot be safely inlined.

        '''
        if not isinstance(node, CodedKern):
            raise TransformationError(
                f"Target of a {self.name} must be a sub-class of "
                f"psyGen.CodedKern but got '{type(node).__name__}'")

        # Check that the PSyIR and associated Symbol table of the Kernel is OK.
        # If this kernel contains symbols that are not captured in the PSyIR
        # SymbolTable then this raises an exception.
        try:
            kernel_schedule = node.get_kernel_schedule()
        except Exception as error:
            raise TransformationError(
                f"{self.name} failed to retrieve PSyIR for kernel "
                f"'{node.name}' using the 'get_kernel_schedule' method."
                ) from error

        # Check that all kernel symbols are declared in the kernel
        # symbol table(s). At this point they may be declared in a
        # container containing this kernel which is not supported.
        # TODO #1823: What about symbols not in References (e.g.
        # literal datatypes, parameters initializations, ...)
        # It could be more nuanced. Global parameters can be
        # brought into the inlined subroutine scope
        for var in kernel_schedule.walk(Reference):
            try:
                var.scope.symbol_table.lookup(
                    var.name, scope_limit=var.ancestor(Routine))
            except KeyError as err:
                raise TransformationError(
                    f"Kernel '{node.name}' contains accesses to data (variable"
                    f" '{var.name}') that are not present in the Symbol Table"
                    f"(s) within subroutine scope. Cannot inline such a"
                    f" kernel.") from err

        # CodeBlocks also have symbols that we need to check
        for block in kernel_schedule.walk(CodeBlock):
            for name in block.get_symbol_names():
                try:
                    block.scope.symbol_table.lookup(
                        name, scope_limit=block.ancestor(Routine))
                except KeyError as err:
                    raise TransformationError(
                        f"Kernel '{node.name}' contains accesses to data "
                        f"(variable '{name}' in a CodeBlock) that are not "
                        f"present in the Symbol Table(s) within subroutine "
                        f"scope. Cannot inline such a kernel.") from err

        # We can't transform subroutines that shadow top-level symbol module
        # names, because we won't be able to bring this into the subroutine
        for scope in kernel_schedule.walk(ScopingNode):
            for symbol in scope.symbol_table.symbols:
                symtab = kernel_schedule.ancestor(Container).symbol_table
                for mod in symtab.containersymbols:
                    if symbol.name == mod.name and not \
                            isinstance(symbol, ContainerSymbol):
                        raise TransformationError(
                            f"Kernel '{node.name}' cannot be module-inlined"
                            f" because the subroutine shadows the symbol "
                            f"name of the module container '{symbol.name}'.")

        # If the symbol already exist at the call site it must be referring
        # to a Routine
        try:
            existing_symbol = node.scope.symbol_table.lookup(node.name)
        except KeyError:
            existing_symbol = None
        if existing_symbol and not isinstance(existing_symbol, RoutineSymbol):
            raise TransformationError(
                f"Cannot module-inline subroutine '{node.name}' because "
                f"symbol '{existing_symbol}' with the same name already "
                f"exists and changing the name of module-inlined "
                f"subroutines is not supported yet.")

    @staticmethod
    def _prepare_code_to_inline(code_to_inline):
        ''' Prepare the PSyIR tree to inline by bringing in to the subroutine
        all referenced symbols so that the implementation is self contained.

        :param code_to_inline: the subroutine to module-inline.
        :type code_to_inline: :py:class:`psyclone.psyir.node.Routine`

        '''
        # pylint: disable=too-many-branches
        source_container = code_to_inline.ancestor(Container)

        # First make a set with all symbols used inside the subroutine
        all_symbols = set()
        for scope in code_to_inline.walk(ScopingNode):
            for symbol in scope.symbol_table.symbols:
                all_symbols.add(symbol)
        for reference in code_to_inline.walk(Reference):
            all_symbols.add(reference.symbol)
        for literal in code_to_inline.walk(Literal):
            # Literals may reference symbols in their precision
            if isinstance(literal.datatype.precision, Symbol):
                all_symbols.add(literal.datatype.precision)
        for caller in code_to_inline.walk(Call):
            all_symbols.add(caller.routine)

        # Then decide which symbols need to be brought inside the subroutine
        symbols_to_bring_in = set()
        for symbol in all_symbols:
            # TODO #1366: We still need a solution for intrinsics that
            # currently are parsed into Calls/RoutineSymbols, for the
            # moment here we skip the ones causing issues.
            if symbol.name in ("random_number", ) and symbol.is_unresolved:
                continue  # Skip intrinsic symbols
            if symbol.is_unresolved or symbol.is_import:
                # This symbol is already in the symbol table, but adding it
                # to the 'symbols_to_bring_in' will make the next step bring
                # into the subroutine all modules that it could come from.
                symbols_to_bring_in.add(symbol)
            if isinstance(symbol, DataSymbol):
                # DataTypes can reference other symbols
                if isinstance(symbol.datatype, DataTypeSymbol):
                    symbols_to_bring_in.add(symbol.datatype)
                elif hasattr(symbol.datatype, 'precision'):
                    if isinstance(symbol.datatype.precision, Symbol):
                        symbols_to_bring_in.add(symbol.datatype.precision)

        # Bring the selected symbols inside the subroutine
        for symbol in symbols_to_bring_in:
            if symbol.name not in code_to_inline.symbol_table:
                code_to_inline.symbol_table.add(symbol)
            # And when necessary the modules where they come from
            if symbol.is_unresolved:
                # We don't know where this comes from, we need to bring
                # in all top-level imports with wildcard imports
                for mod in source_container.symbol_table.containersymbols:
                    if mod.wildcard_import:
                        if mod.name not in code_to_inline.symbol_table:
                            code_to_inline.symbol_table.add(mod)
                        else:
                            code_to_inline.symbol_table.lookup(mod.name).\
                                wildcard_import = True
            elif symbol.is_import:
                module_symbol = symbol.interface.container_symbol
                if module_symbol.name not in code_to_inline.symbol_table:
                    code_to_inline.symbol_table.add(module_symbol)
                else:
                    # If it already exists, we know its a container (from the
                    # validation) so we just need to point to it
                    symbol.interface._container_symbol = \
                        code_to_inline.symbol_table.lookup(module_symbol.name)

    def apply(self, node, options=None):
        ''' Bring the kernel subroutine into this Container.

        :param node: the kernel to module-inline.
        :type node: :py:class:`psyclone.psyGen.CodedKern`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)

        if not options:
            options = {}

        name = node.name
        try:
            existing_symbol = node.scope.symbol_table.lookup(name)
        except KeyError:
            existing_symbol = None

        code_to_inline = node.get_kernel_schedule()
        self._prepare_code_to_inline(code_to_inline)

        if not existing_symbol:
            # If it doesn't exist already, module-inline the subroutine by:
            # 1) Registering the subroutine symbol in the Container
            node.ancestor(Container).symbol_table.add(RoutineSymbol(name))
            # 2) Insert the relevant code into the tree.
            node.ancestor(Container).addchild(code_to_inline.detach())
        else:
            # The routine symbol already exist, and we know from the validation
            # that its a Routine. Now check if they are exactly the same.
            for routine in node.ancestor(Container).walk(Routine,
                                                         stop_type=Routine):
                if routine.name == node.name:
                    # This TransformationError happens here and not in the
                    # validation because it needs the symbols_to_bring_in
                    # applied to effectively compare both versions
                    # This will be fixed when module-inlining versioning is
                    # implemented.
                    if routine != code_to_inline:
                        raise TransformationError(
                            f"Cannot inline subroutine '{node.name}' because "
                            f"another, different, subroutine with the same "
                            f"name already exists and versioning of module-"
                            f"inlined subroutines is not implemented yet.")

        # Set the module-inline flag to avoid generating the kernel imports
        # TODO #1823. If the kernel imports where generated at PSy-layer
        # creation time, we could just remove it here instead of setting a
        # flag.
        node.module_inline = True
