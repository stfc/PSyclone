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


from psyclone.errors import InternalError
from psyclone.psyGen import Transformation, Kern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import RoutineSymbol, ContainerSymbol
from psyclone.psyir.nodes import Container, ScopingNode, Reference, Routine


class KernelModuleInlineTrans(Transformation):
    ''' Module-inlines (bring the subroutine to the same compiler-unit) the
    subroutine pointed by this Kernel. For example:

    >>> invoke = ...
    >>> schedule = invoke.schedule
    >>>
    >>> inline_trans = KernelModuleInlineTrans()
    >>>
    >>> inline_trans.apply(schedule.children[0].loop_body[0])
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    .. warning ::
        For this transformation to work correctly, the Kernel subroutine
        must only use data that is passed in by argument, declared locally
        or included via use association within the subroutine. Two
        examples where in-lining will not work are:

        #. A variable is declared within the module that ``contains`` the
           Kernel subroutine and is then accessed within that Kernel;
        #. A variable is included via use association at the module level
           and accessed within the Kernel subroutine.

        The transformation will reject attempts to in-line such kernels.
    '''

    def __str__(self):
        return "Inline a kernel subroutine into the PSy module"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "KernelModuleInline"

    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a Kernel and that it is possible to
        inline the PSyIR of its contents.

        :param kern: the kernel which is the target of the transformation.
        :type kern: :py:class:`psyclone.psyGen.Kern` or sub-class
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the target node is not a sub-class of \
                                     psyGen.Kern.
        :raises TransformationError: if the subroutine containing the \
                                     implementation of the kernel cannot be \
                                     retrieved wiht 'get_kernel_schedule'.
        :raises TransformationError: if the kernel cannot be safely inlined.

        '''

        if not isinstance(node, Kern):
            raise TransformationError(
                f"Target of a kernel transformation must be a sub-class of "
                f"psyGen.Kern but got '{type(node).__name__}'")

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
        for var in kernel_schedule.walk(Reference):
            try:
                var.scope.symbol_table.lookup(
                    var.name, scope_limit=var.ancestor(Routine))
            except KeyError as err:
                raise TransformationError(
                    f"Kernel '{node.name}' contains accesses to data (variable"
                    f" '{var.name}') that are not present in the Symbol Table"
                    f"(s) within KernelSchedule scope. Cannot inline such a"
                    f" kernel.") from err

        # If the symbol already exist it must be referring to a Routine
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
    def _prepare_code_to_inline(node):
        ''' Prepare the PSyIR tree to inline by brining in the subroutine all
        referenced symbols so that the implementation is self contained.

        :param node: the kernel to module-inline.
        :type node: :py:class:`psyclone.psyGen.CodedKern`

        :returns: a self contained version of the subroutine to inline.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine`

        :raise InternalError: unexpected PSyIR.

        '''
        code_to_inline = node.get_kernel_schedule()
        source_container = code_to_inline.ancestor(Container)
        symbols_to_bring_in = set()

        for scope in code_to_inline.walk(ScopingNode):
            for symbol in scope.symbol_table.symbols:
                if symbol.is_unresolved:
                    # We don't know where this comes from, we need to bring
                    # in all top-level imports
                    for mod in source_container.symbol_table.containersymbols:
                        symbols_to_bring_in.add(mod)
                elif symbol.is_import:
                    pass  # Add to bring in if imported outside?
                elif symbol.is_local:
                    # This should be on the validate
                    # Ok if is a constant, otherwise it should be an
                    # # error?
                    pass

        for symbol in symbols_to_bring_in:
            if symbol.name in code_to_inline.symbol_table:
                same_symbol = code_to_inline.symbol_table.lookup(symbol.name)
                if not isinstance(same_symbol, ContainerSymbol):
                    raise InternalError(
                        f"Incoherent PSyIR found ({symbol.name} not as "
                        f"expected)")
            else:
                code_to_inline.symbol_table.add(symbol)
                # Note that momentarily we have the same symbol in two
                # symbol tables, but this is not a problem because below
                # we detach and discard the ancestor part of the tree.
        return code_to_inline

    def apply(self, node, options=None):
        ''' Bring the kernel subroutine in this Container.

        :param node: the kernel to module-inline.
        :type node: :py:class:`psyclone.psyGen.CodedKern`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node, options)

        if not options:
            options = {}

        name = node.name
        try:
            existing_symbol = node.scope.symbol_table.lookup(name)
        except KeyError:
            existing_symbol = None

        code_to_inline = self._prepare_code_to_inline(node)

        if not existing_symbol:
            # If it doesn't exist already, module-inline the subroutine by:
            # 1) Registering the subroutine symbol in the Container
            node.ancestor(Container).symbol_table.add(RoutineSymbol(name))
            # 2) Insert the relevant code into the tree.
            node.root.addchild(code_to_inline.detach())
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

        # Once module-inlined, all kernelcalls to the same kernel in the same
        # invoke use the inlined implementation
        node.module_inline = True
