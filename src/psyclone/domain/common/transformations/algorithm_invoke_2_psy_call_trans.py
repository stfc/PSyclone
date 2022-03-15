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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab.

''' Transform a PSyclone algorithm-layer-specific invoke call into a call
to the corresponding PSy-layer routine.

'''

import re

from psyclone.core import SymbolicMaths
from psyclone.psyir.nodes import (Call, ArrayReference, CodeBlock, Routine,
                                  Container, FileContainer, Reference, Literal)
from psyclone.psyir.symbols import (RoutineSymbol, ContainerSymbol,
                                    ImportInterface)
from psyclone.domain.common.algorithm import AlgorithmInvokeCall, \
    KernelFunctor
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


class AlgorithmInvoke2PSyCallTrans(Transformation):
    '''XXX.

    '''
    def validate(self, node, options=None):
        '''Validate the node argument.

        :param node: a PSyIR node capturing an invoke call.
        :type node: \
            :py:class:`psyclone.domain.common.algorithm.AlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied call argument is \
            not a PSyIR Call node.
        :raises TransformationError: if the supplied call argument \
            does not have the expected name which would identify it as an \
            invoke call.
        :raises TransformationError: if the invoke arguments are not a \
            PSyIR ArrayReference or CodeBlock.

        '''
        self._call_name = None

        if not isinstance(node, AlgorithmInvokeCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be an `AlgorithmInvokeCall` node but found "
                f"'{type(node).__name__}'.")

    def _def_routine_root_name(self, call):
        '''Internal method that returns the proposed PSy-layer routine
        name given the index of this invoke.

        :returns: the proposed processed routine name for this invoke.
        :rtype: str

        :raises TypeError: if the name is not valid.

        '''
        if call._name:
            routine_root_name = call._name.lower().strip()
            if routine_root_name[0] == '"' and routine_root_name[-1] == '"' \
               or \
               routine_root_name[0] == "'" and routine_root_name[-1] == "'":
                # fparser2 (issue #295) currently includes quotes as
                # part of a string, so strip them out.
                routine_root_name = routine_root_name[1:-1].strip()
            routine_root_name = routine_root_name.replace(" ", "_")
            # Check that the name is a valid routine name
            pattern = re.compile(r"^[a-zA-Z]\w*$", re.ASCII)
            if not pattern.match(routine_root_name):
                raise TypeError(
                    f"AlgorithmInvokeCall:_def_routine_root_name() the "
                    f"(optional) name of an invoke must be a string "
                    f"containing a valid name (with any spaces replaced by "
                    f"underscores) but found '{routine_root_name}'.")
            if not routine_root_name.startswith("invoke"):
                routine_root_name = f"invoke_{routine_root_name}"
        else:
            routine_root_name = f"invoke_{self._index}"
            if len(call.children) == 1:
                # Add the name of the kernel if there is only one call
                routine_root_name += "_" + call.children[0].name
        return routine_root_name

    def create_psylayer_symbol_root_names(self, call):
        '''If the PSy-layer routine and container root names have not been
        created, then create them. The invoke root name is based on
        the position of this node (compared to other nodes of the same
        type) in the PSyIR tree. Note, we do not create and store
        symbols, as the container name needs to be consistent between
        different invoke calls and we have no way of knowing whether
        one has already been created without the symbol being stored
        in the symbol table, and we don't want to add anything related
        to the lower level PSyIR to the symbol table before lowering.

        :raises InternalError: if no Routine or Container is found in \
            the PSyIR tree containing this node.

        '''
        if not call._psylayer_routine_root_name:
            call._psylayer_routine_root_name = call._def_routine_root_name()

        if not call._psylayer_container_root_name:
            # The PSy-layer module naming logic (in algorithm.py) finds
            # the first program, module, subroutine or function in the
            # parse tree and uses that name for the container name. Here
            # we temporarily replicate this functionality. Eventually we
            # will merge. Note, a better future solution could be to use
            # the closest ancestor routine instead.
            for node in call.root.walk((Routine, Container)):
                if not isinstance(node, FileContainer):
                    call._psylayer_container_root_name = \
                        call._def_container_root_name(node)
                    return
            raise InternalError("No Routine or Container node found.")

    def apply(self, call, index, options=None):
        ''' Apply the transformation to the supplied node.

        :param call: a PSyIR call node capturing an invoke call in \
            generic PSyIR.
        :type call: :py:class:`psyclone.psyir.nodes.Call`
        :param int index: the position of this invoke call relative to \
            other invokes in the algorithm layer.
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(call, options=options)

        self.create_psylayer_symbol_root_names(call)

        arguments = []
        sym_maths = SymbolicMaths.get()
        for kern in self.children:
            for arg in kern.children:
                if isinstance(arg, Literal):
                    # Literals are not passed by argument.
                    pass
                elif isinstance(arg, (Reference, ArrayReference)):
                    for existing_arg in arguments:
                        if sym_maths.equal(arg, existing_arg):
                            break
                    else:
                        arguments.append(arg.copy())
                else:
                    raise GenerationError(
                        f"Expected Algorithm-layer kernel arguments to be "
                        f"a literal, reference or array reference, but "
                        f"found '{type(arg).__name__}'.")

        symbol_table = self.scope.symbol_table

        # TODO #753. At the moment the container and routine names
        # produced here will differ from the PSy-layer routine name if
        # there is a name clash in the algorithm layer.
        container_tag = self._psylayer_container_root_name
        try:
            container_symbol = symbol_table.lookup_with_tag(container_tag)
        except KeyError:
            container_symbol = symbol_table.new_symbol(
                root_name=container_tag, tag=container_tag,
                symbol_type=ContainerSymbol)

        routine_tag = self._psylayer_routine_root_name
        interface = ImportInterface(container_symbol)
        routine_symbol = symbol_table.new_symbol(
            root_name=routine_tag, tag=routine_tag, symbol_type=RoutineSymbol,
            interface=interface)

        call = Call.create(routine_symbol, arguments)
        self.replace_with(call)

        # Remove original 'invoke' symbol if there are no other
        # references to it. This keeps the symbol table up-to-date and
        # also avoids an exception being raised in the Fortran Writer
        # as the invoke symbol has an UnresolvedInterface.

        symbol_table = call.scope.symbol_table
        while symbol_table:
            try:
                invoke_symbol = symbol_table.lookup(
                    "invoke", scope_limit=symbol_table.node)
            except KeyError:
                symbol_table = symbol_table.parent_symbol_table()
                continue
            if not symbol_table.node.walk(AlgorithmInvokeCall):
                symbol_table.remove(invoke_symbol)
            break
        else:
            raise InternalError("No 'invoke' symbol found.")


__all__ = ['AlgorithmInvoke2PSyCallTrans']
