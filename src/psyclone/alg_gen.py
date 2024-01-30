# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2014-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''This module provides the Alg class and supporting
exception-handling to translate the original algorithm file into one
that can be compiled and linked with the generated PSy code.

'''

# fparser contains classes that are generated at run time.
# pylint: disable=no-name-in-module
from fparser.two.Fortran2003 import (Main_Program, Module, Use_Stmt, Part_Ref,
                                     Subroutine_Subprogram, Call_Stmt, Name,
                                     Section_Subscript_List, Only_List,
                                     Function_Subprogram, Specification_Part)
# pylint: enable=no-name-in-module
from fparser.two.utils import Base, walk

from psyclone.errors import GenerationError, InternalError, PSycloneError


class NoInvokesError(PSycloneError):
    '''Provides a PSyclone-specific error class for the situation when an
    algorithm code contains no invoke calls.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "Algorithm Error: "+str(value)


# pylint: disable=too-few-public-methods
class Alg:
    '''Generate a modified algorithm code for a single algorithm
    specification. Takes the parse tree of the algorithm specification
    output from the function :func:`psyclone.parse.algorithm.parse`
    and an instance of the :class:`psyGen.PSy` class as input. The
    latter allows consistent names to be generated between the
    algorithm (callng) and psy (callee) layers.

    For example:

    >>> from psyclone.algorithm.parse import parse
    >>> parse_tree, info = parse("argspec.F90")
    >>> from psyclone.psyGen import PSy
    >>> psy = PSy(info)
    >>> from psyclone.alg_gen import Alg
    >>> alg = Alg(parse_tree, psy)
    >>> print(alg.gen)

    :param parse_tree: an object containing a parse tree of the \
        algorithm specification which was produced by the function \
        :func:`psyclone.parse.algorithm.parse`. Assumes the algorithm \
        will be parsed by fparser2 and expects a valid program unit, \
        program, module, subroutine or function.
    :type parse_tree: :py:class:`fparser.two.utils.Base`
    :param psy: an object containing information about the PSy layer.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :param str invoke_name: the name that the algorithm layer uses to \
        indicate an invoke call. This is an optional argument that \
        defaults to the name "invoke".

    '''

    def __init__(self, parse_tree, psy, invoke_name="invoke"):
        self._ast = parse_tree
        self._psy = psy
        self._invoke_name = invoke_name

    @property
    def gen(self):
        '''
        Modifies and returns the algorithm code. 'invoke' calls are replaced
        with calls to the corresponding PSy-layer routines and the USE
        statements for the kernels that were referenced by each 'invoke' are
        removed.

        :returns: the modified algorithm specification as an fparser2 \
                  parse tree.
        :rtype: :py:class:`fparser.two.utils.Base`

        :raises NoInvokesError: if no 'invoke()' calls are found.

        '''
        invoked_kernels = set()
        idx = 0
        # Walk through all statements looking for procedure calls
        for statement in walk(self._ast.content, Call_Stmt):
            # found a Fortran call statement
            call_name = str(statement.items[0])
            if call_name.lower() == self._invoke_name.lower():
                # The call statement is an invoke

                # Work out which kernels are involved so that we can update the
                # USE statements later.
                arg_spec_list = statement.children[1]
                for child in arg_spec_list.children:
                    # An invoke() can include a 'name=xxx' argument so we have
                    # to check that we have a Part_Ref
                    if isinstance(child, Part_Ref):
                        invoked_kernels.add(child.children[0].tostr().lower())

                # Get the PSy callee name and argument list and
                # replace the existing algorithm invoke call with
                # these.
                psy_invoke_info = self._psy.invokes.invoke_list[idx]
                new_name = psy_invoke_info.name
                new_args = Section_Subscript_List(
                    ", ".join(psy_invoke_info.alg_unique_args))
                statement.items = (new_name, new_args)

                # The PSy-layer generates a subroutine within a module
                # so we need to add a 'use module_name, only :
                # subroutine_name' to the algorithm layer.
                _adduse(statement, self._psy.name, only=True,
                        funcnames=[psy_invoke_info.name])
                idx += 1

        if idx == 0:
            raise NoInvokesError(
                "Algorithm file contains no invoke() calls: refusing to "
                "generate empty PSy code")

        # Remove any un-needed imports of the kernels referenced by the removed
        # invoke() calls.
        _rm_kernel_use_stmts(invoked_kernels, self._ast)

        return self._ast


def _rm_kernel_use_stmts(kernels, ptree):
    '''
    Remove any unneeded imports of the named kernels from the supplied fparser2
    parse tree.

    :param Set[str] kernels: the names of the kernels that are no longer \
        invoked.
    :param ptree: the fparser2 parse tree to update.
    :type ptree: :py:class:`fparser.two.Fortran2003.Program`

    '''
    # Setup the various lookup tables that we'll need.
    # Map from the module name to the associated Use_Stmt in the
    # parse tree.
    use_stmt_map = {}
    # Map from module name to the list of symbols imported from it.
    use_only_list = {}
    # Map from symbol name to the name of the module from which it is
    # imported.
    sym_to_mod_map = {}

    for use_stmt in walk(ptree, Use_Stmt):
        mod_name = use_stmt.children[2].tostr().lower()
        use_stmt_map[mod_name] = use_stmt
        use_only_list[mod_name] = None
        only = walk(use_stmt, Only_List)
        if only:
            use_only_list[mod_name] = []
            for child in only[0].children:
                sym_name = child.tostr().lower()
                use_only_list[mod_name].append(sym_name)
                sym_to_mod_map[sym_name] = mod_name

    # Remove the USE statements for the invoked kernels provided that
    # they're not referenced anywhere (apart from USE statements).
    all_other_names = set(name.tostr().lower() for name in
                          walk(ptree.children, Name)
                          if not isinstance(name.parent, Only_List))
    # Update the lists of symbols imported from each module
    for kern in kernels:
        if kern not in all_other_names and kern in sym_to_mod_map:
            # Kernel name is not referenced anywhere but is imported from
            # a module (so is not a Built-In).
            mod_name = sym_to_mod_map[kern]
            use_only_list[mod_name].remove(kern)

    # Finally remove those USE statements that used to have symbols
    # associated with them but now have none.
    for mod, symbols in use_only_list.items():
        if symbols == []:
            this_use = use_stmt_map[mod]
            spec_part = this_use.parent
            spec_part.children.remove(this_use)
            # fparser currently falls over when asked to create Fortran for an
            # empty Specification_Part (fparser/#359). We therefore remove the
            # modified Specification_Part entirely if it is now empty.
            if not spec_part.children:
                spec_part.parent.children.remove(spec_part)


def _adduse(location, name, only=None, funcnames=None):
    '''Add a Fortran 'use' statement to an existing fparser2 parse
    tree. This will be added at the first valid location before the
    current location.

    This function should be part of the fparser2 replacement for
    f2pygen (which uses fparser1) but is kept here until this is
    developed, see issue #240.

    :param location: the current location (node) in the parse tree to which \
                     to add a USE.
    :type location: :py:class:`fparser.two.utils.Base`
    :param str name: the name of the use statement.
    :param bool only: whether to include the 'only' clause in the use \
        statement or not. Defaults to None which will result in only being \
        added if funcnames has content and not being added otherwise.
    :param funcnames: a list of names to include in the use statement's \
        only list. If the list is empty or None then nothing is \
        added. Defaults to None.
    :type funcnames: list of str

    :raises GenerationError: if no suitable enclosing program unit is found \
                             for the provided location.
    :raises NotImplementedError: if the type of parent node is not supported.
    :raises InternalError: if the parent node does not have the expected \
                           structure.
    '''
    # pylint: disable=too-many-locals
    # pylint: disable=too-many-branches
    if not isinstance(location, Base):
        raise GenerationError(
            f"alg_gen.py:_adduse: Location argument must be a sub-class of "
            f"fparser.two.utils.Base but got: {type(location).__name__}.")

    if funcnames:
        # funcnames have been provided for the only clause.
        if only is False:
            # However, the only clause has been explicitly set to False.
            raise GenerationError(
                "alg_gen.py:_adduse: If the 'funcnames' argument is provided "
                "and has content, then the 'only' argument must not be set "
                "to 'False'.")
        if only is None:
            # only has not been specified so set it to True as it is
            # required when funcnames has content.
            only = True

    if only is None:
        # only has not been specified and we can therefore infer that
        # funcnames is empty or is not provided (as earlier code would
        # have set only to True otherwise) so only is not required.
        only = False

    # Create the specified use statement
    only_str = ""
    if only:
        only_str = ", only :"
    my_funcnames = funcnames
    if funcnames is None:
        my_funcnames = []
    use = Use_Stmt(f"use {name}{only_str} {', '.join(my_funcnames)}")

    # find the parent program statement containing the specified location
    parent_prog_statement = None
    current = location
    while current:
        if isinstance(current, (Main_Program, Module, Subroutine_Subprogram,
                                Function_Subprogram)):
            parent_prog_statement = current
            break
        current = current.parent
    else:
        raise GenerationError(
            "The specified location is invalid as it has no parent in the "
            "parse tree that is a program, module, subroutine or function.")

    if not isinstance(parent_prog_statement,
                      (Main_Program, Subroutine_Subprogram,
                       Function_Subprogram)):
        # We currently only support program, subroutine and function
        # as ancestors
        raise NotImplementedError(
            f"alg_gen.py:_adduse: Unsupported parent code found "
            f"'{type(parent_prog_statement)}'. Currently support is limited "
            f"to program, subroutine and function.")
    if not isinstance(parent_prog_statement.content[1], Specification_Part):
        raise InternalError(
            f"alg_gen.py:_adduse: The second child of the parent code "
            f"(content[1]) is expected to be a specification part but "
            f"found '{repr(parent_prog_statement.content[1])}'.")

    # add the use statement as the first child of the specification
    # part of the program
    spec_part = parent_prog_statement.content[1]
    spec_part.content.insert(0, use)


# For auto-API documentation generation.
__all__ = ["NoInvokesError", "Alg"]
