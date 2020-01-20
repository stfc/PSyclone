# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2014-2020, Science and Technology Facilities Council.
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

from __future__ import absolute_import


class NoInvokesError(Exception):
    '''Provides a PSyclone-specific error class for the situation when an
    algorithm code contains no invoke calls.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Algorithm Error: "+value

    def __str__(self):
        return repr(self.value)


# pylint: disable=too-few-public-methods
class Alg(object):
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
        '''Return modified algorithm code.

        :returns: the modified algorithm specification as an fparser2 \
                  parse tree.
        :rtype: :py:class:`fparser.two.utils.Base`

        '''

        from fparser.two.utils import walk
        # pylint: disable=no-name-in-module
        from fparser.two.Fortran2003 import Call_Stmt, Section_Subscript_List

        idx = 0
        # Walk through all statements looking for procedure calls
        for statement in walk(self._ast.content, Call_Stmt):
            # found a Fortran call statement
            call_name = str(statement.items[0])
            if call_name.lower() == self._invoke_name.lower():
                # The call statement is an invoke

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
                adduse(statement, self._psy.name, only=True,
                       funcnames=[psy_invoke_info.name])
                idx += 1

        if idx == 0:
            raise NoInvokesError(
                "Algorithm file contains no invoke() calls: refusing to "
                "generate empty PSy code")

        return self._ast


def adduse(location, name, only=None, funcnames=None):
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
    from fparser.two.Fortran2003 import Main_Program, Module, \
        Subroutine_Subprogram, Function_Subprogram, Use_Stmt, \
        Specification_Part
    from fparser.two.utils import Base
    from psyclone.errors import GenerationError, InternalError

    if not isinstance(location, Base):
        raise GenerationError(
            "alg_gen.py:adduse: Location argument must be a sub-class of "
            "fparser.two.utils.Base but got: {0}.".format(
                type(location).__name__))

    if funcnames:
        # funcnames have been provided for the only clause.
        if only is False:
            # However, the only clause has been explicitly set to False.
            raise GenerationError(
                "alg_gen.py:adduse: If the 'funcnames' argument is provided "
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
    use = Use_Stmt("use {0}{1} {2}".format(name, only_str,
                                           ", ".join(my_funcnames)))

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
            "alg_gen.py:adduse: Unsupported parent code found '{0}'. "
            "Currently support is limited to program, subroutine and "
            "function.".format(str(type(parent_prog_statement))))
    if not isinstance(parent_prog_statement.content[1], Specification_Part):
        raise InternalError(
            "alg_gen.py:adduse: The second child of the parent code "
            "(content[1]) is expected to be a specification part but "
            "found '{0}'.".format(repr(parent_prog_statement.content[1])))

    # add the use statement as the first child of the specification
    # part of the program
    spec_part = parent_prog_statement.content[1]
    spec_part.content.insert(0, use)
