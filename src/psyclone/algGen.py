# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2014-2019, Science and Technology Facilities Council.
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

import fparser


class NoInvokesError(Exception):
    '''Provides a PSyclone-specific error class for the situation when an
    algorithm code contains no invoke calls.'''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Algorithm Error: "+value

    def __str__(self):
        return repr(self.value)


class Alg(object):
    '''Generate a modified algorithm code for a single algorithm
    specification. Takes the ast of the algorithm specification output
    from the function :func:`parse.parse` and an instance of the
    :class:`psyGen.PSy` class as input.

    :param ast ast: An object containing an ast of the algorithm
        specification which was produced by the function
        :func:`parse.parse`.

    :param PSy psy: An object (:class:`psyGen.PSy`) containing
        information about the PSy layer.

    For example:

    >>> from parse import parse
    >>> ast,info=parse("argspec.F90")
    >>> from psyGen import PSy
    >>> psy=PSy(info)
    >>> from algGen import Alg
    >>> alg=Alg(ast,psy)
    >>> print(alg.gen)

    '''

    def __init__(self, ast, psy, invoke_name="invoke"):
        self._ast = ast
        self._psy = psy
        self._invoke_name = invoke_name

    @property
    def gen(self):
        '''
        Generate modified algorithm code

        :rtype: ast

        '''
        # run through all statements looking for procedure calls
        idx = 0
        from fparser.two.utils import walk_ast
        from fparser.two.Fortran2003 import Call_Stmt, Section_Subscript_List

        for statement in walk_ast(self._ast.content):

            if isinstance(statement, Call_Stmt):
                # found a Fortran call statement
                call_name = str(statement.items[0])
                if call_name.lower() == self._invoke_name.lower():
                    # The call statement is an invoke
                    invoke_info = self._psy.invokes.invoke_list[idx]
                    new_name = invoke_info.name
                    new_args = Section_Subscript_List(
                        ", ".join(invoke_info.alg_unique_args))
                    statement.items = (new_name, new_args)
                    adduse(self._ast, self._psy.name, only=True,
                           funcnames=[invoke_info.name])
                    idx += 1

        if idx == 0:
            raise NoInvokesError(
                "Algorithm file contains no invoke() calls: refusing to "
                "generate empty PSy code")

        return self._ast


def adduse(ast, name, only=False, funcnames=None):
    ''' xxx '''
    from fparser.two.utils import walk_ast
    from fparser.two.Fortran2003 import Main_Program, Module, \
        Subroutine_Subprogram, Function_Subprogram, Use_Stmt, \
        Call_Stmt, Actual_Arg_Spec_List, Actual_Arg_Spec, Part_Ref, \
        Specification_Part

    use = Use_Stmt("use {0}, only : {1}".format(name, ", ".join(funcnames)))
    # find first program statement (top down)
    for child in ast.content:
        if isinstance(child, (Main_Program, Module, Subroutine_Subprogram,
                              Function_Subprogram)):
            if isinstance(child, (Main_Program, Subroutine_Subprogram)):
                program = child
                if not isinstance(program.content[1], Specification_Part):
                    raise NotImplementedError("Main program content 1 is expected to be a specification part but found '{0}'".format(program.content[1]))
                spec_part = program.content[1]
                spec_part.content.insert(0, use)
            else:
                raise NotImplementedError("Unsupported code unit found '{0}'".format(str(type(child))))
    return ast
