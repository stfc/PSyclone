# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

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

    def __init__(self, ast, psy):
        self._ast = ast
        self._psy = psy

    @property
    def gen(self):
        '''
        Generate modified algorithm code

        :rtype: ast

        '''
        from fparser import api
        from psyclone.f2pygen import adduse
        psy_name = self._psy.name
        # run through all statements looking for procedure calls
        idx = 0
        for stmt, _ in api.walk(self._ast, -1):

            if isinstance(stmt, fparser.one.statements.Call):
                if stmt.designator == "invoke":
                    invoke_info = self._psy.invokes.invoke_list[idx]
                    stmt.designator = invoke_info.name
                    stmt.items = invoke_info.alg_unique_args
                    adduse(psy_name, stmt.parent, only=True,
                           funcnames=[invoke_info.name])
                    idx += 1

        if idx == 0:
            raise NoInvokesError(
                "Algorithm file contains no invoke() calls: refusing to "
                "generate empty PSy code")

        return self._ast
