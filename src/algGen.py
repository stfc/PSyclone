# Copyright 2013 STFC, all rights reserved
import fparser

class Alg(object):
  '''
  Generate a modified algorithm code for a single algorithm specification. Takes the ast of the algorithm specification output from the function :func:`parse.parse` and an instance of the :class:`psyGen.PSy` class as input.

  :param ast ast: An object containing an ast of the algorithm specification which was produced by the function :func:`parse.parse`.
  :param PSy psy: An object (:class:`psyGen.PSy`) containing information about the PSy layer.

  For example:

  >>> from parse import parse
  >>> ast,info=parse("argspec.F90")
  >>> from psyGen import PSy
  >>> psy=PSy(info)
  >>> from algGen import Alg
  >>> alg=Alg(ast,psy)
  >>> print(alg.gen)

  '''
  def __init__(self,ast,psy):
    self._ast=ast
    self._psy=psy

  @property
  def gen(self):
    '''
    Generate modified algorithm code

    :rtype: ast

    '''
    from fparser import api
    from f2pygen import adduse
    psyName=self._psy.name
    # run through all statements looking for procedure calls
    idx=0
    for stmt, depth in api.walk(self._ast, -1):

      if isinstance(stmt,fparser.statements.Call):
        if stmt.designator=="invoke":
          from psyGen import Invoke
          invokeInfo=self._psy.invokes.invoke_list[idx]
          stmt.designator=invokeInfo.name
          stmt.items=invokeInfo.orig_unique_args
          adduse(psyName,stmt.parent,only=True,funcnames=[invokeInfo.name])
          idx+=1
    return self._ast


