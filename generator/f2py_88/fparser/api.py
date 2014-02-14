"""Public API for Fortran parser.

Module content
--------------
"""
#Author: Pearu Peterson <pearu@cens.ioc.ee>
#Created: Oct 2006

__autodoc__ = ['get_reader', 'parse', 'walk']

import Fortran2003
# import all Statement classes:
from base_classes import EndStatement, classes
from block_statements import *

# CHAR_BIT is used to convert object bit sizes to byte sizes
from utils import CHAR_BIT

def get_reader(input, isfree=None, isstrict=None, include_dirs = None, source_only = None,
               ignore_comments = True):
    """ Returns Fortran reader instance.

    Parameters
    ----------
    input : str
      Specify a string or filename containing Fortran code.    
    isfree, isstrict : {None, bool}
      Specify input Fortran format. The values are determined from the
      input. If that fails then isfree=True and isstrict=False is assumed.
    include_dirs : {None, list}
      Specify a list of include directories. The default list (when
      include_dirs=None) contains the current working directory and
      the directory of ``filename``.
    source_only : {None, list}
      Specify a list of Fortran file names that are searched when the
      ``USE`` statement is encountered.

    Returns
    -------
    reader : `FortranReader`

    Notes
    -----
    If ``input`` is a C filename then the functions searches for comment
    lines starting with ``/*f2py`` and reads following lines as PYF file
    content until a line ``*/`` is found.

    See also
    --------
    parse
    """
    import os
    import re
    from readfortran import FortranFileReader, FortranStringReader
    if os.path.isfile(input):
        name,ext = os.path.splitext(input)
        if ext.lower() in ['.c']:
            # get signatures from C file comments starting with `/*f2py` and ending with `*/`.
            # TODO: improve parser to take line number offset making line numbers in
            #       parser messages correct.
            f2py_c_comments = re.compile('/[*]\s*f2py\s.*[*]/',re.I | re.M)
            f = open(filename,'r')
            c_input = ''
            for s1 in f2py_c_comments.findall(f.read()):
                c_input += s1[2:-2].lstrip()[4:] + '\n'
            f.close()
            if isfree is None: isfree = True
            if isstrict is None: isstrict = True
            return parse(c_input, isfree, isstrict, include_dirs)
        reader = FortranFileReader(input, include_dirs = include_dirs, source_only = source_only)
    elif isinstance(input, str):
        reader = FortranStringReader(input, include_dirs = include_dirs, source_only = source_only)
    else:
        raise TypeError,'Expected string or filename input but got %s' % (type(input))
    if isfree is None: isfree = reader.isfree
    if isstrict is None: isstrict = reader.isstrict
    reader.set_mode(isfree, isstrict)
    return reader

def parse(input, isfree=None, isstrict=None, include_dirs = None, source_only = None,
          ignore_comments = True, analyze=True):
    """ Parse input and return Statement tree.

    Parameters
    ----------
    input : str
      Specify a string or filename containing Fortran code.
    isfree, isstrict : {None, bool}
      Specify input Fortran format. The values are determined from the
      input. If that fails then isfree=True and isstrict=False is assumed.
    include_dirs : {None, list}
      Specify a list of include directories. The default list (when
      include_dirs=None) contains the current working directory and
      the directory of ``filename``.
    source_only : {None, list}
      Specify a list of Fortran file names that are searched when the
      ``USE`` statement is encountered.
    ignore_comments : bool
      When True then discard all comment lines in the Fortran code.
    analyze : bool
      When True then apply run analyze method on the Fortran code tree.

    Returns
    -------
    block : `fparser.api.BeginSource`

    Examples
    --------

        >>> code = '''
        ... c comment
        ...       subroutine foo(a)
        ...         integer a
        ...         print*, "a=",a
        ...       end
        ... '''
        >>> tree = parse(code,isfree=False)
        >>> print tree
              !BEGINSOURCE <cStringIO.StringI object at 0x1798030> mode=fix90
                SUBROUTINE foo(a)
                  INTEGER a
                  PRINT *, "a=", a
                END SUBROUTINE foo
        >>> print `tree`
              BeginSource
                blocktype='beginsource'
                name='<cStringIO.StringI object at 0x1798030> mode=fix90'
                a=AttributeHolder:
              external_subprogram=<dict with keys ['foo']>
                content:
                  Subroutine
                    args=['a']
                    item=Line('subroutine foo(a)',(3, 3),'')
                    a=AttributeHolder:
                variables=<dict with keys ['a']>
                    content:
                      Integer
                        selector=('', '')
                        entity_decls=['a']
                        item=Line('integer a',(4, 4),'')
                      Print
                        item=Line('print*, "a=",a',(5, 5),'')
                  EndSubroutine
                    blocktype='subroutine'
                    name='foo'
            item=Line('end',(6, 6),'')

    See also
    --------
    get_reader
    """
    from parsefortran import FortranParser
    reader = get_reader(input, isfree, isstrict, include_dirs, source_only)
    parser = FortranParser(reader, ignore_comments = ignore_comments)
    parser.parse()
    if analyze:
        parser.analyze()
    return parser.block

def walk(stmt, depth=-1, _initial_depth = None):
    """ Generate Fortran statements by walking the stmt tree until given depth.

    For each block statement in stmt, the walk functions yields a
    tuple ``(statement, depth)`` where ``depth`` is the depth of tree
    stucture for statement.

    Parameters
    ----------
    stmt : Statement
    depth : int
      If depth is positive then walk in the tree until given depth.
      If depth is negative then walk the whole tree.

    Returns
    -------
    generator

    Examples
    --------

      ::

        from fparser import api
        source_str = '''
        subroutine foo
          integer i, r
          do i=1,100
            r = r + i
          end do
        end
        '''
        tree = api.parse(source_str)
        for stmt, depth in api.walk(tree):
            print depth, stmt.item

      that will print::

        1 line #2'subroutine foo'
        2 line #3'integer i, r'
        2 line #4'do i=1,100'
        3 line #5'r = r + i'
        2 line #6'end do'
        1 line #7'end'

    """
    if _initial_depth is None:
        if depth==0:
            return
        _initial_depth = depth
    if not isinstance(stmt, classes.BeginSource):
        yield stmt, _initial_depth - depth
    if isinstance(stmt, classes.BeginStatement):
        last_stmt = stmt.content[-1]
        last_index = len(stmt.content)
        if isinstance(last_stmt, classes.EndStatement):
            last_index -= 1
        else:
            last_stmt = None
        if depth != 0:
            for substmt in stmt.content[:last_index]:
                for statement, statement_depth in walk(substmt, depth-1, _initial_depth):
                    yield statement, statement_depth
        if last_stmt is not None:
            yield last_stmt, _initial_depth - depth
