#!/usr/bin/env python
"""Provides Fortran reader classes.

Overview
========

Provides FortranReader classes for reading Fortran codes from files and
strings. FortranReader handles comments and line continuations of both
fix and free format Fortran codes.

Examples
========

::

    >> from fparser.readfortran import FortranFileReader
    >>> import os
    >>> reader = FortranFileReader(os.path.expanduser('~/src/blas/daxpy.f'))
    >>> print reader.next()
    line #1 'subroutine daxpy(n,da,dx,incx,dy,incy)'
    >>> print `reader.next()`
    Comment('c     constant times a vector plus a vector.\\nc     uses unrolled loops for increments equal to one.\\nc     jack dongarra, linpack, 3/11/78.\\nc     modified 12/3/93, array(1) declarations changed to array(*)',(3, 6))
    >>> print `reader.next()`
    Line('double precision dx(*),dy(*),da',(8, 8),'')
    >>> print `reader.next()`
    Line('integer i,incx,incy,ix,iy,m,mp1,n',(9, 9),'')

Note that the ``.next()`` method may return `Line`, `SyntaxErrorLine`,
`Comment`, `MultiLine`, or `SyntaxErrorMultiLine` instance.
Let us continue with the above example session to illustrate the `Line` methods and attributes::

    >>> item = reader.next()
    >>> item
        Line('if (da .eq. 0.0d0) return',(12, 12),'')
    >>> item.line
        'if (da .eq. 0.0d0) return'
    >>> item.strline
        'if (F2PY_EXPR_TUPLE_5) return'
    >>> item.strlinemap
        {'F2PY_EXPR_TUPLE_5': 'da .eq. 0.0d0'}
    >>> item.span
        (12, 12)
    >>> item.get_line()
        'if (F2PY_EXPR_TUPLE_5) return'

To read a Fortran code from a string, use `FortranStringReader` class::

    >>> from fparser.readfortran import FortranStringReader
    >>> code = '''
    ...       subroutine foo(a)
    ...         integer a
    ...         print*,\"a=\",a
    ...       end
    ... '''
    >>> reader = FortranStringReader(code)
    >>> reader.set_mode(False, True)
    >>> reader.next()
        Line('subroutine foo(a)',(2, 2),'')
    >>> reader.next()
        Line('integer a',(3, 3),'')
    >>> reader.next()
        Line('print*,\"a=\",a',(4, 4),'')

"""
#Author: Pearu Peterson <pearu@cens.ioc.ee>
#Created: May 2006

__all__ = ['FortranFileReader',
           'FortranStringReader',
           'FortranReaderError',
           'Line', 'SyntaxErrorLine',
           'Comment',
           'MultiLine','SyntaxErrorMultiLine',
           ]

import re
import os
import sys
import tempfile
import traceback
import logging
from cStringIO import StringIO
from numpy.distutils.misc_util import yellow_text, red_text, blue_text

from sourceinfo import get_source_info, get_source_info_str
from splitline import String, string_replace_map, splitquote
from utils import is_name

logger = logging.getLogger('fparser')

_spacedigits=' 0123456789'
_cf2py_re = re.compile(r'(?P<indent>\s*)!f2py(?P<rest>.*)',re.I)
_is_fix_cont = lambda line: line and len(line)>5 and line[5]!=' ' and line[:5]==5*' '
_label_re = re.compile(r'\s*(?P<label>\d+)\s*(\b|(?=&)|\Z)',re.I)
_construct_name_re = re.compile(r'\s*(?P<name>\w+)\s*:\s*(\b|(?=&)|\Z)',re.I)
_is_include_line = re.compile(r'\s*include\s*("[^"]+"|\'[^\']+\')\s*\Z',re.I).match
def _is_fix_comment(line, isstrict):
    """ Check if line is a comment line in fixed format Fortran source.

    References
    ----------
    :f2008:`3.3.3`
    """
    if line:
        if line[0] in '*cC!':
            return True
        if not isstrict:
            i = line.find('!')
            if i!=-1:
                start = line[:i].lstrip()
                if not start:
                    if i==5:
                        # line continuation
                        return False
                    return True
                else:
                    # inline comment or ! is used in character context
                    # inline comments are handled elsewhere
                    pass
    elif line=='':
        return True
    return False
_hollerith_start_search = re.compile(r'(?P<pre>\A|,\s*)(?P<num>\d+)h',re.I).search
_is_call_stmt = re.compile(r'call\b', re.I).match

class FortranReaderError(Exception): 
    pass
    # def __init__(self, message):
    #     self.message = message
    #     print >> sys.stderr,message
    #     sys.stderr.flush()

class Line(object):
    """ Holds a Fortran source line.

    Attributes
    ----------
    line : str
      code line
    span : 2-tuple
      starting and ending line numbers
    label : {int, None}
      Specify statement label
    name : {str, None}
      Specify construct name.
    reader : FortranReaderBase
    strline : {None, str}
    is_f2py_directive : bool
      the line contains f2py directive
    """

    f2py_strmap_findall = re.compile(r'(_F2PY_STRING_CONSTANT_\d+_|F2PY_EXPR_TUPLE_\d+)').findall

    def __init__(self, line, linenospan, label, name, reader):
        self.line = line.strip()
        assert self.line, `line, linenospan, label`
        self.span = linenospan
        assert label is None or isinstance(label,int),`label`
        assert name is None or isinstance(name,str) and name!='',`name`
        self.label = label
        self.name = name
        self.reader = reader
        self.strline = None
        self.is_f2py_directive = linenospan[0] in reader.f2py_comment_lines
        self.parse_cache = {}

    def has_map(self):
        return not not (hasattr(self,'strlinemap') and self.strlinemap)

    def apply_map(self, line):
        if not hasattr(self,'strlinemap') or not self.strlinemap:
            return line
        findall = self.f2py_strmap_findall
        str_map = self.strlinemap
        keys = findall(line)
        for k in keys:
            line = line.replace(k, str_map[k])
        return line

    def copy(self, line = None, apply_map = False):
        if line is None:
            line = self.line
        if apply_map:
            line = self.apply_map(line)
        return Line(line, self.span, self.label, self.name, self.reader)

    def clone(self, line):
        self.line = self.apply_map(line)
        self.strline = None
        return

    def __repr__(self):
        return self.__class__.__name__+'(%r,%s,%r,%r,<reader>)' \
               % (self.line, self.span, self.label, self.name)

    def __str__(self):
        s = 'line #%s' % (self.span[0])
        if self.label is not None:
            s += ' %s ' % (self.label)
        if self.name is not None:
            s += '%s: ' % (self.name)
        return s + `self.line`

    def isempty(self, ignore_comments=False):
        return not (self.line or self.label is not None or self.name is not None)

    def get_line(self, apply_map=False):
        if apply_map:
            return self.apply_map(self.get_line(apply_map=False))
        if self.strline is not None:
            return self.strline
        line = self.line
        if self.reader.isf77:
            # Handle Hollerith constants by replacing them
            # with char-literal-constants.
            # H constants may appear only in DATA statements and
            # in the argument list of CALL statement.
            # Hollerith constants were removed from the Fortran 77 standard.
            # The following handling is not perfect but works for simple
            # usage cases.
            # todo: Handle hollerith constants in DATA statement
            if _is_call_stmt(line):
                l2 = self.line[4:].lstrip()
                i = l2.find('(')
                if i != -1 and l2[-1]==')':
                    substrings = ['call '+l2[:i+1]]
                    start_search = _hollerith_start_search
                    l2 = l2[i+1:-1].strip()
                    m = start_search(l2)
                    while m:
                        substrings.append(l2[:m.start()])
                        substrings.append(m.group('pre'))
                        num = int(m.group('num'))
                        substrings.append("'"+l2[m.end():m.end()+num]+"'")
                        l2 = l2[m.end()+num:]
                        m = start_search(l2)
                    substrings.append(l2)
                    substrings.append(')')
                    line = ''.join(substrings)

        line, str_map = string_replace_map(line, lower=not self.reader.ispyf)
        self.strline = line
        self.strlinemap = str_map
        return line

    def parse_line(self, cls, parent_cls):
        if cls not in self.parse_cache:
            self.parse_cache[cls] = None
            obj = cls(self.line, parent_cls = parent_cls)
            self.parse_cache[cls] = obj
        else:
            obj = self.parse_cache[cls]
            #print self.line, cls.__name__,obj
        return obj

    def parse_block(self, reader, cls, parent_cls):
        key = cls, tuple(parent_cls)
        if not self.parse_cache.has_key(key):
            #self.parse_cache[key] = None
            obj = cls(reader, parent_cls = parent_cls)
            self.parse_cache[key] = obj
        else:
            obj = self.parse_cache[key]
            #print self.line, cls.__name__,obj
        return obj

class SyntaxErrorLine(Line, FortranReaderError):
    def __init__(self, line, linenospan, label, name, reader, message):
        Line.__init__(self, line, linenospan, label, name, reader)
        FortranReaderError.__init__(self, message)

class Comment(object):
    """ Holds Fortran comment.

    Attributes
    ----------
    comment : str
      comment multiline string
    span : 2-tuple
      starting and ending line numbers
    reader : FortranReaderBase
    """
    def __init__(self, comment, linenospan, reader):
        self.comment = comment
        self.span = linenospan
        self.reader = reader
    def __repr__(self):
        return self.__class__.__name__+'(%r,%s)' \
               % (self.comment, self.span)
    def isempty(self, ignore_comments=False):
        return ignore_comments # or len(self.comment)<2

class MultiLine(object):
    """ Holds PYF file multiline.

    PYF file multiline is represented as follows::
      prefix+'''+lines+'''+suffix.
    
    Attributes
    ----------
    prefix : str
    block : list
      list of lines
    suffix : str
    span : 2-tuple
      starting and ending line numbers
    reader : FortranReaderBase
    """
    def __init__(self, prefix, block, suffix, linenospan, reader):
        self.prefix = prefix
        self.block  = block
        self.suffix = suffix
        self.span = linenospan
        self.reader = reader
    def __repr__(self):
        return self.__class__.__name__+'(%r,%r,%r,%s)' \
               % (self.prefix,self.block,self.suffix,
                  self.span)
    def isempty(self, ignore_comments=False):
        return not (self.prefix or self.block or self.suffix)

class SyntaxErrorMultiLine(MultiLine, FortranReaderError):
    def __init__(self, prefix, block, suffix, linenospan, reader, message):
        MultiLine.__init__(self, prefix, block, suffix, linenospan, reader)
        FortranReaderError.__init__(self, message)


class FortranReaderBase(object):
    """
    Base class for reading Fortran sources.

    A Fortran source must be file-like object (have a ``.next()``
    method) and it may hold Fortran 77 code, fixed format Fortran
    code, free format Fortran code, or PYF signatures (with extended
    free format Fortran syntax).
    
    The Fortran source is iterated by `get_single_line`,
    `get_next_line`, `put_single_line` methods.

    See also
    --------
    __init__
    """
    def __init__(self, source, isfree, isstrict):
        """ Construct FortranReader instance.

        Parameters
        ----------
        source :
          A file-like object with .next() method used to retrive a line.
        isfree : bool
        isstrict : bool

        See also
        --------
        FortranReaderBase
        """

        self.linecount = 0     # the current number of consumed lines
        self.source = source
        self.isclosed = False

        self.filo_line = []    # used for un-consuming lines.
        self.fifo_item = []
        self.source_lines = [] # source lines cache

        self.f2py_comment_lines = [] # line numbers that contain f2py directives

        self.reader = None
        self.include_dirs = ['.']

        self.source_only = None

        self.set_mode(isfree, isstrict)

        self.exit_on_error = True
        self.restore_cache = []
        return

    def __repr__(self):
        return '%s(%r, %r, %r)' % (self.__class__.__name__, self.source, self.isfree, self.isstrict)

    def find_module_source_file(self, mod_name):
        from utils import get_module_file, module_in_file
        if self.source_only:
            for sf in self.source_only:
                if module_in_file(mod_name, sf):
                    return sf
        else:
            fn = None
            for d in self.include_dirs:
                fn = get_module_file(mod_name, d)
                if fn is not None:
                    return fn

    def set_mode(self, isfree, isstrict):
        """ Set Fortran code mode.

        Parameters
        ----------
        isfree : bool
        isstrict : bool

        See also
        --------
        set_mode_from_str
        """
        assert isfree is not None
        assert isstrict is not None
        self.isfree = isfree and not isstrict
        self.isfix = not isfree and not isstrict
        self.isf77 = not isfree and isstrict
        self.ispyf   = isfree and isstrict
        self.isfree  = isfree
        self.isfixed   = not isfree
        self.isstrict = isstrict
        if isfree and isstrict:
            mode = 'pyf'
        elif self.isfree:
            mode = 'free'
        elif self.isfix:
            mode = 'fix'
        elif self.isf77:
            mode = 'f77'
        else:
            assert False
        self.mode = mode
        self.name = '%s mode=%s' % (self.source, mode)
        return

    def set_mode_from_str(self, mode):
        """Set Fortran code mode from a string.

        Parameters
        ----------
        mode : {'free', 'fix', 'f77', 'pyf'}

        See also
        --------
        set_mode
        """
        if mode=='free':
            isfree, isstrict=True, False
        elif mode=='fix':
            isfree, isstrict=False, False
        elif mode=='f77':
            isfree, isstrict=False, True
        elif mode=='pyf':
            isfree, isstrict=True, True
        else:
            raise NotImplementedError(`mode`)
        self.set_mode(isfree, isstrict)
    
    def close_source(self):
        """ Called when self.source.next() raises StopIteration.
        """
        pass

    # For handling raw source lines:

    def put_single_line(self, line):
        """ Put single line to FILO line buffer.

        ``linecount`` will be decremented, that is, the line was
        returned by ``get_single_line`` call then it will be
        un-consumed.
        
        See also
        --------
        get_single_line, get_next_line
        """
        self.filo_line.append(line)
        self.linecount -= 1
        return

    def get_single_line(self, ignore_empty=False, ignore_comments=False):
        """ Return line from FILO line buffer or from source.

        First try getting the line from FILO line buffer.

        If FILO line buffer is empty then get the next line from
        source. Tabs in source line are expanded, ending blank and new
        line characters will be removed.  The source line will be
        added to ``source_lines`` list. If source line is empty then
        recursively get next non-empty line.

        In both situations ``linecount`` will be incremented, that is,
        the line will be consumed.

        Parameters
        ----------
        ignore_empty : bool
          If True the ignore empty lines.

        See also
        --------
        put_single_line, get_next_line
        """
        try:
            line = self.filo_line.pop()
            self.linecount += 1
            return line
        except IndexError:
            pass
        if self.isclosed:
            return None
        try:
            line = self.source.next()
        except StopIteration:
            self.isclosed = True
            self.close_source()
            return None
        self.linecount += 1
        # expand tabs, replace special symbols, get rid of nl characters
        line = line.expandtabs().replace('\xa0',' ').rstrip()
        self.source_lines.append(line)

        if ignore_comments and _is_fix_comment(line, isstrict=self.isstrict):
            return self.get_single_line(ignore_empty, ignore_comments)
        
        if ignore_empty and not line:
            return self.get_single_line(ignore_empty, ignore_comments)

        return line

    def get_next_line(self, ignore_empty=False, ignore_comments=False):
        """ Return next non-empty line from FILO line buffer or from source.

        The line will be put to FILO line buffer. So, this method can
        be used for looking forward lines without consuming them.

        See also
        --------
        get_single_line, put_single_line
        """
        line = self.get_single_line(ignore_empty, ignore_comments)
        if line is None: return
        self.put_single_line(line)
        return line

    # Parser methods:
    def get_item(self):
        """ Return next item.
        """
        try:
            item = self.next(ignore_comments = True)
        except StopIteration:
            return
        return item

    def put_item(self, item):
        """ Insert item to FIFO item buffer.
        """
        self.fifo_item.insert(0, item)
        return

    # Iterator methods:

    def __iter__(self):
        """ Make FortranReader an iterator.
        """
        return self

    def next(self, ignore_comments = False):
        """ Return the next Fortran code item.

        Include statements are realized.

        Parameters
        ----------
        ignore_comments : bool
          When True then act as if Fortran code does not contain
          any comments or blank lines.

        See also
        --------
        _next, get_source_item
        """
        try:
            if self.reader is not None:
                # inside INCLUDE statement
                try:
                    return self.reader.next()
                except StopIteration:
                    self.reader = None
            item = self._next(ignore_comments)
            if isinstance(item, Line) and _is_include_line(item.line):
                # catch INCLUDE statement and create a new FortranReader
                # to enter to included file.
                reader = item.reader
                filename = item.line.strip()[7:].lstrip()[1:-1]
                include_dirs = self.include_dirs[:]
                path = filename
                for incl_dir in include_dirs:
                    path = os.path.join(incl_dir, filename)
                    if os.path.exists(path):
                        break
                if not os.path.isfile(path): # include file does not exist
                    dirs = os.pathsep.join(include_dirs)
                    # According to Fortran standard, INCLUDE line is
                    # not a Fortran statement.
                    reader.warning('%r not found in %r. INLCUDE line treated as comment line.'\
                                   % (filename, dirs), item)
                    item = self.next(ignore_comments)
                    return item
                    # To keep the information, we are turning the
                    # INCLUDE line to a comment:
                    reader.warning('%r not found in %r. '\
                                   'The INCLUDE line will be turned to a comment.'\
                                   % (filename, dirs), item)
                    return self.comment_item('!'+item.get_line(apply_map=True),
                                             item.span[0], item.span[1])
                reader.info('including file %r' % (path), item)
                self.reader = FortranFileReader(path, include_dirs=include_dirs)
                return self.reader.next(ignore_comments = ignore_comments)
            return item
        except StopIteration:
            raise
        except:
            message = self.format_message('FATAL ERROR',
                                          'while processing line',
                                          self.linecount, self.linecount)
            logger.critical(message)
            # self.show_message(message, sys.stderr)
            # traceback.print_exc(file=sys.stderr)
            logger.debug(''.join(('Traceback\n',''.join( traceback.format_stack()))))
            logger.critical(red_text('STOPPED READING'))
            # self.show_message(red_text('STOPPED READING'), sys.stderr)
            raise StopIteration

    def _next(self, ignore_comments = False):
        """ Return the next item from FIFO item buffer or construct
        one from source line.

        Resolves ``;`` statement terminations.

        Parameters
        ----------
        ignore_comments : bool

        See also
        --------
        next, get_source_item
        """
        fifo_item_pop = self.fifo_item.pop
        while 1:
            try:
                # first empty the FIFO item buffer:
                item = fifo_item_pop(0)
            except IndexError:
                # construct a new item from source
                item = self.get_source_item()
                if item is None:
                    raise StopIteration
            if not (item.isempty(ignore_comments) and ignore_comments):
                break
            # else ignore empty lines and comments
        if not isinstance(item, Comment):
            # resolve `;` statement terminations
            if not self.ispyf and isinstance(item, Line) \
                   and not item.is_f2py_directive \
                   and ';' in item.get_line():
                # ;-separator not recognized in pyf-mode
                items = []
                for line in item.get_line().split(';'):
                    line = line.strip()
                    if line:
                        items.append(item.copy(line, apply_map=True))
                items.reverse()
                for newitem in items:
                    self.fifo_item.insert(0, newitem)
                return fifo_item_pop(0)
            return item
        return item
        # collect subsequent comments to one comment instance
        comments = []
        start = item.span[0]
        while isinstance(item, Comment):
            comments.append(item.comment)
            end = item.span[1]
            while 1:
                try:
                    item = fifo_item_pop(0)
                except IndexError:
                    item = self.get_source_item()
                if item is None or not item.isempty(ignore_comments):
                    break
            if item is None:
                break # hold raising StopIteration for the next call.
        if item is not None:
            self.fifo_item.insert(0,item)
        item = self.comment_item('\n'.join(comments), start, end)
        return item

    # Interface to returned items:

    def line_item(self, line, startlineno, endlineno, label, name, errmessage=None):
        """ Construct Line item.
        """
        if errmessage is None:
            return  Line(line, (startlineno, endlineno), label, name, self)
        return SyntaxErrorLine(line, (startlineno, endlineno),
                               label, name, self, errmessage)

    def multiline_item(self, prefix, lines, suffix,
                       startlineno, endlineno, errmessage=None):
        """ Construct MultiLine item.
        """
        if errmessage is None:
            return MultiLine(prefix, lines, suffix, (startlineno, endlineno), self)
        return SyntaxErrorMultiLine(prefix, lines, suffix,
                                    (startlineno, endlineno), self, errmessage)

    def comment_item(self, comment, startlineno, endlineno):
        """ Construct Comment item.
        """
        return Comment(comment, (startlineno, endlineno), self)

    # For handling messages:

    # def show_message(self, message, stream = sys.stdout):
    #     stream.write(message+'\n')
    #     stream.flush()
    #     return

    def format_message(self, kind, message, startlineno, endlineno,
                       startcolno=0, endcolno=-1):
        back_index = {'warning':2,'error':3,'info':0}.get(kind.lower(),3)
        r = ['While processing %r (mode=%r)..' % (self.id, self.mode)]
        for i in range(max(1,startlineno-back_index),startlineno):
            r.append('%5d:%s' % (i,self.source_lines[i-1]))
        for i in range(startlineno,min(endlineno+back_index,len(self.source_lines))+1):
            if i==0 and not self.source_lines:
                break
            linenostr = '%5d:' % (i)
            if i==endlineno:
                sourceline = self.source_lines[i-1]
                l0 = linenostr+sourceline[:startcolno]
                if endcolno==-1:
                    l1 = sourceline[startcolno:]
                    l2 = ''
                else:
                    l1 = sourceline[startcolno:endcolno]
                    l2 = sourceline[endcolno:]
                r.append('%s%s%s <== %s' % (l0,yellow_text(l1),l2,red_text(message)))
            else:
                r.append(linenostr+ self.source_lines[i-1])
        return '\n'.join(r)

    def format_error_message(self, message, startlineno, endlineno,
                             startcolno=0, endcolno=-1):
        return self.format_message('ERROR',message, startlineno,
                                   endlineno, startcolno, endcolno)

    def format_warning_message(self, message, startlineno, endlineno,
                               startcolno=0, endcolno=-1):
        return self.format_message('WARNING',message, startlineno,
                                   endlineno, startcolno, endcolno)

    def info(self, message, item=None):
        if item is None:            
            m = self.format_message('INFORMATION',
                                      message,
                                      len(self.source_lines)-2, len(self.source_lines))
        else:
            m = self.format_message('INFORMATION',
                                      message,
                                      item.span[0], item.span[1])
        logger.info(m)
        # self.show_message(m, sys.stderr)
        return

    def error(self, message, item=None):
        if item is None:
            m = self.format_error_message(message, len(self.source_lines)-2, len(self.source_lines))
        else:
            m = self.format_error_message(message, item.span[0], item.span[1])
        logger.error(m)
        # self.show_message(m, sys.stderr)
        if self.exit_on_error:
            sys.exit(1)
        return

    def warning(self, message, item=None):
        if item is None:
            m = self.format_warning_message(message, len(self.source_lines)-2, len(self.source_lines))
        else:
            m = self.format_warning_message(message, item.span[0], item.span[1])
        logger.warning(m)
        # self.show_message(m, sys.stderr)
        return

    # Auxiliary methods for processing raw source lines:

    def handle_cf2py_start(self, line):
        """ Apply f2py directives to line.

        F2py directives are specified in the beginning of the line.

        f2py directives can be used only in Fortran codes.  They are
        ignored when used inside PYF files.

        Parameters
        ----------
        line : str

        Returns
        -------
        line : str
        """
        if not line or self.ispyf: return line
        if self.isfixed:
            if line[0] in '*cC!#':
                if line[1:5].lower() == 'f2py':
                    line = 5*' ' + line[5:]
                    self.f2py_comment_lines.append(self.linecount)
            if self.isf77:
                return line
        m = _cf2py_re.match(line)
        if m:
            newline = m.group('indent')+5*' '+m.group('rest')
            self.f2py_comment_lines.append(self.linecount)
            assert len(newline)==len(line),`newlinel,line`
            return newline
        return line

    def handle_inline_comment(self, line, lineno, quotechar=None):
        """
        Parameters
        ----------
        line : str
        lineno : int
        quotechar : {None, str}

        Returns
        -------
        line_with_no_comments : str
        quotechar : {None, str}
        had_comment : bool

        Notes
        -----
        In-line comments are separated from line and put back to fifo
        sequence where it will be processed as comment line.
        """
        had_comment = False
        if quotechar is None and '!' not in line and \
           '"' not in line and "'" not in line:
            return line, quotechar, had_comment
        i = line.find('!')
        put_item = self.fifo_item.append
        if quotechar is None and i!=-1:
            # first try a quick method:
            newline = line[:i]
            if '"' not in newline and '\'' not in newline:
                if self.isf77 or not line[i:].startswith('!f2py'):
                    put_item(self.comment_item(line[i:], lineno, lineno))
                    return newline, quotechar, True
        # handle cases where comment char may be a part of a character content
        #splitter = LineSplitter(line, quotechar)
        #items = [item for item in splitter]
        #newquotechar = splitter.quotechar
        items, newquotechar = splitquote(line, quotechar)

        noncomment_items = []
        noncomment_items_append = noncomment_items.append
        n = len(items)
        commentline = None
        for k in range(n):
            item = items[k]
            if isinstance(item, String) or '!' not in item:
                noncomment_items_append(item)
                continue
            j = item.find('!')
            noncomment_items_append(item[:j])
            items[k] = item[j:]
            commentline = ''.join(items[k:])
            break
        if commentline is not None:
            if commentline.startswith('!f2py'):
                # go to next iteration:
                newline = ''.join(noncomment_items) + commentline[5:]
                self.f2py_comment_lines.append(lineno)
                return self.handle_inline_comment(newline, lineno, quotechar)
            put_item(self.comment_item(commentline, lineno, lineno))
            had_comment = True
        return ''.join(noncomment_items), newquotechar, had_comment

    def handle_multilines(self, line, startlineno, mlstr):
        i = line.find(mlstr)
        if i != -1:
            prefix = line[:i]
            # skip fake multiline starts
            p,k = prefix,0
            while p.endswith('\\'):
                p,k = p[:-1],k+1
            if k % 2: return
        if i != -1 and '!' not in prefix:
            # Note character constans like 'abc"""123',
            # so multiline prefix should better not contain `'' or `"' not `!'.
            for quote in '"\'':
                if prefix.count(quote) % 2:
                    message = self.format_warning_message(\
                            'multiline prefix contains odd number of %r characters' \
                            % (quote), startlineno, startlineno,
                            0, len(prefix))
                    logger.warning(message)
                    # self.show_message(message, sys.stderr)

            suffix = None
            multilines = []
            line = line[i+3:]
            while line is not None:
                j = line.find(mlstr)
                if j != -1 and '!' not in line[:j]:
                    multilines.append(line[:j])
                    suffix = line[j+3:]
                    break
                multilines.append(line)
                line = self.get_single_line()
            if line is None:
                message = self.format_error_message(\
                            'multiline block never ends', startlineno,
                            startlineno, i)
                return self.multiline_item(\
                            prefix,multilines,suffix,\
                            startlineno, self.linecount, message)
            suffix,qc,had_comment = self.handle_inline_comment(suffix, self.linecount)
            # no line continuation allowed in multiline suffix
            if qc is not None:
                message = self.format_message(\
                            'ASSERTION FAILURE(pyf)',
                        'following character continuation: %r, expected None.' % (qc),
                            startlineno, self.linecount)
                logger.warning(message)
                # self.show_message(message, sys.stderr)
            # XXX: should we do line.replace('\\'+mlstr[0],mlstr[0])
            #      for line in multilines?
            return self.multiline_item(prefix,multilines,suffix,
                                       startlineno, self.linecount)


    # The main method of interpreting raw source lines within
    # the following contexts: f77, fixed, free, pyf.

    def get_source_item(self):
        """ Return next source item.
        
        A source item is ..
        - a fortran line
        - a list of continued fortran lines
        - a multiline - lines inside triple-qoutes, only when in ispyf mode
        - a comment line
        """
        get_single_line = self.get_single_line
        line = get_single_line()
        if line is None: return
        startlineno = self.linecount
        line = self.handle_cf2py_start(line)
        is_f2py_directive = startlineno in self.f2py_comment_lines
        isstrict = self.isstrict
        have_comment = False
        label = None
        name = None
        
        if self.ispyf:
            # handle multilines
            for mlstr in ['"""',"'''"]:
                r = self.handle_multilines(line, startlineno, mlstr)
                if r: return r
        if self.isfixed:
            if _is_fix_comment(line, isstrict):
                # comment line:
                return self.comment_item(line, startlineno, startlineno)

            for i in range(min(5,len(line))):
                # check that fixed format line starts according to Fortran standard
                if line[i] not in _spacedigits:
                    message =  'non-space/digit char %r found in column %i'\
                              ' of fixed Fortran code' % (line[i],i+1)
                    if i==0:
                        message += ', interpreting line as comment line'
                    if self.isfix:     
                        if i!=0:
                            message = message + ', switching to free format mode'
                        message = self.format_warning_message(\
                            message,startlineno, self.linecount)
                        logger.warning(message)
                        # self.show_message(message, sys.stderr)
                        if i==0:
                            # non standard comment line:
                            return self.comment_item(line, startlineno, startlineno)                           
                        self.set_mode(True, False)
                    else:
                        if i==0:
                            message = self.format_warning_message(\
                            message,startlineno, self.linecount)
                            logger.warning(message)
                            # self.show_message(message, sys.stderr)
                            # non standard comment line:
                            return self.comment_item(line, startlineno, startlineno)                           
                        # return line item with error message
                        # TODO: handle cases with line[6:]==''
                        return self.line_item(line[6:], startlineno, self.linecount,
                                           label, name, self.format_error_message(\
                            message, startlineno, self.linecount))
            # check for label
            s = line[:5].strip().lower()
            if s:
                label = int(s)
            if not self.isf77:
                m = _construct_name_re.match(line[6:])
                if m:
                    name = m.group('name')
                    line = line[:6] + line[6:][m.end():].lstrip()
            if not line[6:].strip():
                # check for a blank line
                if name is not None:
                    self.error('No construct following construct-name.')
                elif label is not None:
                    self.warning('Label must follow nonblank character (F2008:3.2.5_2)')
                return self.comment_item('', startlineno, self.linecount)
            # line is not a comment and the start of the line is valid

        if self.isf77 and not is_f2py_directive:
            # Fortran 77 is easy..
            lines = [line[6:72]]
            while _is_fix_cont(self.get_next_line(ignore_empty=True, ignore_comments=True)):
                # handle fix format line continuations for F77 code
                line = get_single_line()
                lines.append(line[6:72])
            return self.line_item(''.join(lines),startlineno,self.linecount,label,name)

        handle_inline_comment = self.handle_inline_comment

        endlineno = self.linecount
        if self.isfix and not is_f2py_directive:
            # handle inline comment
            newline,qc, had_comment = handle_inline_comment(line[6:], startlineno)
            have_comment |= had_comment
            lines = [newline]
            next_line = self.get_next_line()

            while _is_fix_cont(next_line) or _is_fix_comment(next_line, isstrict):
                # handle fix format line continuations for F90 or
                # newer code.  Mixing fix format and free format line
                # continuations is not allowed nor detected, just
                # eject warnings.
                line2 = get_single_line() # consume next_line as line2
                if _is_fix_comment(line2, isstrict):
                    # handle fix format comments inside line continuations after
                    # the line construction
                    citem = self.comment_item(line2,self.linecount,self.linecount)
                    self.fifo_item.append(citem)
                else:
                    # line continuation
                    newline, qc, had_comment = self.handle_inline_comment(line2[6:],
                                                                          self.linecount, qc)
                    have_comment |= had_comment
                    lines.append(newline)
                    endlineno = self.linecount
                next_line = self.get_next_line()
            # no character continuation should follows now
            if qc is not None:
                message = self.format_message(\
                            'ASSERTION FAILURE(fix)',
                            'following character continuation: %r, expected None.'\
                            % (qc), startlineno, self.linecount)
                logger.warning(message)
                # self.show_message(message, sys.stderr)
            if len(lines)>1:
                for i in range(len(lines)):
                    l = lines[i]
                    if l.rstrip().endswith('&'):
                        message = self.format_warning_message(\
                        'free format line continuation character `&\' detected'\
                        ' in fix format code',
                        startlineno + i, startlineno + i, l.rfind('&')+5)
                        logger.warning(message)
                        # self.show_message(message, sys.stderr)
            return self.line_item(''.join(lines),startlineno, endlineno,label,name)

        # line is free format or fixed format with f2py directive (that
        # will be interpretted as free format line).
        
        start_index = 0
        if self.isfix:
            start_index = 6

        lines = []
        lines_append = lines.append
        put_item = self.fifo_item.append
        qc = None
        while line is not None:
            if start_index: # fix format code
                line,qc,had_comment = handle_inline_comment(line[start_index:],
                                                self.linecount,qc)
                have_comment |= had_comment
                is_f2py_directive = self.linecount in self.f2py_comment_lines
            else:
                # free format
                line_lstrip = line.lstrip()
                if lines:
                    if line_lstrip.startswith('!'):
                        # check for comment line within line continuation
                        put_item(self.comment_item(line_lstrip,
                                                   self.linecount, self.linecount))
                        have_comment = True
                        line = get_single_line()
                        continue
                    elif line_lstrip == "":
                        # skip blank lines within a line continuation
                        line = get_single_line()
                        continue
                else:
                    # first line, check for a label
                    m = _label_re.match(line)
                    if m:
                        assert not label,`label`
                        label = int(m.group('label'))
                        line = line[m.end():]
                    # check for a construct name
                    m = _construct_name_re.match(line)
                    if m:
                        name = m.group('name')
                        line = line[m.end():].lstrip()
                line,qc,had_comment = handle_inline_comment(line, self.linecount, qc)
                have_comment |= had_comment
                is_f2py_directive = self.linecount in self.f2py_comment_lines

            i = line.rfind('&')
            if i!=-1:
                line_i1_rstrip = line[i+1:].rstrip()
            if not lines:
                # first line
                if i == -1 or line_i1_rstrip:
                    lines_append(line)
                    break
                endlineno = self.linecount
                lines_append(line[:i])
                line = get_single_line()
                continue
            if i == -1 or line_i1_rstrip:
                # no line continuation follows
                i = len(line)
            k = -1
            if i != -1:
                # handle the beggining of continued line
                k = line[:i].find('&')
                if k != 1 and line[:k].lstrip():
                    k = -1
            endlineno = self.linecount
            lines_append(line[k+1:i])
            if i==len(line):
                break
            line = get_single_line()

        if qc is not None:
            message = self.format_message('ASSERTION FAILURE(free)',
                'following character continuation: %r, expected None.' % (qc),
                startlineno, endlineno)
            logger.error(message)
            # self.show_message(message, sys.stderr)
        line_content = ''.join(lines).strip()
        if line_content:
            return self.line_item(line_content,startlineno,endlineno,label,name)
        if label is not None:
            self.warning('Label must follow nonblank character (F2008:3.2.5_2)')
        if name is not None:
            self.error('No construct following construct-name.')
        if have_comment:
            return self.next()
        return self.comment_item('', startlineno, endlineno)

    ##  FortranReaderBase

# Fortran file and string readers:

class FortranFileReader(FortranReaderBase):

    def __init__(self, filename, include_dirs = None, source_only=None):
        isfree, isstrict = get_source_info(filename)
        self.id = filename
        self.file = open(filename,'r')
        FortranReaderBase.__init__(self, self.file, isfree, isstrict)
        if include_dirs is None:
            self.include_dirs.insert(0, os.path.dirname(filename))
        else:
            self.include_dirs = include_dirs[:]
        if source_only is not None:
            self.source_only = source_only[:]
        return

    def close_source(self):
        self.file.close()

class FortranStringReader(FortranReaderBase):

    def __init__(self, string, include_dirs = None, source_only = None):
        self.id = 'string-'+str(id(string))
        source = StringIO(string)
        isfree, isstrict = get_source_info_str(string)
        FortranReaderBase.__init__(self, source, isfree, isstrict)
        if include_dirs is not None:
            self.include_dirs = include_dirs[:]
        if source_only is not None:
            self.source_only = source_only[:]
        return

# Testing:

def test_f77():
    string_f77 = """c -*- f77 -*-
c12346 comment
      subroutine foo
      call foo
     'bar
a    'g
      abc=2
cf2py call me ! hey
      call you ! hi
      end
     '"""
    reader = FortranStringReader(string_f77)
    assert reader.mode=='fix77', `reader.mode`
    for item in reader:
        print item

    filename = tempfile.mktemp()+'.f'
    f = open(filename,'w')
    f.write(string_f77)
    f.close()

    reader = FortranFileReader(filename)
    for item in reader:
        print item

def test_pyf():
    string_pyf = """! -*- pyf -*-
python module foo
  interface
  beginml '''1st line
  2nd line
  end line'''endml='tere!fake comment'!should be a comment
  a = 2
  'charc\"onstant' ''' single line mline '''a='hi!fake comment'!should be a comment
  a=\\\\\\\\\\'''not a multiline'''
  !blah='''never ending multiline
  b=3! hey, fake line continuation:&
  c=4& !line cont
  &45
  thisis_label_2 : c = 3
   xxif_isotropic_2 :     if ( string_upper_compare ( o%opt_aniso, 'ISOTROPIC' ) ) then
   g=3
   endif
  end interface
  if ( pc_get_lun() .ne. 6) &

    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping c_flag=", a, &
    & /, " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), trim(c_flag), pc_get_lun()
end python module foo
! end of file
"""
    reader = FortranStringReader(string_pyf)
    assert reader.mode=='pyf', `reader.mode`
    for item in reader:
        print item

def test_fix90():
    string_fix90 = """c -*- fix -*-
      subroutine foo
cComment
 1234 a = 3 !inline comment
      b = 3
!
     !4!line cont. with comment symbol
     &5
      a = 3!f2py.14 ! pi!
!   KDMO
      write (obj%print_lun, *) ' KDMO : '
      write (obj%print_lun, *) '  COORD = ',coord, '  BIN_WID = ',             &
       obj%bin_wid,'  VEL_DMO = ', obj%vel_dmo
      end subroutine foo
      subroutine

     & foo
      end
"""
    reader = FortranStringReader(string_fix90)
    assert reader.mode=='fix90', `reader.mode`
    for item in reader:
        print item

def simple_main():
    for filename in sys.argv[1:]:
        print 'Processing',filename
        reader = FortranFileReader(filename)
        for item in reader:
            print >> sys.stdout, item
            sys.stdout.flush()
            pass

def profile_main():
    import hotshot, hotshot.stats
    prof = hotshot.Profile("readfortran.prof")
    prof.runcall(simple_main)
    prof.close()
    stats = hotshot.stats.load("readfortran.prof")
    stats.strip_dirs()
    stats.sort_stats('time', 'calls')
    stats.print_stats(30)

if __name__ == "__main__":
    #test_pyf()
    #test_fix90()
    #test_f77()
    #profile_main()
    simple_main()
