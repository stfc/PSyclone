# Modified work Copyright (c) 2017-2024 Science and Technology
# Facilities Council.
# Original work Copyright (c) 1999-2008 Pearu Peterson

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

"""Base classes and exception handling for Fortran parser.

"""
# Original author: Pearu Peterson <pearu@cens.ioc.ee>
# First version created: Oct 2006

import re
from fparser.common import readfortran
from fparser.common.splitline import string_replace_map
from fparser.common.readfortran import FortranReaderBase
from fparser.two.symbol_table import SYMBOL_TABLES

# A list of supported extensions to the standard(s)

# An X edit descriptor in a format statement specifies the position
# (forward from the current position) at which the next character will
# be transmitted to or from a record. In standard Fortran2003 the X
# edit descriptor must be preceeded by an integer which specifies how
# far forward from the current position. The 'x-format' extension
# allows the X edit descriptor to be specified without a preceeding
# integer.
_EXTENSIONS = ["x-format"]

# Cray pointers are a well known extension to the Fortran
# standard. See http://pubs.cray.com/content/S-3901/8.6/
# cray-fortran-reference-manual-s-3901-86/types or
# https://gcc.gnu.org/onlinedocs/gfortran/Cray-pointers.html for
# example. If 'cray-pointer' is specified in EXTENSIONS then this
# extension is allowed in fparser.
_EXTENSIONS += ["cray-pointer"]

# A Hollerith constant is a way of specifying a string as a sequence
# of characters preceded by the string length and separated by an 'H'
# e.g. 5Hhello. See
# https://gcc.gnu.org/onlinedocs/gfortran/Hollerith-constants-support.html,
# for example for more details. fparser currently supports Hollerith
# constants specified in format statements when 'hollerith' is specified
# in the EXTENSIONS list.
_EXTENSIONS += ["hollerith"]

# Many compilers support the use of '$' in a fortran write statement
# to indicate that the carriage return should be suppressed. This is
# an extension to the Fortran standard and is supported by fparser if
# 'dollar-descriptor' is specified in the EXTENSIONS list.
_EXTENSIONS += ["dollar-descriptor"]

# Many compilers support the optional 'CONVERT' argument to OPEN(). This is
# used to indicate any endian-related conversion that must be performed
# when reading/writing data using unformatted IO.
_EXTENSIONS += ["open-convert"]

# While non-standard, many compilers support negative numbers, and string
# operations in stop statements, e.g. `stop -1` or `stop str1//str2`.
# With this extension, these statements will be allowed.
_EXTENSIONS += ["extended-stop-args"]


def EXTENSIONS():
    """
    :returns: the list of extensions (to the Fortran standard) currently active.
    :rtype: list[str]
    """
    return _EXTENSIONS


# Set this to True to get verbose output (on stdout) detailing the matches made
# while parsing.
_SHOW_MATCH_RESULTS = False


class FparserException(Exception):
    """Base class exception for fparser. This allows an external tool to
    capture all exceptions if required.

    :param str info: a string giving contextual error information.

    """

    def __init__(self, info):
        Exception.__init__(self, info)


class NoMatchError(FparserException):
    """An exception indicating that a particular rule implemented by a
    class does not match the provided string. It does not necessary
    mean there is an error as another rule may match. This exception
    is used internally so should never be visible externally.

    """


class FortranSyntaxError(FparserException):
    """An exception indicating that fparser believes the provided code to
    be invalid Fortran. Also returns information about the location of
    the error if that information is available.

    :param reader: input string or reader where the error took \
    place. This is used to provide line number and line content \
    information.
    :type reader: str or :py:class:`FortranReaderBase`
    :param str info: a string giving contextual error information.

    """

    def __init__(self, reader, info):
        output = "at unknown location "
        if isinstance(reader, FortranReaderBase):
            output = "at line {0}\n>>>{1}\n".format(
                reader.linecount, reader.source_lines[reader.linecount - 1]
            )
        if info:
            output += "{0}".format(info)
        FparserException.__init__(self, output)


class InternalError(FparserException):
    """An exception indicating that an unexpected error has occured in the
    parser.

    :param str info: a string giving contextual error information.

    """

    def __init__(self, info):
        new_info = "'{0}'. Please report this to the " "authors.".format(info)
        FparserException.__init__(self, new_info)


class InternalSyntaxError(FparserException):
    """An exception indicating that a syntax error has been found by the
    parser. This is used instead of `FortranSyntaxError` when the
    reader object is not available.

    """


def show_result(func):
    """
    A decorator that enables the matching sequence to be debugged by outputting
    the result (to stdout) whenever a new node in the parse tree is successfully
    constructed.

    :param function func: the functor that is being called.

    :returns: the supplied functor.
    :rtype: function

    """
    if not _SHOW_MATCH_RESULTS:
        # Just return the supplied functor unchanged.
        return func

    # It's not possible to monkeypatch decorators since the functions they are
    # wrapping get modified at module-import time. Therefore, we can't get
    # coverage of the rest of this routine.
    def new_func(cls, string, **kws):  # pragma: no cover
        """
        New functor to replace the one supplied. Simply wraps the supplied
        functor with some code that prints the match if it was successful.

        :param type cls: the Class that is being matched.
        :param str string: the string we are attempting to match.
        :param *kws: additional keyword arguments.
        :type *kws: Dict[str, Any]

        :returns: new functor object.
        :rtype: function

        """
        result = func(cls, string, **kws)
        if isinstance(result, StmtBase):
            if result:
                print(f"{cls.__name__}({string}) -> {result}")
            else:
                print(f"{cls.__name__}({string}) did NOT match")
        return result

    return new_func  # pragma: no cover


#
# BASE CLASSES
#


class ComparableMixin:
    """Mixin class to provide rich comparison operators.

    This mixin provides a set of rich comparison operators. Each class using
    this mixin has to provide a _cmpkey() method that returns a key of objects
    that can be compared.

    See also http://python3porting.com/preparing.html#richcomparisons
    """

    # pylint: disable=too-few-public-methods

    def _compare(self, other, method):
        """Call the method, if other is able to be used within it.

        :param object other: The other object to compare with
        :type other: object
        :param method: The method to call to compare self and other.
        :type method: LambdaType

        :return: NotImplemented, when the comparison for the given type
                 combination can't be performed.
        :rtype: :py:class:`types.NotImplementedType`

        """
        try:
            # This routine's purpose is to access the protected method
            # _cmpkey() from client classes, therefore: pylint:
            # disable=protected-access
            return method(self._cmpkey(), other._cmpkey())
        except (AttributeError, TypeError):
            # _cmpkey not implemented, or return different type,
            # so I can't compare with "other".
            # According to the Python Language Reference Manual
            # (http://www.network-theory.co.uk/docs/pylang/Coercionrules.html)
            # return NotImplemented
            return NotImplemented

    def __lt__(self, other):
        return self._compare(other, lambda s, o: s < o)

    def __le__(self, other):
        return self._compare(other, lambda s, o: s <= o)

    def __eq__(self, other):
        return self._compare(other, lambda s, o: s == o)

    def __ge__(self, other):
        return self._compare(other, lambda s, o: s >= o)

    def __gt__(self, other):
        return self._compare(other, lambda s, o: s > o)

    def __ne__(self, other):
        return self._compare(other, lambda s, o: s != o)


class DynamicImport:
    """This class imports a set of fparser.two dependencies that can not
    be imported during the Python Import time because they have a circular
    dependency with this file.

    They are imported once when the Fortran2003 is already processed by
    calling the import_now() method.

    The alternative is to have the equivalent top-level imports in the
    Base.__new__ method, but this method is in the parser critical path and
    is best to keep expensive operations out of it.
    """

    @staticmethod
    def import_now():
        """Execute the Import of Fortran2003 dependencies."""
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2003 import (
            Else_If_Stmt,
            Else_Stmt,
            End_If_Stmt,
            Masked_Elsewhere_Stmt,
            Elsewhere_Stmt,
            End_Where_Stmt,
            Type_Guard_Stmt,
            End_Select_Type_Stmt,
            Case_Stmt,
            End_Select_Stmt,
            Comment,
            Include_Stmt,
            add_comments_includes_directives,
        )
        from fparser.two import C99Preprocessor

        DynamicImport.Else_If_Stmt = Else_If_Stmt
        DynamicImport.Else_Stmt = Else_Stmt
        DynamicImport.End_If_Stmt = End_If_Stmt
        DynamicImport.Masked_Elsewhere_Stmt = Masked_Elsewhere_Stmt
        DynamicImport.Elsewhere_Stmt = Elsewhere_Stmt
        DynamicImport.End_Where_Stmt = End_Where_Stmt
        DynamicImport.Type_Guard_Stmt = Type_Guard_Stmt
        DynamicImport.End_Select_Type_Stmt = End_Select_Type_Stmt
        DynamicImport.Case_Stmt = Case_Stmt
        DynamicImport.End_Select_Stmt = End_Select_Stmt
        DynamicImport.Comment = Comment
        DynamicImport.Include_Stmt = Include_Stmt
        DynamicImport.C99Preprocessor = C99Preprocessor
        DynamicImport.add_comments_includes_directives = (
            add_comments_includes_directives
        )


di = DynamicImport()


def _set_parent(parent_node, items):
    """ Recursively set the parent of all of the elements
    in the list that are a sub-class of Base. (Recursive because
    sometimes the list of elements itself contains a list or tuple.)

    :param parent_node: the parent of the nodes listed in `items`.
    :type parent_node: sub-class of :py:class:`fparser.two.utils.Base`
    :param items: list or tuple of nodes for which to set the parent.
    :type items: list or tuple of :py:class:`fparser.two.utils.Base` \
                 or `str` or `list` or `tuple` or NoneType.
    """
    for item in items:
        if item:
            if isinstance(item, Base):
                # We can only set the parent of `Base` objects.
                # Anything else (e.g. str) is passed over.
                item.parent = parent_node
            elif isinstance(item, (list, tuple)):
                _set_parent(parent_node, item)


class Base(ComparableMixin):
    """Base class for Fortran 2003 syntax rules.

    All Base classes have the following attributes::

        self.string - original argument to construct a class instance, its
                      type is either str or FortranReaderBase.
        self.item   - Line instance (holds label) or None.

    :param type cls: the class of object to create.
    :param string: (source of) Fortran string to parse.
    :type string: str | :py:class:`fparser.common.readfortran.FortranReaderBase`
    :param parent_cls: the parent class of this object.
    :type parent_cls: `type`

    """

    # This dict of subclasses is populated dynamically by code at the end
    # of the fparser.two.parser module. That code uses the entries in the
    # 'subclass_names' list belonging to each class defined in this module.
    # See Issue #191 for a discussion of a way of getting rid of this state.
    subclasses = {}

    def __init__(self, string, parent_cls=None):
        # pylint:disable=unused-argument
        self.parent = None

    @show_result
    def __new__(cls, string, parent_cls=None, _deepcopy=False):
        if parent_cls is None:
            parent_cls = [cls]
        elif cls not in parent_cls:
            parent_cls.append(cls)

        # Get the class' match method if it has one
        match = getattr(cls, "match", None)

        if _deepcopy:
            # If this is part of a deep-copy operation (and string is None), simply call
            # the super method without string
            return super().__new__(cls)

        if (
            isinstance(string, FortranReaderBase)
            and match
            and not issubclass(cls, BlockBase)
        ):
            reader = string
            item = reader.get_item()
            if item is None:
                return None
            if isinstance(item, readfortran.Comment):
                # We got a comment but we weren't after a comment (we handle
                # those in Comment.__new__)
                obj = None
            else:
                try:
                    obj = item.parse_line(cls, parent_cls)
                except NoMatchError:
                    obj = None
            if obj is None:
                # No match so give the item back to the reader
                reader.put_item(item)
                return None
            obj.item = item
            return obj

        result = None
        if match:
            # IMPORTANT: if string is FortranReaderBase then cls must
            # restore readers content when no match is found.
            try:
                result = cls.match(string)
            except NoMatchError as msg:
                if str(msg) == "%s: %r" % (cls.__name__, string):
                    # avoid recursion 1.
                    raise

        if isinstance(result, tuple):
            obj = object.__new__(cls)
            obj.string = string
            obj.item = None
            # Set-up parent information for the results of the match
            _set_parent(obj, result)
            if hasattr(cls, "init"):
                obj.init(*result)
            return obj
        if isinstance(result, Base):
            return result
        if result is None:
            # Loop over the possible sub-classes of this class and
            # check for matches. This uses the list of subclasses calculated
            # at runtime in fparser.two.parser.
            for subcls in Base.subclasses.get(cls.__name__, []):
                if subcls in parent_cls:  # avoid recursion 2.
                    continue
                try:
                    obj = subcls(string, parent_cls=parent_cls)
                except NoMatchError:
                    obj = None
                if obj is not None:
                    return obj
        else:
            raise AssertionError(repr(result))
        # If we get to here then we've failed to match the current line
        if isinstance(string, FortranReaderBase):
            content = False
            for index in range(string.linecount):
                # Check all lines up to this one for content. We
                # should be able to only check the current line but
                # but as the line number returned is not always
                # correct (due to coding errors) we cannot assume the
                # line pointed to is the line where the error actually
                # happened.
                if string.source_lines[index].strip():
                    content = True
                    break
            if not content:
                # There are no lines in the input or all lines up to
                # this one are empty or contain only white space. This
                # is typically accepted by fortran compilers so we
                # follow their lead and do not raise an exception.
                return
            line = string.source_lines[string.linecount - 1]
            errmsg = f"at line {string.linecount}\n>>>{line}\n"
        else:
            errmsg = f"{cls.__name__}: '{string}'"
        raise NoMatchError(errmsg)

    def __getnewargs__(self):
        """Method to dictate the values passed to the __new__() method upon
        unpickling. The method must return a pair (args, kwargs) where
        args is a tuple of positional arguments and kwargs a dictionary
        of named arguments for constructing the object. Those will be
        passed to the __new__() method upon unpickling.

        :return: set of arguments for __new__
        :rtype: tuple[str, NoneType, bool]
        """
        return (self.string, None, True)

    def get_root(self):
        """
        Gets the node at the root of the parse tree to which this node belongs.

        :returns: the node at the root of the parse tree.
        :rtype: :py:class:`fparser.two.utils.Base`

        """
        current = self
        while current.parent:
            current = current.parent
        return current

    @property
    def children(self):
        """Return an iterable containing the immediate children of this node in
        the parse tree.

        If this node represents an expression then its children are
        contained in a tuple which is immutable. Therefore, the
        manipulation of the children of such a node must be done by
        replacing the `items` property of the node directly rather than via the
        objects returned by this method.

        :returns: the immediate children of this node.
        :rtype: list or tuple containing zero or more of \
                :py:class:`fparser.two.utils.Base` or NoneType or str

        """
        child_list = getattr(self, "content", None)
        if child_list is None:
            child_list = getattr(self, "items", [])
        return child_list

    def init(self, *items):
        """
        Store the supplied list of nodes in the `items` list of this node.

        :param items: the children of this node.
        :type items: tuple of :py:class:`fparser.two.utils.Base`

        """
        self.items = items

    def torepr(self):
        return "%s(%s)" % (self.__class__.__name__, ", ".join(map(repr, self.items)))

    def __str__(self):
        return self.tostr()

    def __repr__(self):
        return self.torepr()

    def _cmpkey(self):
        """Provides a key of objects to be used for comparing."""
        return self.items

    def tofortran(self, tab="", isfix=None):
        """
        Produce the Fortran representation of this Comment.

        :param str tab: characters to pre-pend to output.
        :param bool isfix: whether or not this is fixed-format code.

        :returns: Fortran representation of this comment.
        :rtype: str
        """
        this_str = str(self)
        if this_str.strip():
            return tab + this_str
        # If this_str is empty (i.e this Comment is a blank line) then
        # don't prepend any spaces to it
        return this_str

    def restore_reader(self, reader):
        reader.put_item(self.item)


class ScopingRegionMixin:
    """
    Mixin class for use in all classes that represent a scoping region and
    thus have an associated symbol table.

    """

    def get_scope_name(self):
        """
        :returns: the name of this scoping region.
        :rtype: str
        """
        return self.get_name().string


class BlockBase(Base):
    """
    Base class for matching all block constructs::

        <block-base> = [ <startcls> ]
                         [ <subcls> ]...
                         ...
                         [ <subcls> ]...
                         [ <endcls> ]

    """

    @staticmethod
    def match(
        startcls,
        subclasses,
        endcls,
        reader,
        match_labels=False,
        match_names=False,
        match_name_classes=(),
        enable_do_label_construct_hook=False,
        enable_if_construct_hook=False,
        enable_where_construct_hook=False,
        strict_order=False,
        strict_match_names=False,
    ):
        """
        Checks whether the content in reader matches the given
        type of block statement (e.g. DO..END DO, IF...END IF etc.)

        :param type startcls: the class marking the beginning of the block
        :param list subclasses: list of classes that can be children of \
                                the block.
        :param type endcls: the class marking the end of the block.
        :param reader: content to check for match.
        :type reader: str or instance of :py:class:`FortranReaderBase`
        :param bool match_labels: whether or not the statement terminating \
            the block must have a label that matches the opening statement. \
            Default is False.
        :param bool match_names: TBD
        :param tuple match_name_classes: TBD
        :param bool enable_do_label_construct_hook: TBD
        :param bool enable_if_construct_hook: TBD
        :param bool enable_where_construct_hook: TBD
        :param bool strict_order: whether to enforce the order of the \
                                  given subclasses.
        :param bool strict_match_names: if start name present, end name \
                                        must exist and match.

        :return: instance of startcls or None if no match is found
        :rtype: startcls

        """
        # This implementation uses the DynamicImport class and its instance di
        # to access the Fortran2003 and C99Preprocessor classes, this is a
        # performance optimization to avoid importing the classes inside this
        # method since it is in the hotpath (and it can't be done in the
        # top-level due to circular dependencies).
        assert isinstance(reader, FortranReaderBase), repr(reader)
        content = []
        # This will store the name of the new SymbolTable if we match a
        # scoping region.
        table_name = None

        if startcls is not None:
            # Deal with any preceding comments, includes, and/or directives
            DynamicImport.add_comments_includes_directives(content, reader)
            # Now attempt to match the start of the block
            try:
                obj = startcls(reader)
            except NoMatchError:
                obj = None
            if obj is None:
                # Ultimately we failed to find a match for the
                # start of the block so put back any comments that
                # we processed along the way
                for obj in reversed(content):
                    obj.restore_reader(reader)
                return
            if isinstance(obj, ScopingRegionMixin):
                # We are entering a new scoping unit so create a new
                # symbol table.
                # NOTE: if the match subsequently fails then we must
                #       delete this symbol table.
                table_name = obj.get_scope_name()
                SYMBOL_TABLES.enter_scope(table_name, obj)
            # Store the index of the start of this block proper (i.e.
            # excluding any comments)
            start_idx = len(content)
            content.append(obj)

            if hasattr(obj, "get_start_label") and enable_do_label_construct_hook:
                start_label = obj.get_start_label()
            if match_names:
                start_name = obj.get_start_name()

        # Comments and Include statements are always valid sub-classes
        classes = subclasses + [di.Comment, di.Include_Stmt]
        # Preprocessor directives are always valid sub-classes
        cpp_classes = [
            getattr(di.C99Preprocessor, cls_name)
            for cls_name in di.C99Preprocessor.CPP_CLASS_NAMES
        ]
        classes += cpp_classes
        if endcls is not None:
            classes += [endcls]
            endcls_all = tuple([endcls] + endcls.subclasses[endcls.__name__])

        try:
            # Start trying to match the various subclasses, starting from
            # the beginning of the list (where else?)
            i = 0
            had_match = False
            found_end = False
            while i < len(classes):
                if enable_do_label_construct_hook:
                    # Multiple, labelled DO statements can reference the
                    # same label.
                    obj = startcls(reader)
                    if obj is not None and hasattr(obj, "get_start_label"):
                        if start_label == obj.get_start_label():
                            content.append(obj)
                            continue
                        obj.restore_reader(reader)
                # Attempt to match the i'th subclass
                cls = classes[i]
                try:
                    obj = cls(reader)
                except NoMatchError:
                    obj = None
                if obj is None:
                    # No match for this class, continue checking the list
                    # starting from the i+1'th...
                    i += 1
                    continue

                # We got a match for this class
                had_match = True
                content.append(obj)

                if match_names and isinstance(obj, match_name_classes):
                    end_name = obj.get_end_name()
                    if end_name and not start_name:
                        raise FortranSyntaxError(
                            reader,
                            f"Name '{end_name}' has no corresponding starting name",
                        )
                    if (
                        end_name
                        and start_name
                        and end_name.lower() != start_name.lower()
                    ):
                        raise FortranSyntaxError(
                            reader, f"Expecting name '{start_name}', got '{end_name}'"
                        )

                if endcls is not None and isinstance(obj, endcls_all):
                    if match_labels:
                        start_label, end_label = (
                            content[start_idx].get_start_label(),
                            content[-1].get_end_label(),
                        )
                        if start_label != end_label:
                            continue
                    if match_names:
                        start_name, end_name = (
                            content[start_idx].get_start_name(),
                            content[-1].get_end_name(),
                        )

                        if end_name and not start_name:
                            raise FortranSyntaxError(
                                reader,
                                f"Name '{end_name}' has no corresponding starting name",
                            )
                        elif strict_match_names and start_name and not end_name:
                            raise FortranSyntaxError(
                                reader, f"Expecting name '{start_name}' but none given"
                            )
                        elif (
                            start_name
                            and end_name
                            and (start_name.lower() != end_name.lower())
                        ):
                            raise FortranSyntaxError(
                                reader,
                                f"Expecting name '{start_name}', got '{end_name}'",
                            )
                    # We've found the enclosing end statement so break out
                    found_end = True
                    break
                if not strict_order:
                    # Return to start of classes list now that we've matched.
                    i = 0
                if enable_if_construct_hook:
                    if isinstance(obj, di.Else_If_Stmt):
                        # Got an else-if so go back to start of possible
                        # classes to match
                        i = 0
                    if isinstance(obj, (di.Else_Stmt, di.End_If_Stmt)):
                        # Found end-if
                        enable_if_construct_hook = False
                if enable_where_construct_hook:
                    if isinstance(obj, di.Masked_Elsewhere_Stmt):
                        i = 0
                    if isinstance(obj, (di.Elsewhere_Stmt, di.End_Where_Stmt)):
                        enable_where_construct_hook = False
                continue

        except FortranSyntaxError as err:
            # We hit trouble so clean up the symbol table
            if table_name:
                SYMBOL_TABLES.exit_scope()
                # Remove any symbol table that we created
                SYMBOL_TABLES.remove(table_name)
            raise err

        if table_name:
            SYMBOL_TABLES.exit_scope()

        if not had_match or endcls and not found_end:
            # We did not get a match from any of the subclasses or
            # failed to find the endcls
            if endcls is not None:
                if table_name:
                    # Remove any symbol table that we created
                    SYMBOL_TABLES.remove(table_name)
                for obj in reversed(content):
                    obj.restore_reader(reader)
                return None

        if not content:
            # We can only get to here if startcls is None - if startcls is not
            # None and fails to match then we will already have returned. If
            # it is not None and matches then content will not be empty.
            # Since startcls must be None, we won't have created a symbol
            # table so we don't have to clean up.
            return None

        if startcls is not None and endcls is not None:
            # check names of start and end statements:
            start_stmt = content[start_idx]
            end_stmt = content[-1]
            if (
                isinstance(end_stmt, endcls_all)
                and hasattr(end_stmt, "get_name")
                and hasattr(start_stmt, "get_name")
            ):
                if end_stmt.get_name() is not None:
                    if (
                        start_stmt.get_name().string.lower()
                        != end_stmt.get_name().string.lower()
                    ):
                        end_stmt.item.reader.error(
                            "expected <%s-name> is %s but got %s. Ignoring."
                            % (
                                end_stmt.get_type().lower(),
                                start_stmt.get_name(),
                                end_stmt.get_name(),
                            )
                        )
        return (content,)

    def init(self, content):
        """
        Initialise the `content` attribute with the list of child nodes.

        :param content: list of nodes that are children of this one.
        :type content: list of :py:class:`fparser.two.utils.Base` or NoneType

        """
        self.content = content

    def _cmpkey(self):
        """Provides a key of objects to be used for comparing."""
        return self.content

    def tostr(self):
        return self.tofortran()

    def torepr(self):
        return "%s(%s)" % (self.__class__.__name__, ", ".join(map(repr, self.content)))

    def tofortran(self, tab="", isfix=None):
        """
        Create a string containing the Fortran representation of this class

        :param str tab: indent to prefix to code.
        :param bool isfix: whether or not to generate fixed-format code.

        :return: Fortran representation of this class.
        :rtype: str
        """
        mylist = []
        start = self.content[0]
        end = self.content[-1]
        extra_tab = ""
        if isinstance(end, EndStmtBase):
            extra_tab = "  "
        if start is not None:
            mylist.append(start.tofortran(tab=tab, isfix=isfix))
        for item in self.content[1:-1]:
            mylist.append(item.tofortran(tab=tab + extra_tab, isfix=isfix))
        if len(self.content) > 1:
            mylist.append(end.tofortran(tab=tab, isfix=isfix))
        return "\n".join(mylist)

    def restore_reader(self, reader):
        for obj in reversed(self.content):
            obj.restore_reader(reader)


class SequenceBase(Base):
    """
    Match one or more fparser2 rules separated by a defined separator::

        sequence-base is obj [sep obj ] ...

    """

    @staticmethod
    def match(separator, subcls, string):
        """Match one or more 'subcls' fparser2 rules in the string 'string'
        separated by 'separator'.

        :param str separator: the separator used to split the supplied \
                              string.
        :param subcls: an fparser2 object representing the rule that \
                       should be matched.
        :type subcls: subclass of :py:class:`fparser.two.utils.Base`
        :param str string: the input string to match.

        :returns: a tuple containing 1) the separator and 2) the \
                  matched objects in a tuple, or None if there is no match.
        :rtype: Optional[(Str, :py:class:`fparser.two.utils.Base`)]

        :raises InternalError: if the separator or string arguments  \
                               are not the expected type.
        :raises InternalError: if the separator is white space.

        """
        if not isinstance(separator, str):
            raise InternalError(
                f"SequenceBase class match method argument separator expected "
                f"to be a string but found '{type(separator)}'."
            )
        if not isinstance(string, str):
            raise InternalError(
                f"SequenceBase class match method argument string expected to "
                f"be a string but found '{type(string)}'."
            )

        if separator == " ":
            raise InternalError(
                "SequenceBase class match method argument separator cannot "
                "be white space."
            )

        line, repmap = string_replace_map(string)
        splitted = line.split(separator)
        if not splitted:
            # There should be at least one entry.
            return None

        lst = [subcls(repmap(entry.strip())) for entry in splitted]

        return separator, tuple(lst)

    def init(self, separator, items):
        """Store the result of the match method if the match is successful.

        :param str separator: the separator used to split the supplied string.
        :param items: a tuple containing the matched objects.
        :type items: tuple(Subclass of :py:class:`fparser.two.utils.Base`)

        """
        self.separator = separator
        self.items = items

    def tostr(self):
        """
        :returns: The Fortran representation of this object as a string.
        :rtype: str

        """
        sep = self.separator
        if sep == ",":
            sep = sep + " "
        elif sep == " ":
            pass
        else:
            sep = " " + sep + " "
        return sep.join(map(str, self.items))

    def torepr(self):
        """
        :returns: The Python representation of this object as a string.
        :rtype: str

        """
        return "{0}('{1}', {2})".format(
            self.__class__.__name__, self.separator, self.items
        )

    # The mixin class is likely to be removed so _cmpkey would not be
    # needed. It is not used at the moment. It is only commented out
    # at this point, rather than removed, in case it turns out that
    # the mixin class is useful.
    # def _cmpkey(self):
    #     """ Provides a key of objects to be used for comparing.
    #     """
    #     return (self.separator, self.items)


class UnaryOpBase(Base):
    """
    ::

        unary-op-base is unary-op rhs

    """

    def tostr(self):
        return "%s %s" % tuple(self.items)

    @staticmethod
    def match(op_pattern, rhs_cls, string, exclude_op_pattern=None):
        m = op_pattern.match(string)
        if not m:
            return
        rhs = string[m.end() :].lstrip()
        if not rhs:
            return
        op = string[: m.end()].rstrip().upper()
        if exclude_op_pattern is not None:
            if exclude_op_pattern.match(op):
                return
        return op, rhs_cls(rhs)


class BinaryOpBase(Base):
    """
    ::

        binary-op-base is lhs op rhs

    Splits the input text into text to the left of the matched
    operator and text to the right of the matched operator and tries
    to match the lhs text with the supplied lhs class rule and the rhs
    text with the supplied rhs class rule.

    """

    @staticmethod
    def match(
        lhs_cls, op_pattern, rhs_cls, string, right=True, exclude_op_pattern=None
    ):
        """Matches the binary-op-base rule.

        If the operator defined by argument 'op_pattern' is found in
        the string provided in argument 'string' then the text to the
        left-hand-side of the operator is matched with the class rule
        provided in the 'lhs_cls' argument and the text to the
        right-hand-side of the operator is matched with the class rule
        provided in the 'rhs_cls' argument.

        If the optional 'right' argument is set to true (the default)
        then, in the case where the pattern matches multiple times in
        the input string, the right-most match will be chosen. If the
        'right' argument is set to false then the left-most match will
        be chosen.

        if a pattern is provided to the optional 'exclude_op_pattern'
        argument then there will be no match if the pattern matched by
        the 'op_pattern' argument also matches this pattern. The
        default (None) does nothing.

        :param lhs_cls: an fparser2 object representing the rule that \
            should be matched to the lhs text.
        :type lhs_cls: subclass of :py:class:`fparser.two.utils.Base`
        :param op_pattern: the pattern to match.
        :type op_pattern: `str` or \
            :py:class:`fparser.two.pattern_tools.Pattern`
        :param rhs_cls: an fparser2 object representing the rule that \
            should be matched to the rhs text.
        :type rhs_cls: subclass of :py:class:`fparser.two.utils.Base`
        :param str string: the string to match with the pattern and \
            lhs and rhs rules.
        :param bool right: in the case where there are multiple \
            matches to the pattern in the string this optional \
            argument specifies whether the righmost pattern match \
            should be chosen (True, the default) or whether the \
            leftmost pattern should be chosen (False).
        :param exclude_op_pattern: optional argument which specifies a \
            particular subpattern to exclude from the match. Defaults \
            to None which means there is no subpattern.
        :type exclude_op_pattern: :py:class:`fparser.two.pattern_tools.Pattern`

        :returns: a tuple containing the matched lhs, the operator and \
            the matched rhs of the input string or None if there is \
            no match.
        :rtype: (:py:class:`fparser.two.utils.Base`, str, \
            :py:class:`fparser.two.utils.Base`) or NoneType

        """
        line, repmap = string_replace_map(string)

        if isinstance(op_pattern, str):
            if right:
                text_split = line.rsplit(op_pattern, 1)
            else:
                text_split = line.split(op_pattern, 1)
            if len(text_split) != 2:
                return None
            lhs, rhs = text_split[0].rstrip(), text_split[1].lstrip()
            oper = op_pattern
        else:
            if right:
                text_split = op_pattern.rsplit(line)
            else:
                text_split = op_pattern.lsplit(line)
            if not text_split or len(text_split) != 3:
                return None
            lhs, oper, rhs = text_split
            lhs = lhs.rstrip()
            rhs = rhs.lstrip()
            oper = oper.upper()
        if not lhs or not rhs:
            return None
        if exclude_op_pattern and exclude_op_pattern.match(oper):
            return None

        # Matching the shorter text first can be much more efficient
        # for complex expressions.
        if right:
            # The split is closest to the right so try to match the
            # RHS first.
            rhs_obj = rhs_cls(repmap(rhs))
            lhs_obj = lhs_cls(repmap(lhs))
        else:
            # The split is closest to the left so try to match the LHS
            # first.
            lhs_obj = lhs_cls(repmap(lhs))
            rhs_obj = rhs_cls(repmap(rhs))

        return (lhs_obj, oper.replace(" ", ""), rhs_obj)

    def tostr(self):
        """Return the string representation of this object. Uses join() which
        is efficient and can make a big performance difference for
        complex expressions.

        :returns: the string representation of this object.
        :rtype: str

        """
        return " ".join([str(self.items[0]), str(self.items[1]), str(self.items[2])])


class SeparatorBase(Base):
    """
    ::

        separator-base is [ lhs ] : [ rhs ]

    """

    @staticmethod
    def match(lhs_cls, rhs_cls, string, require_lhs=False, require_rhs=False):
        line, repmap = string_replace_map(string)
        if ":" not in line:
            return
        lhs, rhs = line.split(":", 1)
        lhs = lhs.rstrip()
        rhs = rhs.lstrip()
        lhs_obj, rhs_obj = None, None
        if lhs:
            if lhs_cls is None:
                return
            lhs_obj = lhs_cls(repmap(lhs))
        elif require_lhs:
            return
        if rhs:
            if rhs_cls is None:
                return
            rhs_obj = rhs_cls(repmap(rhs))
        elif require_rhs:
            return
        return lhs_obj, rhs_obj

    def tostr(self):
        s = ""
        if self.items[0] is not None:
            s += "%s :" % (self.items[0])
        else:
            s += ":"
        if self.items[1] is not None:
            s += " %s" % (self.items[1])
        return s


class KeywordValueBase(Base):
    """
    ::

        keyword-value-base is [ lhs = ] rhs

    where::

        R215 keyword is name.

    """

    @staticmethod
    def match(lhs_cls, rhs_cls, string, require_lhs=True, upper_lhs=False):
        """
        Attempts to match the supplied `string` with `lhs_cls` = `rhs_cls`.
        If `lhs_cls` is a str then it is compared with the content to the
        left of the first '=' character in `string`. If that content is a
        valid Fortran name but does *not* match `lhs_cls` then the match
        fails, irrespective of the setting of `require_lhs`.

        :param lhs_cls: list, tuple or single value of classes to attempt to \
                        match LHS against (in order), or string containing \
                        keyword to match.
        :type lhs_cls: names of classes deriving from `:py:class:Base` or str
        :param rhs_cls: name of class to match RHS against.
        :type rhs_cls: name of a class deriving from `:py:class:Base`
        :param str string: text to be matched.
        :param bool require_lhs: whether the expression to be matched must \
                                 contain a LHS that is assigned to.
        :param bool upper_lhs: whether or not to convert the LHS of the \
                               matched expression to upper case.

        :return: instances of the classes representing quantities on the LHS \
                 and RHS (LHS is optional) or None if no match is found.
        :rtype: 2-tuple of objects or NoneType

        """
        if require_lhs and "=" not in string:
            return None
        if isinstance(lhs_cls, (list, tuple)):
            for cls in lhs_cls:
                obj = KeywordValueBase.match(
                    cls, rhs_cls, string, require_lhs=require_lhs, upper_lhs=upper_lhs
                )
                if obj:
                    return obj
            return obj
        # We can't just blindly check whether 'string' contains an '='
        # character as it could itself hold a string constant containing
        # an '=', e.g. FMT='("Hello = False")'.
        # Therefore we only split on the left-most '=' character
        pieces = string.split("=", 1)
        lhs = None
        if len(pieces) == 2:
            # It does contain at least one '='. Proceed to attempt to match
            # the content on the LHS of it.
            lhs = pieces[0].strip()
            if isinstance(lhs_cls, str):
                # lhs_cls is a keyword
                if upper_lhs:
                    lhs = lhs.upper()
                if lhs != lhs_cls:
                    # The content to the left of the '=' does not match the
                    # supplied keyword
                    lhs = None
            else:
                lhs = lhs_cls(lhs)
        if not lhs:
            # We haven't matched the LHS and therefore proceed to treat the
            # whole string as a RHS if the LHS is not strictly required.
            if require_lhs:
                return None
            rhs = string.strip()
        else:
            rhs = pieces[-1].strip()
        if rhs:
            rhs = rhs_cls(rhs)
        if not rhs:
            return None
        return lhs, rhs

    def tostr(self):
        if self.items[0] is None:
            return str(self.items[1])
        return "%s = %s" % tuple(self.items)


class BracketBase(Base):
    """
    bracket-base is left-bracket something right-bracket.

    This class is able to cope with nested brackets as long as they
    are correctly nested. Brackets in strings are ignored.

    The 'something' can be specified as being optional.

    """

    @staticmethod
    def match(brackets, cls, string, require_cls=True):
        """A generic match method for all types of bracketed
        expressions.

        :param str brackets: the format of the left and right brackets \
        provided as a string, for example '()'
        :param cls: the class to match the content within the brackets \
        :type cls: subclass of :py:class:`fparser.two.utils.Base`
        :param str string: the content to match
        :param bool require_cls: whether the class and associated \
        content is mandatory (True) or optional (False). The default \
        is True.
        :return: None if there is no match, otherwise a tuple with the \
        first and third entries being strings containing the left and \
        right brackets respectively and the second entry being either \
        None or an instance of the class provided as the second \
        argument (cls).
        :rtype: 'NoneType', ( `str`, `NoneType`, `str`) or ( `str`, \
        `cls`, `str` )

        """
        if not cls and require_cls:
            return None
        if not string:
            return None
        string_strip = string.strip()
        if not brackets:
            return None
        brackets_nospc = brackets.replace(" ", "")
        if not brackets_nospc:
            return None
        if len(brackets_nospc) % 2 == 1:
            # LHS and RHS bracketing must be the same size
            return None
        bracket_len = len(brackets_nospc) // 2
        left = brackets_nospc[:bracket_len]
        right = brackets_nospc[-bracket_len:]
        if len(string_strip) < bracket_len * 2:
            return None
        if not (string_strip.startswith(left) and string_strip.endswith(right)):
            return None
        # Check whether or not there's anything between the open
        # and close brackets
        line = string_strip[bracket_len:-bracket_len].lstrip()
        if (not line and cls and require_cls) or (line and not cls):
            return None
        if not line and (not cls or not require_cls):
            return left, None, right
        return left, cls(line), right

    def tostr(self):
        """
        :raises InternalError: if the internal items list variable is \
                               not the expected size.
        :raises InternalError: if the first element of the internal \
                               items list is None or is an empty string.
        """

        if len(self.items) != 3:
            raise InternalError(
                "Class BracketBase method tostr() has '{0}' items, "
                "but expecting 3.".format(len(self.items))
            )
        if not self.items[0]:
            raise InternalError(
                "Class BracketBase method tostr(). 'Items' entry 0 "
                "should be a string containing the left hand bracket "
                "but it is empty or None"
            )
        if not self.items[2]:
            raise InternalError(
                "Class BracketBase method tostr(). 'Items' entry 2 "
                "should be a string containing the right hand bracket "
                "but it is empty or None"
            )
        if self.items[1] is None:
            return "{0}{1}".format(self.items[0], self.items[2])
        return "{0}{1}{2}".format(self.items[0], self.items[1], self.items[2])


class NumberBase(Base):
    """
    ::

        number-base is number [ _ kind-param ]

    """

    @staticmethod
    def match(number_pattern, string):
        m = number_pattern.match(string.replace(" ", ""))
        if m is None:
            return
        d = m.groupdict()
        return d["value"].upper(), d.get("kind_param")

    def tostr(self):
        if self.items[1] is None:
            return str(self.items[0])
        return "%s_%s" % tuple(self.items)

    def _cmpkey(self):
        """Provides a key of objects to be used for comparing."""
        return self.items[0]


class CallBase(Base):
    """
    ::

        call-base is lhs ( [ rhs ] )

    """

    @staticmethod
    def match(lhs_cls, rhs_cls, string, upper_lhs=False, require_rhs=False):
        """
        :param lhs_cls: the class to match with the lhs.
        :type lhs_cls: str | class
        :param rhs_cls: the class to match with the rhs.
        :type rhs_cls: str | class
        :param str string: the string to attempt to match.
        :param bool upper_lhs: whether or not to convert the lhs to uppercase \
                               before attempting the match.
        :param bool require_rhs: whether the rhs (the part within parentheses) \
                                 must be present.

        :returns: a tuple containing the lhs and rhs matches or None if there is \
                  no match.
        :rtype: Optional[Tuple[:py:class:`fparser.two.utils.Base`, \
                               Optional[:py:class:`fparser.two.utils.Base`]]]

        """
        if not string.rstrip().endswith(")"):
            return None
        line, repmap = string_replace_map(string)
        open_idx = line.rfind("(")
        if open_idx == -1:
            return None
        lhs = line[:open_idx].rstrip()
        if not lhs:
            return
        close_idx = line.rfind(")")
        rhs = line[open_idx + 1 : close_idx].strip()
        lhs = repmap(lhs)
        if upper_lhs:
            lhs = lhs.upper()
        rhs = repmap(rhs)
        if isinstance(lhs_cls, str):
            if lhs_cls != lhs:
                return None
        else:
            lhs = lhs_cls(lhs)
        if rhs:
            if isinstance(rhs_cls, str):
                if rhs_cls != rhs:
                    return None
            else:
                rhs = rhs_cls(rhs)
            return lhs, rhs
        if require_rhs:
            return None
        return lhs, None

    def tostr(self):
        if self.items[1] is None:
            return "%s()" % (self.items[0])
        return "%s(%s)" % (self.items[0], self.items[1])


class CALLBase(CallBase):
    """
    ::

        CALL-base is LHS ( [ rhs ] )

    """

    @staticmethod
    def match(lhs_cls, rhs_cls, string, require_rhs=False):
        return CallBase.match(
            lhs_cls, rhs_cls, string, upper_lhs=True, require_rhs=require_rhs
        )


class StringBase(Base):
    """
    ::

        string-base is xyz

    """

    @staticmethod
    def match(pattern, string):
        if isinstance(pattern, (list, tuple)):
            for p in pattern:
                obj = StringBase.match(p, string)
                if obj is not None:
                    return obj
            return
        if isinstance(pattern, str):
            if len(pattern) == len(string) and pattern == string:
                return (string,)
            return
        if pattern.match(string):
            return (string,)
        return None

    def init(self, string):
        self.string = string

    def tostr(self):
        return str(self.string)

    def torepr(self):
        return "%s(%r)" % (self.__class__.__name__, self.string)

    def _cmpkey(self):
        """Provides a key of objects to be used for comparing."""
        return self.string


class STRINGBase(StringBase):
    """STRINGBase matches an upper case version of the input string with
    another a pattern (typically taken from pattern_tools.py) and
    returns the string in upper case if there is a match.

    """

    @staticmethod
    def match(my_pattern, string):
        """Matches an input string with a specified pattern. Casts the string
        to upper case before performing a match and, if there is a
        match, returns the string in upper case.

        The pattern can be a regular expression, a string, a list or a
        tuple. If the input pattern is a regular expression or a
        string, a direct equivalence is performed. If the input pattern is a
        list or a tuple, then all of the contents of the list
        or tuple are searched for a match (by recursing). The list or tuple may
        contain regular expressions, strings, lists or tuples. This
        functionality can be used to recurse down a tree of lists and
        or tuples until regular expressions or strings are found (at
        the leaves of the tree) on which to match. The patterns used
        to match in fparser can be found in patterns_tools.py. These
        make use of the pattern class, whose match method behaves like
        a regular expression. For example:

        from fparser.two import pattern_tools
        pattern = pattern_tools.intrinsic_type_name
        result = STRINGBase.match(pattern, "logical")

        :param pattern: the pattern to match
        :type pattern: `list`, `tuple`, `str` or an `re` expression
        :param str string: the string to match with the pattern
        :return: None if there is no match, or a tuple containing the \
        matched string in upper case.
        :rtype: `NoneType` or ( `str` )

        """
        if string is None:
            return None
        if not isinstance(string, str):
            raise InternalError(
                f"Supplied string should be of type str, but found {type(string)}"
            )
        if isinstance(my_pattern, (list, tuple)):
            for child in my_pattern:
                result = STRINGBase.match(child, string)
                if result:
                    return result
            return None
        string_upper = string.upper()
        if isinstance(my_pattern, str):
            if len(my_pattern) == len(string) and my_pattern == string_upper:
                return (string_upper,)
            return None
        try:
            if my_pattern.match(string_upper):
                return (string_upper,)
        except AttributeError:
            raise InternalError(
                f"Supplied pattern should be a list, tuple, str or regular "
                f"expression but found {type(my_pattern)}"
            )
        return None


class StmtBase(Base):
    """
    ::

        [ [ label ] [ construct-name : ] ] stmt

    Attributes::

        item : readfortran.Line

    """

    def tofortran(self, tab="", isfix=None):
        label = None
        name = None
        if self.item is not None:
            label = self.item.label
            name = self.item.name
        if isfix:
            c = " "
        else:
            c = ""
        if label:
            t = c + str(label)
            if isfix:
                while len(t) < 6:
                    t += " "
            else:
                tab = tab[len(t) :] or " "
        else:
            # BUG allow for fixed format here
            t = ""
        if name:
            return t + tab + name + ":" + str(self)
        return t + tab + str(self)

    def get_end_label(self):
        return self.item.label


class EndStmtBase(StmtBase):
    """
    ::

        end-stmt-base = END [ stmt [ stmt-name] ]

    """

    @staticmethod
    def match(stmt_type, stmt_name, string, require_stmt_type=False):
        """
        Attempts to match the supplied string as a form of 'END xxx' statement.

        :param str stmt_type: the type of end statement (e.g. "do") that we \
            attempt to match.
        :param type stmt_name: a class which should be used to match against \
            the name should this statement be named (e.g. end subroutine sub).
        :param str string: the string to attempt to match.
        :param bool require_stmt_type: whether or not the string must contain \
            the type of the block that is ending.

        :returns: 2-tuple containing the matched end-statement type (if any) \
            and, optionally, an associated name or None if there is no match.
        :rtype: Optional[
                    Tuple[Optional[str],
                          Optional[:py:class:`fparser.two.Fortran2003.Name`]]]

        """
        start = string[:3].upper()
        if start != "END":
            # string doesn't begin with 'END'
            return None
        line = string[3:].lstrip()
        start = line[: len(stmt_type)].upper()
        if start:
            if start.replace(" ", "") != stmt_type.replace(" ", ""):
                # Not the correct type of 'END ...' statement.
                return None
            line = line[len(stmt_type) :].lstrip()
        else:
            if require_stmt_type:
                # No type was found but one is required.
                return None
            # Got a bare "END" and that is a valid match.
            return None, None
        if line:
            if stmt_name is None:
                # There is content after the 'end xxx' but this block isn't
                # named so we fail to match.
                return None
            # Attempt to match the content after 'end xxx' with the supplied
            # name class.
            return stmt_type, stmt_name(line)
        # Successful match with an unnamed 'end xxx'.
        return stmt_type, None

    def init(self, stmt_type, stmt_name):
        """
        Initialise this EndStmtBase object.

        :param str stmt_type: the type of statement, e.g. 'PROGRAM'.
        :param stmt_name: the name associated with the statement or None.
        :type stmt_name: :py:class:`fparser.two.Fortran2003.Name`

        """
        self.items = [stmt_type, stmt_name]

    def get_name(self):
        return self.items[1]

    def get_type(self):
        return self.items[0]

    def tostr(self):
        if self.items[1] is not None:
            return "END %s %s" % tuple(self.items)
        if self.items[0] is not None:
            return "END %s" % (self.items[0])
        return "END"

    def torepr(self):
        return "%s(%r, %r)" % (
            self.__class__.__name__,
            self.get_type(),
            self.get_name(),
        )

    def get_end_name(self):
        name = self.items[1]
        if name is not None:
            return name.string


def isalnum(c):
    return c.isalnum() or c == "_"


class WORDClsBase(Base):
    """Base class to support situations where there is a keyword which is
    optionally followed by further text, potentially separated by
    a double colon. For example::

        'program fred', or 'import :: a,b'

        WORD-cls is WORD [ [ :: ] cls ]

    """

    @staticmethod
    def match(keyword, cls, string, colons=False, require_cls=False):
        """Checks whether the content in string matches the expected
        WORDClsBase format with 'keyword' providing the keyword, 'cls'
        providing the following text, 'colons' specifying whether an
        optional double colon is allowed as a separator between the keyword
        and cls and 'require_cls' specifying whether cls must have
        content or not.

        Note, if the optional double colon is allowed and exists in the string
        then 1) cls must also have content i.e. it implies
        `require_cls=True` and 2) white space is not required between
        the keyword and the double colon and the double colon and cls.

        The simplest form of keyword pattern is a string. However this
        method can also match more complex patterns as specified by
        the Pattern class in pattern_tools.py. As patterns can be
        built from combinations of other patterns (again see
        pattern_tool.py) this method also supports a hierarchy of
        lists and/or tuples of patterns.

        :param keyword: the pattern of the WORD to match. This can be \
            a Pattern, string, list or tuple, with a list or tuple \
            containing one or more Pattern, string, list or tuple.
        :type keyword: :py:class:`fparser.two.pattern_tools.Pattern`, \
            str, tuple of str/Pattern/tuple/list or list of \
            str/Pattern/tuple/list
        :param cls: the class to match.
        :type cls: a subclass of :py:class:`fparser.two.utils.Base`
        :param str string: Text that we are trying to match.
        :param bool colons: whether a double colon is allowed as an optional \
            separator between between WORD and cls.
        :param bool require_cls: whether content for cls is required \
            or not.

        :returns: None if there is no match or, if there is a match, a \
            2-tuple containing a string matching the 'WORD' and an \
            instance of 'cls' (or None if an instance of cls is not \
            required and not provided).
        :rtype: Optional[Tupe[Str, Optional[Cls]]]

        """
        if isinstance(keyword, (tuple, list)):
            for child in keyword:
                try:
                    obj = WORDClsBase.match(
                        child, cls, string, colons=colons, require_cls=require_cls
                    )
                except NoMatchError:
                    obj = None
                if obj is not None:
                    return obj
            return None

        if isinstance(keyword, str):
            line = string.lstrip()
            if line[: len(keyword)].upper() != keyword.upper():
                return None
            line = line[len(keyword) :]
            pattern_value = keyword
        else:
            my_match = keyword.match(string)
            if my_match is None:
                return None
            line = string[len(my_match.group()) :]
            pattern_value = keyword.value

        if not line:
            if require_cls:
                # no text found but it is required
                return None
            return pattern_value, None
        if isalnum(line[0]):
            return None
        line = line.lstrip()
        has_colons = False
        if colons and line.startswith("::"):
            has_colons = True
            line = line[2:].lstrip()
        if not line:
            if has_colons or require_cls:
                # colons without following content is not allowed.
                return None
            return pattern_value, None
        if cls is None:
            return None
        return pattern_value, cls(line)

    def tostr(self):
        """Convert the class into Fortran.

        :return: String representation of this class without any \
                 optional double colon.
        :rtype: str

        """
        if self.items[1] is None:
            return str(self.items[0])
        s = str(self.items[1])
        if s and s[0] in "(*":
            return "%s%s" % (self.items[0], s)
        return "%s %s" % (self.items[0], s)

    def tostr_a(self):
        """Convert the class into Fortran, adding in the double colon.

        :return: String representation of this class including an \
                 optional double colon.
        :rtype: str

        """
        if self.items[1] is None:
            return str(self.items[0])
        return "%s :: %s" % (self.items[0], self.items[1])


class Type_Declaration_StmtBase(StmtBase):
    """
    ::

        type-declaration-stmt is declaration-type-spec [ [ ,
            attr-spec ]... :: ] entity-decl-list

    """

    subclass_names = []
    use_names = None  # derived class must define this list

    @staticmethod
    def match(decl_type_spec_cls, attr_spec_list_cls, entity_decl_list_cls, string):
        line, repmap = string_replace_map(string)
        i = line.find("::")
        if i != -1:
            j = line[:i].find(",")
            if j != -1:
                i = j
        else:
            if line[:6].upper() == "DOUBLE":
                m = re.search(r"\s[a-z_]", line[6:].lstrip(), re.I)
                if m is None:
                    return
                i = m.start() + len(line) - len(line[6:].lstrip())
            else:
                m = re.search(r"\s[a-z_]", line, re.I)
                if m is None:
                    return
                i = m.start()
        type_spec = decl_type_spec_cls(repmap(line[:i].rstrip()))
        if type_spec is None:
            return
        line = line[i:].lstrip()
        if line.startswith(","):
            i = line.find("::")
            if i == -1:
                return
            attr_specs = attr_spec_list_cls(repmap(line[1:i].strip()))
            if attr_specs is None:
                return
            line = line[i:]
        else:
            attr_specs = None
        if line.startswith("::"):
            line = line[2:].lstrip()
        entity_decls = entity_decl_list_cls(repmap(line))
        if entity_decls is None:
            return
        return type_spec, attr_specs, entity_decls

    def tostr(self):
        """
        :returns: the text representation of this node.
        :rtype: str
        """
        if self.items[1] is None:
            return f"{self.items[0]} :: {self.items[2]}"
        return f"{self.items[0]}, {self.items[1]} :: {self.items[2]}"


def walk(node_list, types=None, indent=0, debug=False):
    """
    Walk down the parse tree produced by fparser2.  Returns a list of all
    nodes with the specified type(s).

    :param node_list: node or list of nodes from which to walk.
    :type node_list: (list of) :py:class:fparser.two.utils.Base
    :param types: type or tuple of types of Node to return. (Default is to \
                  return all nodes.)
    :type types: type or tuple of types
    :param int indent: extent to which to indent debug output.
    :param bool debug: whether or not to write textual representation of AST \
                       to stdout.
    :returns: a list of nodes
    :rtype: `list` of :py:class:`fparser.two.utils.Base`
    """
    local_list = []

    if not isinstance(node_list, (list, tuple)):
        node_list = [node_list]

    for child in node_list:
        if debug:
            if isinstance(child, str):
                print(indent * "  " + "child type = ", type(child), repr(child))
            else:
                print(indent * "  " + "child type = ", type(child))
        if types is None or isinstance(child, types):
            local_list.append(child)
        # Recurse down
        if isinstance(child, Base):
            local_list += walk(child.children, types, indent + 1, debug)
        elif isinstance(child, tuple):
            for component in child:
                local_list += walk(component, types, indent + 1, debug)

    return local_list


def get_child(node, node_type):
    """
    Searches for the first, immediate child of the supplied node that is of
    the specified type.

    :param node: the node whose children will be searched.
    :type node: :py:class:`fparser.two.utils.Base`
    :param type node_type: the class of child node to search for.

    :returns: the first child node of type node_type that is encountered \
              or None.
    :rtype: :py:class:`fparser.two.utils.Base`

    """
    for child in node.children:
        if isinstance(child, node_type):
            return child
    return None
