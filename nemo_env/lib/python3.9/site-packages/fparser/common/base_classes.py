# -*- coding: utf-8 -*-
# Modified work Copyright (c) 2017-2022 Science and Technology
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

"""
Base classes for all Fortran statement types
"""

__all__ = [
    "Statement",
    "BeginStatement",
    "EndStatement",
    "Variable",
    "AttributeHolder",
    "ProgramBlock",
]

import copy
import logging

from fparser.common.readfortran import Line, Comment
from fparser.common.utils import split_comma, specs_split_comma, is_int_literal_constant
from fparser.common.utils import classes, AnalyzeError


class AttributeHolder:
    # copied from symbolic.base module
    """
    Defines a object with predefined attributes. Only those attributes
    are allowed that are specified as keyword arguments of a constructor.
    When an argument is callable then the corresponding attribute will
    be read-only and set by the value the callable object returns.
    """

    def __init__(self, **kws):
        self._attributes = {}
        self._readonly = []
        for k, v in list(kws.items()):
            self._attributes[k] = v
            if callable(v):
                self._readonly.append(k)

    def __getattr__(self, name):
        if name not in self._attributes:
            message = "%s instance has no attribute %r, " + "expected attributes: %s"
            attributes = ", ".join(list(self._attributes.keys()))
            raise AttributeError(message % (self.__class__.__name__, name, attributes))
        value = self._attributes[name]
        if callable(value):
            value = value()
            self._attributes[name] = value
        return value

    def __setattr__(self, name, value):
        if name in ["_attributes", "_readonly"]:
            self.__dict__[name] = value
            return
        if name in self._readonly:
            message = "%s instance attribute %r is readonly"
            raise AttributeError(message % (self.__class__.__name__, name))
        if name not in self._attributes:
            message = "%s instance has no attribute %r, " + "expected attributes: %s"
            attributes = ",".join(list(self._attributes.keys()))
            raise AttributeError(message % (self.__class__.__name__, name, attributes))
        self._attributes[name] = value

    def isempty(self):
        for k in list(self._attributes.keys()):
            v = getattr(self, k)
            if v:
                return False
        return True

    def __repr__(self):
        return self.torepr()

    def torepr(self, depth=-1, tab=""):
        if depth == 0:
            return tab + self.__class__.__name__
        lines = [self.__class__.__name__ + ":"]
        ttab = tab + "    "
        for k in list(self._attributes.keys()):
            v = getattr(self, k)
            if v:
                if isinstance(v, list):
                    lines.append(ttab + "%s=<%s-list>" % (k, len(v)))
                elif isinstance(v, dict):
                    lines.append(ttab + "%s=<dict with keys %s>" % (k, list(v.keys())))
                else:
                    lines.append(ttab + "%s=<%s>" % (k, type(v)))
        return "\n".join(lines)

    def todict(self):
        d = {}
        for k in list(self._attributes.keys()):
            v = getattr(self, k)
            d[k] = v
        return d


def get_base_classes(cls):
    bases = ()
    for c in cls.__bases__:
        bases += get_base_classes(c)
    return bases + cls.__bases__ + (cls,)


class Variable(metaclass=classes):
    """
    Variable instance has attributes::

        name
        typedecl
        dimension
        attributes
        intent
        parent - Statement instances defining the variable

    """

    def __init__(self, parent, name):
        self.parent = parent
        self.parents = [parent]
        self.name = name
        self.typedecl = None
        self.dimension = None
        self.bounds = None
        self.length = None
        self.attributes = []
        self.intent = None
        self.bind = []
        self.check = []
        self.init = None

        # after calling analyze the following additional attributes are set:
        # .is_array:
        #    rank
        #    shape

    def __repr__(self):
        line = []
        for a in [
            "name",
            "typedecl",
            "dimension",
            "bounds",
            "length",
            "attributes",
            "intent",
            "bind",
            "check",
            "init",
        ]:
            v = getattr(self, a)
            if v:
                line.append("%s=%r" % (a, v))
        return "Variable: " + ", ".join(line)

    def get_typedecl(self):
        if self.typedecl is None:
            self.set_type(self.parent.get_type(self.name))
        return self.typedecl

    def add_parent(self, parent):
        if id(parent) not in list(map(id, self.parents)):
            self.parents.append(parent)
        self.parent = parent

    def set_type(self, typedecl):
        if self.typedecl is not None:
            if not self.typedecl == typedecl:
                self.parent.warning(
                    "variable %r already has type %s, "
                    "resetting to %s"
                    % (self.name, self.typedecl.tostr(), typedecl.tostr())
                )
        assert typedecl is not None
        self.typedecl = typedecl

    def set_init(self, expr):
        if self.init is not None:
            if not self.init == expr:
                self.parent.warning(
                    "variable %r already has initialization %r, "
                    "resetting to %r" % (self.name, self.expr, expr)
                )
        self.init = expr

    def set_dimension(self, dims):
        dims = [tuple(dim.split(":")) for dim in dims]
        dims = [tuple(map(str.strip, dim)) for dim in dims]
        if self.dimension is not None:
            if not self.dimension == dims:
                self.parent.warning(
                    "variable %r already has dimension %r, "
                    "resetting to %r" % (self.name, self.dimension, dims)
                )
        self.dimension = dims

    def set_bounds(self, bounds):
        if self.bounds is not None:
            if not self.bounds == bounds:
                self.parent.warning(
                    "variable %r already has bounds %r, "
                    "resetting to %r" % (self.name, self.bounds, bounds)
                )
        self.bounds = bounds

    def set_length(self, length):
        if self.length is not None:
            if not self.length == length:
                self.parent.warning(
                    "variable %r already has length %r, "
                    "resetting to %r" % (self.name, self.length, length)
                )
        self.length = length

    known_intent_specs = [
        "IN",
        "OUT",
        "INOUT",
        "CACHE",
        "HIDE",
        "COPY",
        "OVERWRITE",
        "CALLBACK",
        "AUX",
        "C",
        "INPLACE",
        "OUT=",
    ]

    def set_intent(self, intent):
        if self.intent is None:
            self.intent = []
        for i in intent:
            if i not in self.intent:
                if i not in self.known_intent_specs:
                    self.parent.warning(
                        "unknown intent-spec %r for %r" % (i, self.name)
                    )
                self.intent.append(i)

    known_attributes = [
        "PUBLIC",
        "PRIVATE",
        "ALLOCATABLE",
        "ASYNCHRONOUS",
        "EXTERNAL",
        "INTRINSIC",
        "OPTIONAL",
        "PARAMETER",
        "POINTER",
        "PROTECTED",
        "SAVE",
        "TARGET",
        "VALUE",
        "VOLATILE",
        "REQUIRED",
    ]

    def is_intent_in(self):
        # TODO Something hinky is going on here. self.intent is a list which
        #      doesn't make a lot of sense. How can a variable have more
        #      than one intent?
        # TODO Furthermore if no intent is specified the assumed intent is
        #      "inout". Below an explicit "inout" returns False but a None
        #      returns True.
        if not self.intent:
            return True
        if "HIDE" in self.intent:
            return False
        if "INPLACE" in self.intent:
            return False
        if "IN" in self.intent:
            return True
        if "OUT" in self.intent:
            return False
        if "INOUT" in self.intent:
            return False
        if "OUTIN" in self.intent:
            return False
        return True

    def is_intent_inout(self):
        if not self.intent:
            return False
        if "INOUT" in self.intent:
            if "IN" in self.intent or "HIDE" in self.intent or "INPLACE" in self.intent:
                message = "INOUT ignored in INPUT(%s)"
                self.warning(message % (", ".join(self.intent)))
                return False
            return True
        return False

    def is_intent_hide(self):
        if not self.intent:
            return False
        if "HIDE" in self.intent:
            return True
        if "OUT" in self.intent:
            return (
                "IN" not in self.intent
                and "INPLACE" not in self.intent
                and "INOUT" not in self.intent
            )
        return False

    def is_intent_inplace(self):
        return self.intent and "INPLACE" in self.intent

    def is_intent_out(self):
        return self.intent and "OUT" in self.intent

    def is_intent_c(self):
        return self.intent and "C" in self.intent

    def is_intent_cache(self):
        return self.intent and "CACHE" in self.intent

    def is_intent_copy(self):
        return self.intent and "COPY" in self.intent

    def is_intent_overwrite(self):
        return self.intent and "OVERWRITE" in self.intent

    def is_intent_callback(self):
        return self.intent and "CALLBACK" in self.intent

    def is_intent_aux(self):
        return self.intent and "AUX" in self.intent

    def is_private(self):
        if "PUBLIC" in self.attributes:
            return False
        if "PRIVATE" in self.attributes:
            return True
        return self.parent.parent.check_private(self.name)

    def is_public(self):
        return not self.is_private()

    def is_allocatable(self):
        return "ALLOCATABLE" in self.attributes

    def is_external(self):
        return "EXTERNAL" in self.attributes

    def is_intrinsic(self):
        return "INTRINSIC" in self.attributes

    def is_parameter(self):
        return "PARAMETER" in self.attributes

    def is_optional(self):
        return (
            "OPTIONAL" in self.attributes
            and "REQUIRED" not in self.attributes
            and not self.is_intent_hide()
        )

    def is_required(self):
        return self.is_optional() and not self.is_intent_hide()

    def is_pointer(self):
        return "POINTER" in self.attributes

    def is_array(self):
        return not not (self.bounds or self.dimension)

    def is_scalar(self):
        return not self.is_array()

    def update(self, *attrs):
        attributes = self.attributes
        if len(attrs) == 1 and isinstance(attrs[0], (tuple, list)):
            attrs = attrs[0]
        for attr in attrs:
            lattr = attr.lower()
            uattr = attr.upper()
            if lattr.startswith("dimension"):
                assert self.dimension is None, repr((self.dimension, attr))
                line = attr[9:].lstrip()
                assert line[0] + line[-1] == "()", repr(line)
                self.set_dimension(split_comma(line[1:-1].strip(), self.parent.item))
                continue
            if lattr.startswith("intent"):
                line = attr[6:].lstrip()
                assert line[0] + line[-1] == "()", repr(line)
                self.set_intent(
                    specs_split_comma(line[1:-1].strip(), self.parent.item, upper=True)
                )
                continue
            if lattr.startswith("bind"):
                line = attr[4:].lstrip()
                assert line[0] + line[-1] == "()", repr(line)
                self.bind = specs_split_comma(
                    line[1:-1].strip(), self.parent.item, upper=True
                )
                continue
            if lattr.startswith("check"):
                line = attr[5:].lstrip()
                assert line[0] + line[-1] == "()", repr(line)
                self.check.extend(split_comma(line[1:-1].strip(), self.parent.item))
                continue
            if uattr not in attributes:
                if uattr not in self.known_attributes:
                    self.parent.warning("unknown attribute %r" % (attr))
                attributes.append(uattr)

    def __str__(self):
        s = ""
        typedecl = self.get_typedecl()
        if typedecl is not None:
            s += typedecl.tostr() + " "
        a = self.attributes[:]
        if self.dimension is not None:
            dimensions = [":".join(spec) for spec in self.dimension]
            a.append("DIMENSION(%s)" % (", ".join(dimensions)))
        if self.intent is not None:
            a.append("INTENT(%s)" % (", ".join(self.intent)))
        if self.bind:
            a.append("BIND(%s)" % (", ".join(self.bind)))
        if self.check:
            a.append("CHECK(%s)" % (", ".join(self.check)))
        if a:
            s += ", " + ", ".join(a) + " :: "
        s += self.name
        if self.bounds:
            s += "(%s)" % (", ".join([":".join(spec) for spec in self.bounds]))
        if self.length:
            if is_int_literal_constant(self.length):
                s += "*%s" % (self.length)
            else:
                s += "*(%s)" % (self.length)
        if self.init:
            s += " = " + self.init
        return s

    def get_array_spec(self):
        assert self.is_array(), "array_spec is available only for arrays"
        if self.bounds:
            if self.dimension:
                message = (
                    "both bounds=%r and dimension=%r are defined, "
                    + "ignoring dimension."
                )
                self.parent.warning(message % (self.bounds, self.dimension))
            array_spec = self.bounds
        else:
            array_spec = self.dimension
        return array_spec

    def is_deferred_shape_array(self):
        if not self.is_array():
            return False
        return self.is_allocatable() or self.is_pointer()

    def is_assumed_size_array(self):
        if not self.is_array():
            return False
        return self.get_array_spec()[-1][-1] == "*"

    def is_assumed_shape_array(self):
        if not self.is_array():
            return False
        if self.is_deferred_shape_array():
            return False
        for spec in self.get_array_spec():
            if not spec[-1]:
                return True
        return False

    def is_explicit_shape_array(self):
        if not self.is_array():
            return False
        if self.is_deferred_shape_array():
            return False
        for spec in self.get_array_spec():
            if not spec[-1] or spec[-1] == "*":
                return False
        return True

    def is_allocatable_array(self):
        return self.is_array() and self.is_allocatable()

    def is_array_pointer(self):
        return self.is_array() and self.is_pointer()

    def analyze(self):
        typedecl = self.get_typedecl()
        if self.is_array():
            array_spec = self.get_array_spec()
            self.rank = len(array_spec)
            if self.is_deferred_shape_array():  # a(:, :)
                pass
            elif self.is_explicit_shape_array():
                shape = []
                for spec in array_spec:
                    if len(spec) == 1:
                        shape.append(spec[0].replace(" ", ""))
                    else:
                        try:
                            # lower subscript
                            lss = int(spec[0].replace(" ", ""))
                            # upper subscript
                            uss = int(spec[1].replace(" ", ""))
                            n = uss - (lss - 1)
                        except ValueError:
                            n = "(%s)-(%s)" % (spec[1], spec[0])
                        shape.append(str(n))
                self.shape = shape

    def error(self, message):
        return self.parent.error(message)

    def warning(self, message):
        return self.parent.warning(message)

    def info(self, message):
        return self.parent.info(message)


class ProgramBlock(metaclass=classes):
    pass


class Statement(metaclass=classes):
    """
    Statement instance has attributes::

        parent  - Parent BeginStatement or FortranParser instance
        item    - Line instance containing the statement line
        isvalid - boolean, when False, the Statement instance will be ignored

    """

    modes = ["free", "fix", "f77", "pyf"]
    _repr_attr_names = []

    def __init__(self, parent, item):
        self.parent = parent
        if item is not None:
            self.reader = item.reader
        else:
            self.reader = parent.reader
        self.top = getattr(parent, "top", None)  # the top of statement tree
        self.item = item

        if isinstance(parent, ProgramBlock):
            self.programblock = parent
        elif isinstance(self, ProgramBlock):
            self.programblock = self
        elif hasattr(parent, "programblock"):
            self.programblock = parent.programblock
        else:
            pass

        # When a statement instance is constructed by error, set isvalid to
        # False
        self.isvalid = True
        # when a statement should be ignored, set ignore to True
        self.ignore = False

        # attribute a will hold analyze information.
        a_dict = {}
        for cls in get_base_classes(self.__class__):
            if hasattr(cls, "a"):
                a_dict.update(copy.deepcopy(cls.a.todict()))
        self.a = AttributeHolder(**a_dict)
        if hasattr(self.__class__, "a"):
            assert self.a is not self.__class__.a

        self.process_item()

    def __repr__(self):
        return self.torepr()

    def torepr(self, depth=-1, incrtab=""):
        tab = incrtab + self.get_indent_tab()
        clsname = self.__class__.__name__
        lines = [tab + clsname]
        if depth == 0:
            return "\n".join(lines)
        ttab = tab + "  "
        for n in self._repr_attr_names:
            attr = getattr(self, n, None)
            if not attr:
                continue
            if hasattr(attr, "torepr"):
                r = attr.torepr(depth - 1, incrtab)
            else:
                r = repr(attr)
            lines.append(ttab + "%s=%s" % (n, r))
        if self.item is not None:
            lines.append(ttab + "item=%r" % (self.item))
        if not self.isvalid:
            lines.append(ttab + "isvalid=%r" % (self.isvalid))
        if self.ignore:
            lines.append(ttab + "ignore=%r" % (self.ignore))
        if not self.a.isempty():
            lines.append(
                ttab + "a=" + self.a.torepr(depth - 1, incrtab + "  ").lstrip()
            )
        return "\n".join(lines)

    def get_indent_tab(self, deindent=False, isfix=None):
        if isfix is None:
            isfix = self.reader.format.is_fixed
        if isfix:
            tab = " " * 6
        else:
            tab = ""
        p = self.parent
        while isinstance(p, Statement):
            tab += "  "
            p = p.parent
        if deindent:
            tab = tab[:-2]
        label = getattr(self.item, "label", None)
        if label is None:
            return tab
        s = str(label)
        if isfix:
            s = " " + s
        tab = tab[len(s) :]
        if not tab:
            tab = " "
        tab = s + tab
        return tab

    def __str__(self):
        return self.tofortran()

    def asfix(self):
        lines = []
        for line in self.tofortran(isfix=True).split("\n"):
            if len(line) > 72 and line[0] == " ":
                lines.append(line[:72] + "&\n     &")
                line = line[72:]
                while len(line) > 66:
                    lines.append(line[:66] + "&\n     &")
                    line = line[66:]
                lines.append(line + "\n")
            else:
                lines.append(line + "\n")
        return "".join(lines).replace("\n     &\n", "\n")

    def format_message(self, kind, message):
        if self.item is not None:
            message = self.reader.format_message(
                kind, message, self.item.span[0], self.item.span[1]
            )
        else:
            return message
        return message

    # def show_message(self, message, stream=sys.stderr):
    #     print >> stream, message
    #     stream.flush()
    #     return

    def error(self, message):
        message = self.format_message("ERROR", message)
        logging.getLogger(__name__).error(message)

    def warning(self, message):
        message = self.format_message("WARNING", message)
        logging.getLogger(__name__).warning(message)

    def info(self, message):
        message = self.format_message("INFO", message)
        logging.getLogger(__name__).info(message)

    def analyze(self):
        self.warning("nothing analyzed")

    def get_variable(self, name):
        """Return Variable instance of variable name."""
        mth = getattr(self, "get_variable_by_name", self.parent.get_variable)
        return mth(name)

    def get_type(self, name):
        """Return type declaration using implicit rules
        for name.
        """
        mth = getattr(self, "get_type_by_name", self.parent.get_type)
        return mth(name)

    def get_type_decl(self, kind):
        mth = getattr(self, "get_type_decl_by_kind", self.parent.get_type_decl)
        return mth(kind)

    def get_provides(self):
        """
        Returns dictonary containing statements that block provides or None
        when N/A.
        """
        return None


class BeginStatement(Statement):
    """
    ::

        [ construct_name : ] <blocktype> [ <name> ]

    BeginStatement instances have additional attributes::

        name
        blocktype

    Block instance has attributes::

        content - list of Line or Statement instances
        name    - name of the block, unnamed blocks are named
                  with the line label
        construct_name - name of a construct
        parent  - Block or FortranParser instance
        item    - Line instance containing the block start statement
        get_item, put_item - methods to retrive/submit Line instances
                  from/to Fortran reader.
        isvalid - boolean, when False, the Block instance will be ignored.

        stmt_cls, end_stmt_cls

    """

    _repr_attr_names = [
        "blocktype",
        "name",
        "construct_name",
    ] + Statement._repr_attr_names

    def __init__(self, parent, item=None):
        self.content = []
        self.get_item = parent.get_item  # get line function
        self.put_item = parent.put_item  # put line function
        if not hasattr(self, "blocktype"):
            self.blocktype = self.__class__.__name__.lower()
        if not hasattr(self, "name"):
            # process_item may change this
            self.name = "__" + self.blocktype.upper() + "__"
        self.construct_name = getattr(item, "name", None)
        Statement.__init__(self, parent, item)

    def tostr(self):
        return self.blocktype.upper() + " " + self.name

    def tofortran(self, isfix=None):
        construct_name = self.construct_name
        construct_name = construct_name + ": " if construct_name else ""
        lines = [self.get_indent_tab(isfix=isfix) + construct_name + self.tostr()]
        for c in self.content:
            lines.append(c.tofortran(isfix=isfix))
        return "\n".join(lines)

    def torepr(self, depth=-1, incrtab=""):
        tab = incrtab + self.get_indent_tab()
        ttab = tab + "  "
        lines = [Statement.torepr(self, depth=depth, incrtab=incrtab)]
        if depth == 0 or not self.content:
            return "\n".join(lines)
        lines.append(ttab + "content:")
        for c in self.content:
            if isinstance(c, EndStatement):
                lines.append(c.torepr(depth - 1, incrtab))
            else:
                lines.append(c.torepr(depth - 1, incrtab + "  "))
        return "\n".join(lines)

    def process_item(self):
        """Process the line"""
        item = self.item
        if item is None:
            return
        self.fill()

    def fill(self, end_flag=False):
        """
        Fills blocks content until the end of block statement.
        """

        mode = self.reader.format.mode
        class_list = self.get_classes()
        self.classes = [cls for cls in class_list if mode in cls.modes]
        self.pyf_classes = [cls for cls in class_list if "pyf" in cls.modes]

        item = self.get_item()
        while item is not None:
            if isinstance(item, Line):
                if self.process_subitem(item):
                    end_flag = True
                    break
            elif isinstance(item, Comment):
                # TODO: FIX ME, Comment content is a string
                self.content.append(classes.Comment(self, item))
            else:
                raise NotImplementedError(repr(item))
            item = self.get_item()

        if not end_flag:
            self.warning("failed to find the end of block")

    def process_subitem(self, item):
        """
        Check if item is blocks start statement, if it is, read the block.

        Return True to stop adding items to given block.
        """
        line = item.get_line()

        # First check for the end of block
        cls = self.end_stmt_cls
        if cls.match(line):
            stmt = cls(self, item)
            if stmt.isvalid:
                self.content.append(stmt)
                return True

        if item.is_f2py_directive:
            classes = self.pyf_classes
        else:
            classes = self.classes

        # Look for statement match
        for cls in classes:
            if cls.match(line):
                stmt = cls(self, item)
                if stmt.isvalid:
                    if not stmt.ignore:
                        self.content.append(stmt)
                    return False
                # item may be cloned that changes the items line:
                line = item.get_line()

        # Check if f77 code contains inline comments or other f90
        # constructs that got undetected by get_source_info.
        if item.reader.format.is_f77:
            i = line.find("!")
            if i != -1:
                message = item.reader.format_message(
                    "WARNING",
                    'no parse pattern found for "%s" in %r block, '
                    "trying to remove inline comment (not in Fortran 77)."
                    % (item.get_line(), self.__class__.__name__),
                    item.span[0],
                    item.span[1],
                )
                # .. but at the expense of loosing the comment.
                logging.getLogger(__name__).warning(message)
                if line[:i]:
                    newitem = item.copy(line[:i].rstrip())
                    return self.process_subitem(newitem)
                else:
                    return True

            # try fix statement classes
            f77_classes = self.classes
            classes = []
            for cls in self.get_classes():
                if "f77" in cls.modes and cls not in f77_classes:
                    classes.append(cls)
            if classes:
                message = item.reader.format_message(
                    "WARNING",
                    'no parse pattern found for "%s" in %r block'
                    " maybe due to strict f77 mode."
                    " Trying f90 fix mode patterns.."
                    % (item.get_line(), self.__class__.__name__),
                    item.span[0],
                    item.span[1],
                )
                logging.getLogger(__name__).warning(message)

                item.reader.set_mode(False, False)
                self.classes = classes

                r = BeginStatement.process_subitem(self, item)
                if r is None:
                    # restore f77 fix mode
                    self.classes = f77_classes
                    item.reader.set_mode(False, True)
                else:
                    message = item.reader.format_message(
                        "INFORMATION",
                        "The f90 fix mode resolved the parse pattern issue."
                        " Setting reader to f90 fix mode.",
                        item.span[0],
                        item.span[1],
                    )
                    logging.getLogger(__name__).info(message)
                    # set f90 fix mode
                    self.classes = f77_classes + classes
                    self.reader.set_mode(False, False)
                return r

        self.handle_unknown_item_and_raise(item)

    def handle_unknown_item_and_raise(self, item):
        """Called when process_subitem does not find a start or end of block.
        It adds the item (which is an instance of Line) to the content, but
        then raises an AnalyzeError. An instance of Line in content typically
        results in other errors later (e.g. because Line has no analyze
        method).
        """
        message = item.reader.format_message(
            "WARNING",
            'no parse pattern found for "%s" in %r block.'
            % (item.get_line(), self.__class__.__name__),
            item.span[0],
            item.span[1],
        )
        logging.getLogger(__name__).warning(message)
        self.content.append(item)
        raise AnalyzeError(message)

    def analyze(self):
        for stmt in self.content:
            stmt.analyze()


class EndStatement(Statement):
    """
    END [<blocktype> [<name>]]

    EndStatement instances have additional attributes::

        name
        blocktype

    """

    _repr_attr_names = ["blocktype", "name"] + Statement._repr_attr_names

    def __init__(self, parent, item):
        if not hasattr(self, "blocktype"):
            self.blocktype = self.__class__.__name__.lower()[3:]
        Statement.__init__(self, parent, item)

    def process_item(self):
        item = self.item
        line = item.get_line().replace(" ", "")[3:]
        line = item.apply_map(line)
        blocktype = self.blocktype

        if line.lower().startswith(blocktype):
            line = line[len(blocktype) :].strip()
        else:
            if line:
                # not the end of expected block
                line = ""
                self.isvalid = False
        if self.parent.construct_name:
            name = self.parent.construct_name
        else:
            name = self.parent.name
        if line:
            # line variable is already cast to lower case so would fail if any
            # upper case letters exist in the label. Also, fortran is case
            # insensitive anyway so we should assume labels may have a
            # different case and therefore cast both to the same case in our
            # equivalence test.
            if line.lower() != name.lower():
                message = (
                    "expected the end of %r block " + "but got the end of %r, skipping."
                )
                self.warning(message % (name, line))
                self.isvalid = False
        self.name = name

    def analyze(self):
        pass

    def get_indent_tab(self, deindent=False, isfix=None):
        return Statement.get_indent_tab(self, deindent=True, isfix=isfix)

    def tofortran(self, isfix=None):
        """Returns a valid Fortran string for this END statement. It
        guarantees that there is no white space after the 'END' in case
        of an unnamed statement.

        :param bool isfix: True if the code is in fixed format.

        :returns: the (named or unnamed) valid Fortran END statement \
                  as a string.
        :rtype: str

        """
        if self.name:
            return self.get_indent_tab(isfix=isfix) + "END {0} {1}".format(
                self.blocktype.upper(), self.name
            )

        # Make sure there is no space after an unnamed END:
        return self.get_indent_tab(isfix=isfix) + "END {0}".format(
            self.blocktype.upper()
        )
