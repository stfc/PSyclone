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
Fortran type-declaration statements.

"""

__all__ = [
    "Integer",
    "Real",
    "DoublePrecision",
    "Complex",
    "DoubleComplex",
    "Character",
    "Logical",
    "Byte",
    "TypeStmt",
    "Class",
    "intrinsic_type_spec",
    "declaration_type_spec",
    "Implicit",
]

import re
import string
from fparser.common.base_classes import (
    Statement,
    BeginStatement,
    EndStatement,
    AttributeHolder,
    Variable,
)
from fparser.common.utils import (
    split_comma,
    AnalyzeError,
    name_re,
    is_entity_decl,
    is_name,
    parse_array_spec,
)

# Intrinsic type specification statements


class TypeDeclarationStatement(Statement):
    """
    Class capturing various sorts of type declaration.

    ::

        <declaration-type-spec> [ [, <attr-spec>] :: ] <entity-decl-list>
        <declaration-type-spec> = <intrinsic-type-spec>
                                  | TYPE ( <derived-type-spec> )
                                  | CLASS ( <derived-type-spec> )
                                  | CLASS ( * )

        <derived-type-spec> = <type-name> [ ( <type-param-spec-list> ) ]
        <type-param-spec> = [ <keyword> = ] <type-param-value>
        <type-param-value> = <scalar-int-expr> | * | :

        <intrinsic-type-spec> = INTEGER [<kind-selector>]
                                | REAL [<kind-selector>]
                                | DOUBLE PRECISION
                                | COMPLEX [<kind-selector>]
                                | CHARACTER [<char-selector>]
                                | LOGICAL [<kind-selector>]

        <kind-selector> = ( [ KIND = ] <scalar-int-initialization-expr> )
        EXTENSION:
          <kind-selector> = ( [ KIND = ] <scalar-int-initialization-expr> )
                            | * <length>

        <char-selector> = <length-selector>
                          | ( LEN = <type-param-value>,
                              KIND = <scalar-int-initialization-expr> )
                          | ( <type-param-value>, [ KIND = ]
                              <scalar-int-initialization-expr> )
                          | ( KIND = <scalar-int-initialization-expr> [,
                              LEN = <type-param-value>] )
        <length-selector> = ( [ LEN = ] <type-param-value> )
                            | * <char-length> [ , ]
        <char-length> = ( <type-param-value> ) | <scalar-int-literal-constant>

        <attr-spec> = <access-spec> | ALLOCATABLE | ASYNCHRONOUS
                      | DIMENSION ( <array-spec> ) | EXTERNAL
                      | INTENT ( <intent-spec> ) | INTRINSIC
                      | <language-binding-spec> | OPTIONAL
                      | PARAMETER | POINTER | PROTECTED | SAVE
                      | TARGET | VALUE | VOLATILE
        <entity-decl> = <object-name> [ ( <array-spec> ) ] [ * <char-length> ]
                        [ <initialization> ]
                      | <function-name> [ * <char-length> ]
        <initialization> =  = <initialization-expr>
                            | => NULL
        <access-spec> = PUBLIC | PRIVATE
        <language-binding-spec> = BIND ( C [ , NAME =
                                         <scalar-char-initialization-expr>] )
        <array-spec> =   <explicit-shape-spec-list>
                       | <assumed-shape-spec-list>
                       | <deferred-shape-spec-list>
                       | <assumed-size-spec>
        <explicit-shape-spec> = [ <lower-bound> : ] <upper-bound>
        <assumed-shape-spec> = [ <lower-bound> ] :
        <deferred-shape-spec> = :
        <assumed-size-spec> = [ <explicit-shape-spec-list> , ] [ <lower-bound> : ] *
        <bound> = <specification-expr>

        <int-literal-constant> = <digit-string> [ _ <kind-param> ]
        <digit-string> = <digit> [ <digit> ]..
        <kind-param> = <digit-string> | <scalar-int-constant-name>

    """

    _repr_attr_names = [
        "selector",
        "attrspec",
        "entity_decls",
    ] + Statement._repr_attr_names

    def process_item(self):
        item = self.item
        apply_map = item.apply_map
        clsname = self.__class__.__name__.lower()
        line = item.get_line()
        from .block_statements import Function

        if not line.lower().startswith(clsname):
            i = 0
            j = 0
            for c in line:
                i += 1
                if c == " ":
                    continue
                j += 1
                if j == len(clsname):
                    break
            line = line[:i].replace(" ", "") + line[i:]

        assert line.lower().startswith(clsname), repr((line, clsname))
        line = line[len(clsname) :].lstrip()

        if line.startswith("("):
            i = line.find(")")
            selector = apply_map(line[: i + 1].strip())
            line = line[i + 1 :].lstrip()
        elif line.startswith("*"):
            selector = "*"
            line = line[1:].lstrip()
            if line.startswith("("):
                i = line.find(")")
                selector += apply_map(line[: i + 1].rstrip())
                line = line[i + 1 :].lstrip()
            else:
                m = re.match(r"\d+(_\w+|)|[*]", line)
                if not m:
                    self.isvalid = False
                    return
                i = m.end()
                selector += line[:i].rstrip()
                line = line[i:].lstrip()
        else:
            selector = ""

        fm = Function.match(line)
        if fm:
            l2 = line[: fm.end()]
            m2 = re.match(r".*?\b(?P<name>\w+)\Z", l2)
            if not m2:
                self.isvalid = False
                return
            fname = m2.group("name")
            fitem = item.copy(clsname + selector + " :: " + fname, apply_map=True)
            self.parent.put_item(fitem)
            item.clone(line)
            self.isvalid = False
            return

        if line.startswith(","):
            line = line[1:].lstrip()

        self.raw_selector = selector
        if isinstance(self, Character):
            self.selector = self._parse_char_selector(selector)
        else:
            self.selector = self._parse_kind_selector(selector)

        i = line.find("::")
        if i == -1:
            self.attrspec = []
            self.entity_decls = split_comma(line, self.item)
        else:
            self.attrspec = split_comma(line[:i].rstrip(), self.item)
            self.entity_decls = split_comma(line[i + 2 :].lstrip(), self.item)
        for entity in self.entity_decls:
            if not is_entity_decl(entity):
                self.isvalid = False
                return

        if isinstance(self.parent, Function) and self.parent.name in self.entity_decls:
            assert self.parent.typedecl is None, repr(self.parent.typedecl)
            self.parent.typedecl = self
            self.ignore = True
        if isinstance(self, Type):
            self.name = self.selector[1].lower()
            assert is_name(self.name), repr(self.name)
        else:
            self.name = clsname
        return

    def _parse_kind_selector(self, selector):
        if not selector:
            return "", ""
        length, kind = "", ""
        if selector.startswith("*"):
            length = selector[1:].lstrip()
        else:
            assert selector[0] + selector[-1] == "()", repr(selector)
            l = selector[1:-1].strip()
            if l.lower().startswith("kind"):
                l = l[4:].lstrip()
                if l[0] + l[-1] == "()":
                    kind = "kind" + l
                else:
                    assert l.startswith("="), repr(l)
                    kind = l[1:].lstrip()
            else:
                kind = l
        return length, kind

    def _split_char_selector(self, line):
        """line=``[key=]value`` -> key, value.
        If line does not have name part then return None, value.
        """
        for name in ["len", "kind"]:
            if line[: len(name)].lower() == name:
                value_part = line[len(name) :].lstrip()
                if value_part.startswith("="):
                    return name, value_part[1:].lstrip()
        return None, line

    def _parse_char_selector(self, selector):
        if not selector:
            return "", ""
        if selector.startswith("*"):
            l = selector[1:].lstrip()
            if l.startswith("("):
                if l.endswith(","):
                    l = l[:-1].rstrip()
                assert l.endswith(")"), repr(l)
                l = l[1:-1].strip()
                if l.lower().startswith("len"):
                    l = l[3:].lstrip()[1:].lstrip()
            kind = ""
        else:
            assert selector[0] + selector[-1] == "()", repr(selector)
            l = split_comma(selector[1:-1].strip(), self.item)
            if len(l) == 1:
                l = l[0]
                key, value = self._split_char_selector(l)
                if key == "len":
                    kind, l = "", value
                elif key == "kind":
                    kind, l = value, ""
                else:
                    kind = ""
            else:
                assert len(l) == 2, repr(l)
                key0, value0 = self._split_char_selector(l[0])
                key1, value1 = self._split_char_selector(l[1])
                if key0 == "len":
                    assert key1 in [None, "kind"], repr(key1)
                    l, kind = value0, value1
                elif key0 == "kind":
                    assert key1 == "len", repr(key1)
                    l, kind = value1, value0
                else:
                    assert key0 is None, repr(key0)
                    assert key1 in [None, "kind"], repr(key1)
                    l, kind = value0, value1
        return l, kind

    def tostr(self):
        """Create a text representation of this object and return it"""
        clsname = self.__class__.__name__.upper()
        text = ""
        length, kind = self.selector
        if isinstance(self, Character):
            if length and kind:
                text += "(LEN=%s, KIND=%s)" % (length, kind)
            elif length:
                text += "(LEN=%s)" % (length)
            elif kind:
                text += "(KIND=%s)" % (kind)
        elif isinstance(self, Type):
            text += "(%s)" % (kind)
        elif isinstance(self, Class):
            if kind:
                # For a class declaration, 'kind' is actually the class
                # that the variable is an instance of. Therefore there
                # is no "(KIND=xxx)", just (xxx).
                text += "({0})".format(kind)
        else:
            if length:
                text += "*%s" % (length)
            if kind:
                text += "(KIND=%s)" % (kind)

        return clsname + text

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        s = self.tostr()
        if self.attrspec:
            s += ", " + ", ".join(self.attrspec)
        # If we were to change fparser so that it always produces the
        # '::' separator then we'd simply comment-out the if below.
        if self.attrspec or "=" in str(self.entity_decls):
            s += " ::"
        if self.entity_decls:
            s += " " + ", ".join(self.entity_decls)
        return tab + s

    def __str__(self):
        return self.tofortran()

    def __eq__(self, other):
        if self.__class__ is not other.__class__:
            return False
        return self.selector == other.selector

    def astypedecl(self):
        if self.entity_decls or self.attrspec:
            return self.__class__(self.parent, self.item.copy(self.tostr()))
        return self

    def analyze(self):
        if not self.entity_decls:
            return
        variables = self.parent.a.variables
        typedecl = self.astypedecl()
        attrspec = self.attrspec[:]
        access_spec_lst = [a for a in attrspec if a.lower() in ["private", "public"]]
        if access_spec_lst:
            access_spec = access_spec_lst[0]
            attrspec.remove(access_spec)
        else:
            access_spec = None
        for item in self.entity_decls:
            name, array_spec, char_length, value = self._parse_entity(item)
            var = self.parent.get_variable(name)
            var.add_parent(self)
            if char_length:
                var.set_length(char_length)
            var.set_type(typedecl)
            var.update(self.attrspec)
            if array_spec:
                var.set_bounds(array_spec)
            if value:
                var.set_init(value)
            if access_spec is not None:
                l = getattr(self.parent.a, access_spec.lower() + "_id_list")
                l.append(name)
            var.analyze()
        return

    def _parse_entity(self, line):
        m = name_re(line)
        assert m, repr((line, self.item, self.__class__.__name__))
        name = line[: m.end()]
        line = line[m.end() :].lstrip()
        array_spec = None
        char_length = None
        value = None
        if line:
            item = self.item.copy(line)
            line = item.get_line()
            if line.startswith("("):
                i = line.find(")")
                assert i != -1, repr(line)
                array_spec = parse_array_spec(line[1:i].strip(), item)
                line = line[i + 1 :].lstrip()

            if line.startswith("*"):
                i = line.find("=")
                if i == -1:
                    char_length = item.apply_map(line[1:].lstrip())
                    line = ""
                else:
                    char_length = item.apply_map(line[1:i].strip())
                    line = line[i:]
            if line.startswith("="):
                value = item.apply_map(line[1:].lstrip())
        return name, array_spec, char_length, value

    def get_zero_value(self):
        raise NotImplementedError(repr(self.__class__.__name__))

    def assign_expression(self, name, value):
        return "%s = %s" % (name, value)

    def get_kind(self):
        return self.selector[1] or self.default_kind

    def get_length(self):
        return self.selector[0] or 1

    def get_byte_size(self):
        length, kind = self.selector
        if length:
            return int(length)
        if kind:
            return int(kind)
        return self.default_kind

    def is_intrinsic(self):
        return not isinstance(self, (Type, Class))

    def is_derived(self):
        return isinstance(self, Type)

    def is_numeric(self):
        return isinstance(
            self, (Integer, Real, DoublePrecision, Complex, DoubleComplex, Byte)
        )

    def is_nonnumeric(self):
        return isinstance(self, (Character, Logical))


class Integer(TypeDeclarationStatement):
    match = re.compile(r"integer\b", re.I).match
    default_kind = 4

    def get_zero_value(self):
        kind = self.get_kind()
        if kind == self.default_kind:
            return "0"
        return "0_%s" % (kind)


class Real(TypeDeclarationStatement):
    match = re.compile(r"real\b", re.I).match
    default_kind = 4

    def get_zero_value(self):
        kind = self.get_kind()
        if kind == self.default_kind:
            return "0.0"
        return "0_%s" % (kind)


class DoublePrecision(TypeDeclarationStatement):
    match = re.compile(r"double\s*precision\b", re.I).match
    default_kind = 8

    def get_byte_size(self):
        return self.default_kind

    def get_zero_value(self):
        return "0.0D0"


class Complex(TypeDeclarationStatement):
    match = re.compile(r"complex\b", re.I).match
    default_kind = 4

    def get_byte_size(self):
        length, kind = self.selector
        if length:
            return int(length)
        if kind:
            return 2 * int(kind)
        return 2 * self.default_kind

    def get_zero_value(self):
        kind = self.get_kind()
        if kind == self.default_kind:
            return "(0.0, 0.0)"
        return "(0.0_%s, 0.0_%s)" % (kind, kind)

    def get_part_typedecl(self):
        bz = self.get_byte_size() / 2
        return Real(self.parent, self.item.copy("REAL*%s" % (bz)))


class DoubleComplex(TypeDeclarationStatement):
    # not in standard
    match = re.compile(r"double\s*complex\b", re.I).match
    default_kind = 8

    def get_byte_size(self):
        return 2 * self.default_kind

    def get_zero_value(self):
        return "(0.0D0,0.0D0)"


class Logical(TypeDeclarationStatement):
    match = re.compile(r"logical\b", re.I).match
    default_kind = 4

    def get_zero_value(self):
        return ".FALSE."


class Character(TypeDeclarationStatement):
    match = re.compile(r"character\b", re.I).match
    default_kind = 1

    def get_zero_value(self):
        return "''"


class Byte(TypeDeclarationStatement):
    # not in standard
    match = re.compile(r"byte\b", re.I).match
    default_kind = 1

    def get_zero_value(self):
        return "0"


class Type(TypeDeclarationStatement):
    match = re.compile(r"type\s*\(", re.I).match

    def get_zero_value(self):
        type_decl = self.get_type_decl(self.name)
        component_names = type_decl.a.component_names
        components = type_decl.a.components
        l = []
        for name in component_names:
            var = components[name]
            l.append(var.typedecl.get_zero_value())
        return "%s(%s)" % (type_decl.name, ", ".join(l))

    def get_kind(self):
        # See 4.5.2, page 48
        raise NotImplementedError(repr(self.__class__.__name__))


TypeStmt = Type


class Class(TypeDeclarationStatement):
    match = re.compile(r"class\s*\(", re.I).match


class Implicit(Statement):
    """
    Class capturing various forms of IMPLICIT statement.

    ::

        IMPLICIT <implicit-spec-list>
        IMPLICIT NONE
        <implicit-spec> = <declaration-type-spec> ( <letter-spec-list> )
        <letter-spec> = <letter> [ - <letter> ]

    """

    match = re.compile(r"implicit\b", re.I).match

    letters = string.ascii_lowercase

    def process_item(self):
        line = self.item.get_line()[8:].lstrip()
        if line.lower() == "none":
            self.items = []
            return
        items = []
        for item in split_comma(line, self.item):
            i = item.find("(")
            assert i != -1 and item.endswith(")"), repr(item)
            specs = []
            for spec in split_comma(item[i + 1 : -1].strip(), self.item):
                if "-" in spec:
                    s, e = spec.lower().split("-")
                    s = s.strip()
                    e = e.strip()
                    assert s in self.letters and e in self.letters, repr((s, e))
                else:
                    e = s = spec.lower().strip()
                    assert s in self.letters, repr((s, e))
                specs.append((s, e))
            tspec = item[:i].rstrip()
            stmt = None
            for cls in declaration_type_spec:
                if cls.match(tspec):
                    stmt = cls(self, self.item.copy(tspec))
                    if stmt.isvalid:
                        break
            assert stmt is not None, repr((item, line))
            items.append((stmt, specs))
        self.items = items
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        if not self.items:
            return tab + "IMPLICIT NONE"
        l = []
        for stmt, specs in self.items:
            l1 = []
            for s, e in specs:
                if s == e:
                    l1.append(s)
                else:
                    l1.append(s + "-" + e)
            l.append("%s ( %s )" % (stmt.tostr(), ", ".join(l1)))
        return tab + "IMPLICIT " + ", ".join(l)

    def analyze(self):
        """
        Analyze the Implicit statments constructed by the parser and
        set-up the associated implicit_rules belonging to the parent
        of this object in the AST.
        """
        implicit_rules = self.parent.a.implicit_rules
        if not self.items:
            if implicit_rules:
                self.warning(
                    "overriding previously set implicit rule mapping"
                    " %r." % (implicit_rules)
                )
            self.parent.a.implicit_rules = None
            return
        if implicit_rules is None:
            self.warning("overriding previously set IMPLICIT NONE")
            self.parent.a.implicit_rules = implicit_rules = {}
        for stmt, specs in self.items:
            for start, end in specs:
                start_idx = string.ascii_lowercase.index(start.lower())
                end_idx = string.ascii_lowercase.index(end.lower())
                for lchar in string.ascii_lowercase[start_idx : end_idx + 1]:
                    implicit_rules[lchar] = stmt
        return


intrinsic_type_spec = [
    Integer,
    Real,
    DoublePrecision,
    Complex,
    DoubleComplex,
    Character,
    Logical,
    Byte,
]
declaration_type_spec = intrinsic_type_spec + [TypeStmt, Class]
