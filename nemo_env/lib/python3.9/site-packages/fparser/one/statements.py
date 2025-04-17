# Modified work Copyright (c) 2017-2022 Science and Technology
# Facilities Council
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
Fortran single line statements.

"""

import re
import sys

from fparser.common.base_classes import Statement

# Auxiliary tools

from fparser.common.utils import (
    split_comma,
    specs_split_comma,
    AnalyzeError,
    ParseError,
    parse_bind,
    parse_result,
    is_name,
    extract_bracketed_list_items,
)
from fparser.common.utils import classes

__all__ = [
    "GeneralAssignment",
    "Assignment",
    "PointerAssignment",
    "Assign",
    "Call",
    "Goto",
    "ComputedGoto",
    "AssignedGoto",
    "Continue",
    "Return",
    "Stop",
    "Print",
    "Read",
    "Read0",
    "Read1",
    "Write",
    "Flush",
    "Wait",
    "Contains",
    "Allocate",
    "Deallocate",
    "ModuleProcedure",
    "Access",
    "Public",
    "Private",
    "Close",
    "Cycle",
    "Backspace",
    "Endfile",
    "Rewind",
    "Open",
    "Format",
    "Save",
    "Data",
    "Nullify",
    "Use",
    "Exit",
    "Parameter",
    "Equivalence",
    "Dimension",
    "Target",
    "Pointer",
    "Protected",
    "Volatile",
    "Value",
    "ArithmeticIf",
    "Intrinsic",
    "Inquire",
    "Sequence",
    "External",
    "Namelist",
    "Common",
    "Optional",
    "Intent",
    "Entry",
    "Import",
    "ForallStmt",
    "SpecificBinding",
    "GenericBinding",
    "FinalBinding",
    "Allocatable",
    "Asynchronous",
    "Bind",
    "Else",
    "ElseIf",
    "Case",
    "TypeIs",
    "ClassIs",
    "WhereStmt",
    "ElseWhere",
    "Enumerator",
    "FortranName",
    "Threadsafe",
    "Depend",
    "Check",
    "CallStatement",
    "CallProtoArgument",
    "Pause",
    "Comment",
]


class StatementWithNamelist(Statement):
    """
    <statement> [ :: ] <name-list>
    """

    def process_item(self):
        if self.item.has_map():
            self.isvalid = False
            return
        if hasattr(self, "stmtname"):
            clsname = self.stmtname
        else:
            clsname = self.__class__.__name__
        line = self.item.get_line()[len(clsname) :].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = items = []
        for item in split_comma(line):
            if not is_name(item):
                self.isvalid = False
                return
            items.append(item)
        return

    def tofortran(self, isfix=None):
        if hasattr(self, "stmtname"):
            clsname = self.stmtname.upper()
        else:
            clsname = self.__class__.__name__.upper()
        s = ", ".join(self.items)
        if s:
            s = " " + s
        return self.get_indent_tab(isfix=isfix) + clsname + s


# Execution statements


class GeneralAssignment(Statement):
    """
    <variable> = <expr>
    <pointer variable> => <expr>
    """

    match = re.compile(r"\w[^=]*\s*=\>?").match
    item_re = re.compile(
        r"(?P<variable>\w[^=]*)\s*(?P<sign>=\>?)\s*(?P<expr>.*)\Z", re.I
    ).match
    _repr_attr_names = ["variable", "sign", "expr"] + Statement._repr_attr_names

    def process_item(self):
        m = self.item_re(self.item.get_line())
        if not m:
            self.isvalid = False
            return
        self.sign = sign = m.group("sign")
        if isinstance(self, Assignment) and sign != "=":
            self.isvalid = False
            return
        elif isinstance(self, PointerAssignment) and sign != "=>":
            self.isvalid = False
            return
        else:
            if sign == "=>":
                self.__class__ = PointerAssignment
            else:
                self.__class__ = Assignment
        apply_map = self.item.apply_map
        v1 = v = m.group("variable").replace(" ", "")
        while True:
            i = v.find(")")
            if i == -1:
                break
            v = v[i + 1 :]
            if v.startswith("(") or v.startswith(r"%"):
                continue
            if v:
                self.isvalid = False
                return
        self.variable = apply_map(v1)
        self.expr = apply_map(m.group("expr"))
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "%s %s %s" % (
            self.variable,
            self.sign,
            self.expr,
        )

    def analyze(self):
        return


class Assignment(GeneralAssignment):
    pass


class PointerAssignment(GeneralAssignment):
    pass


class Assign(Statement):
    """
    ASSIGN <label> TO <int-variable-name>
    """

    modes = ["fix77"]
    match = re.compile(r"assign\s*\d+\s*to\s*\w+\s*\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[6:].lstrip()
        i = line.lower().find("to")
        assert not self.item.has_map()
        self.items = [line[:i].rstrip(), line[i + 2 :].lstrip()]
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "ASSIGN %s TO %s" % (
            self.items[0],
            self.items[1],
        )

    def analyze(self):
        return


class Call(Statement):
    """
    Call statement class::

        CALL <procedure-designator> [ ( [ <actual-arg-spec-list> ] ) ]

        <procedure-designator> = <procedure-name>
                               | <proc-component-ref>
                               | <data-ref> % <binding-name>

        <actual-arg-spec> = [ <keyword> = ] <actual-arg>
        <actual-arg> = <expr>
                     | <variable>
                     | <procedure-name>
                     | <proc-component-ref>
                     | <alt-return-spec>
        <alt-return-spec> = * <label>

        <proc-component-ref> = <variable> % <procedure-component-name>

        <variable> = <designator>

    """

    # As indicated in the specification above, a call to a subroutine
    # that has no arguments does *not* require parentheses.
    # e.g.:
    #   call bob
    # is valid Fortran.
    match = re.compile(r"call\b\s*\w([\s\w\(\)\%]*\w)?\s*(\(.*\))?\s*$", re.I).match

    def process_item(self):
        """Parse the string containing the Call and store the
        designator and list of arguments (if any)"""
        item = self.item
        apply_map = item.apply_map
        line = item.get_line()[4:].strip()
        # Work backwards from the end of the line in order to allow
        # for code like:
        #     call my_type(1)%my_function(arg(2))
        # The following code will also support something like:
        #     call my_type(1)%my_function("(")
        # because fparser will previously have identified the "(" as a
        # string expression and replaced it with something like
        # "F2PY_EXPR_TUPLE_2"
        if line.endswith(")"):
            # Work back down the line until we find the matching '('
            i = len(line) - 2
            nopen = 1
            while i > 0:
                if line[i] == ")":
                    nopen += 1
                elif line[i] == "(":
                    nopen -= 1
                if nopen == 0:
                    # Have found the matching '(' at position i
                    break
                i -= 1
            if i <= 0:
                # Have reached the beginning of the string without
                # finding the matching '('
                self.isvalid = False
                return
            self.designator = apply_map(line[:i]).strip()
            items = split_comma(line[i + 1 : -1], item)
        else:
            # Call has no argument list
            items = []
            self.designator = apply_map(line).strip()
        self.items = items
        return

    def tofortran(self, isfix=None):
        """Returns the Fortran representation of this object as a string"""
        txt = self.get_indent_tab(isfix=isfix) + "CALL " + str(self.designator)
        if self.items:
            txt += "(" + ", ".join(map(str, self.items)) + ")"
        return txt

    def analyze(self):
        a = self.programblock.a
        variables = a.variables
        if hasattr(a, "external"):
            external = a.external
            if self.designator in external:
                print("Need to analyze:", self, file=sys.stderr)
        return


class Goto(Statement):
    """
    GO TO <label>
    """

    match = re.compile(r"go\s*to\s*\d+\s*\Z", re.I).match

    def process_item(self):
        assert not self.item.has_map()
        self.label = self.item.get_line()[2:].lstrip()[2:].lstrip()
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "GO TO %s" % (self.label)

    def analyze(self):
        return


class ComputedGoto(Statement):
    """
    GO TO ( <label-list> ) [ , ] <scalar-int-expr>
    """

    match = re.compile(r"go\s*to\s*\(", re.I).match

    def process_item(self):
        apply_map = self.item.apply_map
        line = self.item.get_line()[2:].lstrip()[2:].lstrip()
        i = line.index(")")
        self.items = split_comma(line[1:i], self.item)
        line = line[i + 1 :].lstrip()
        if line.startswith(","):
            line = line[1:].lstrip()
        self.expr = apply_map(line)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "GO TO (%s) %s" % (
            ", ".join(self.items),
            self.expr,
        )

    def analyze(self):
        return


class AssignedGoto(Statement):
    """
    GO TO <int-variable-name> [ ( <label> [ , <label> ]... ) ]
    """

    modes = ["fix77"]
    match = re.compile(r"go\s*to\s*\w+\s*\(?", re.I).match

    def process_item(self):
        line = self.item.get_line()[2:].lstrip()[2:].lstrip()
        i = line.find("(")
        if i == -1:
            self.varname = line
            self.items = []
            return
        self.varname = line[:i].rstrip()
        assert line[-1] == ")", repr(line)
        self
        self.items = split_comma(line[i + 1 : -1], self.item)
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        if self.items:
            return tab + "GO TO %s (%s)" % (self.varname, ", ".join(self.items))
        return tab + "GO TO %s" % (self.varname)

    def analyze(self):
        return


class Continue(Statement):
    """
    CONTINUE
    """

    match = re.compile(r"continue\Z", re.I).match

    def process_item(self):
        self.label = self.item.label
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(deindent=True) + "CONTINUE"

    def analyze(self):
        return


class Return(Statement):
    """
    RETURN [ <scalar-int-expr> ]
    """

    match = re.compile(r"return\b", re.I).match

    def process_item(self):
        self.expr = self.item.apply_map(self.item.get_line()[6:].lstrip())
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        if self.expr:
            return tab + "RETURN %s" % (self.expr)
        return tab + "RETURN"

    def analyze(self):
        return


class Stop(Statement):
    """
    STOP [ <stop-code> ]
    <stop-code> = <scalar-char-constant> | <1-5-digit>
    """

    match = re.compile(r'stop\s*((\'\w*\'|"\w*")+|\d+|)\Z', re.I).match

    def process_item(self):
        self.code = self.item.apply_map(self.item.get_line()[4:].lstrip())
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        if self.code:
            return tab + "STOP %s" % (self.code)
        return tab + "STOP"

    def analyze(self):
        return


class Print(Statement):
    """
    ::

        PRINT <format> [, <output-item-list>]
        <format> = <default-char-expr> | <label> | *

        <output-item> = <expr> | <io-implied-do>
        <io-implied-do> = ( <io-implied-do-object-list> , <implied-do-control> )
        <io-implied-do-object> = <input-item> | <output-item>
        <implied-do-control> = <do-variable>
                             = <scalar-int-expr> ,
                               <scalar-int-expr> [ , <scalar-int-expr> ]
        <input-item> = <variable> | <io-implied-do>

    """

    match = re.compile(r"print\s*(\'\w*\'|\"\w*\"|\d+|[*]|\b\w)", re.I).match

    def process_item(self):
        item = self.item
        apply_map = item.apply_map
        line = item.get_line()[5:].lstrip()
        items = split_comma(line, item)
        self.format = items[0]
        self.items = items[1:]
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "PRINT %s" % (
            ", ".join([self.format] + self.items)
        )

    def analyze(self):
        return


class Read(Statement):
    """
    Read0:    READ ( <io-control-spec-list> ) [ <input-item-list> ]

        <io-control-spec-list> = [ UNIT = ] <io-unit>
                                 | [ FORMAT = ] <format>
                                 | [ NML = ] <namelist-group-name>
                                 | ADVANCE = <scalar-default-char-expr>
                                 ...

    Read1:    READ <format> [, <input-item-list>]
        <format> == <default-char-expr> | <label> | *
    """

    match = re.compile(r'read\b\s*[\w(*\'"]', re.I).match

    def process_item(self):
        item = self.item
        line = item.get_line()[4:].lstrip()
        if line.startswith("("):
            self.__class__ = Read0
        else:
            self.__class__ = Read1
        self.process_item()
        return

    def analyze(self):
        return


class Read0(Read):
    def process_item(self):
        item = self.item
        line = item.get_line()[4:].lstrip()
        i = line.find(")")
        self.specs = specs_split_comma(line[1:i], item)
        self.items = split_comma(line[i + 1 :], item)
        return

    def tofortran(self, isfix=None):
        s = self.get_indent_tab(isfix=isfix) + "READ (%s)" % (", ".join(self.specs))
        if self.items:
            return s + " " + ", ".join(self.items)
        return s


class Read1(Read):
    def process_item(self):
        item = self.item
        line = item.get_line()[4:].lstrip()
        items = split_comma(line, item)
        self.format = items[0]
        self.items = items[1:]
        return

    def tofortran(self, isfix=None):
        return (
            self.get_indent_tab(isfix=isfix)
            + "READ "
            + ", ".join([self.format] + self.items)
        )


class Write(Statement):
    """
    WRITE ( io-control-spec-list ) [<output-item-list>]
    """

    match = re.compile(r"write\s*\(", re.I).match

    def process_item(self):
        item = self.item
        line = item.get_line()[5:].lstrip()
        i = line.find(")")
        assert i != -1, repr(line)
        self.specs = specs_split_comma(line[1:i], item)
        self.items = split_comma(line[i + 1 :], item)
        return

    def tofortran(self, isfix=None):
        s = self.get_indent_tab(isfix=isfix) + "WRITE (%s)" % ", ".join(self.specs)
        if self.items:
            s += " " + ", ".join(self.items)
        return s

    def analyze(self):
        return


class Flush(Statement):
    """
    ::

        FLUSH <file-unit-number>
        FLUSH ( <flush-spec-list> )
        <flush-spec> = [ UNIT = ] <file-unit-number>
                     | IOSTAT = <scalar-int-variable>
                     | IOMSG = <iomsg-variable>
                     | ERR = <label>
    """

    match = re.compile(r"flush\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[5:].lstrip()
        if not line:
            self.isvalid = False
            return
        if line.startswith("("):
            assert line[-1] == ")", repr(line)
            self.specs = specs_split_comma(line[1:-1], self.item)
        else:
            self.specs = specs_split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        return tab + "FLUSH (%s)" % (", ".join(self.specs))

    def analyze(self):
        return


class Wait(Statement):
    """
    ::

        WAIT ( <wait-spec-list> )
        <wait-spec> = [ UNIT = ] <file-unit-number>
                    | END = <label>
                    | EOR = <label>
                    | ERR = <label>
                    | ID = <scalar-int-expr>
                    | IOMSG = <iomsg-variable>
                    | IOSTAT = <scalar-int-variable>

    """

    match = re.compile(r"wait\s*\(.*\)\Z", re.I).match

    def process_item(self):
        self.specs = specs_split_comma(
            self.item.get_line()[4:].lstrip()[1:-1], self.item
        )
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        return tab + "WAIT (%s)" % (", ".join(self.specs))

    def analyze(self):
        return


class Contains(Statement):
    """
    CONTAINS
    """

    match = re.compile(r"contains\Z", re.I).match

    def process_item(self):
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "CONTAINS"


class Allocate(Statement):
    """
    ::

        ALLOCATE ( [ <type-spec> :: ] <allocation-list> [ , <alloc-opt-list> ] )
        <alloc-opt> = STAT = <stat-variable>
                    | ERRMSG = <errmsg-variable>
                    | SOURCE = <source-expr>
        <allocation> = <allocate-object> [ ( <allocate-shape-spec-list> ) ]
    """

    match = re.compile(r"allocate\s*\(.*\)\Z", re.I).match

    def process_item(self):
        """
        Process the ALLOCATE statement and store the various entities being
        allocated in self.items. Any type-specification is stored in
        self.spec.

        :raises ParseError: if an invalid type-specification is used
        """
        line = self.item.get_line()[8:].lstrip()[1:-1].strip()
        item2 = self.item.copy(line, True)
        line2 = item2.get_line()
        i = line2.find("::")
        if i != -1:
            spec = item2.apply_map(line2[:i].rstrip())
            from .block_statements import type_spec

            stmt = None
            for cls in type_spec:
                if cls.match(spec):
                    stmt = cls(self, item2.copy(spec))
                    if stmt.isvalid:
                        break
            if stmt is not None and stmt.isvalid:
                spec = stmt
            elif is_name(spec):
                # Type spec is the name of a derived type
                pass
            else:
                raise ParseError(
                    "Unrecognised type-specification in ALLOCATE statement: "
                    "{0}".format(self.item.line)
                )
            line2 = line2[i + 2 :].lstrip()
        else:
            spec = None
        self.spec = spec
        self.items = specs_split_comma(line2, item2)
        return

    def tofortran(self, isfix=None):
        """
        Create the Fortran code for this ALLOCATE statement

        :param bool isfix: whether or not to generate fixed-format code
        :return: Fortran code
        :rtype: str
        """
        type_spec = ""
        if self.spec:
            if isinstance(self.spec, str):
                type_spec = self.spec
            else:
                type_spec = self.spec.tostr()
            type_spec += " :: "
        return self.get_indent_tab(isfix=isfix) + "ALLOCATE (%s%s)" % (
            type_spec,
            ", ".join(self.items),
        )

    def analyze(self):
        return


class Deallocate(Statement):
    """
    ::

        DEALLOCATE ( <allocate-object-list> [ , <dealloc-opt-list> ] )
        <allocate-object> = <variable-name>
                          | <structure-component>
        <structure-component> = <data-ref>
        <dealloc-opt> = STAT = <stat-variable>
                        | ERRMSG = <errmsg-variable>
    """

    match = re.compile(r"deallocate\s*\(.*\)\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[10:].lstrip()[1:-1].strip()
        self.items = specs_split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "DEALLOCATE (%s)" % (
            ", ".join(self.items)
        )

    def analyze(self):
        return


class ModuleProcedure(Statement):
    """
    [ MODULE ] PROCEDURE [::] <procedure-name-list>
    """

    match = re.compile(r"(module\s*|)procedure\b\s*(::)?", re.I).match

    def process_item(self):
        line = self.item.get_line()
        m = self.match(line)
        assert m, repr(line)
        items = split_comma(line[m.end() :].strip(), self.item)
        for n in items:
            if not is_name(n):
                self.isvalid = False
                return
        self.items = items
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        return tab + "MODULE PROCEDURE %s" % (", ".join(self.items))

    def analyze(self):
        module_procedures = self.parent.a.module_procedures
        module_procedures.extend(self.items)
        # XXX: add names to parent_provides
        return


class Access(Statement):
    """
    <access-spec> [ [::] <access-id-list>]
    <access-spec> = PUBLIC | PRIVATE
    <access-id> = <use-name> | <generic-spec>
    """

    match = re.compile(r"(public|private)\b", re.I).match

    def process_item(self):
        clsname = self.__class__.__name__.lower()
        line = self.item.get_line()
        if not line.lower().startswith(clsname):
            self.isvalid = False
            return
        line = line[len(clsname) :].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        clsname = self.__class__.__name__.upper()
        tab = self.get_indent_tab(isfix=isfix)
        if self.items:
            return tab + clsname + " " + ", ".join(self.items)
        return tab + clsname

    def analyze(self):
        clsname = self.__class__.__name__
        bits = getattr(self.parent.a, clsname.lower() + "_id_list")
        if self.items:
            for name in self.items:
                if name not in bits:
                    bits.append(name)
        else:
            if "" not in bits:
                bits.append("")
            if not isinstance(self.parent, classes.Module):
                parentclsname = self.parent.__class__.__name__
                message = (
                    "C548 violation: %s statement only allowed in the"
                    " specification-part of a module, not in a %s."
                    % (clsname.upper(), parentclsname.lower())
                )
                self.warning(message)
        access_id_list = self.parent.a.private_id_list + self.parent.a.public_id_list
        if access_id_list.count("") > 1:
            message = (
                "C548 violation: only one access-stmt with omitted"
                " access-id-list is permitted in"
                " the module-specification-part."
            )
            self.warning(message)
        # todo: check for conflicting access statement usages
        #       (e.g. private foo; public foo)
        # todo: check for conflicting generic-spec id-s.
        return


class Public(Access):
    is_public = True


class Private(Access):
    is_public = False


class Close(Statement):
    """
    ::

        CLOSE ( <close-spec-list> )
        <close-spec> = [ UNIT = ] <file-unit-number>
                       | IOSTAT = <scalar-int-variable>
                       | IOMSG = <iomsg-variable>
                       | ERR = <label>
                       | STATUS = <scalar-default-char-expr>
    """

    match = re.compile(r"close\s*\(.*\)\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[5:].lstrip()[1:-1].strip()
        self.specs = specs_split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        return tab + "CLOSE (%s)" % (", ".join(self.specs))

    def analyze(self):
        return


class Cycle(Statement):
    """
    CYCLE [ <do-construct-name> ]
    """

    match = re.compile(r"cycle\b\s*\w*\s*\Z", re.I).match

    def process_item(self):
        self.name = self.item.get_line()[5:].lstrip()
        return

    def tofortran(self, isfix=None):
        if self.name:
            return self.get_indent_tab(isfix=isfix) + "CYCLE " + self.name
        return self.get_indent_tab(isfix=isfix) + "CYCLE"

    def analyze(self):
        return


class FilePositioningStatement(Statement):
    """
    REWIND <file-unit-number>
    REWIND ( <position-spec-list> )
    <position-spec-list> = [ UNIT = ] <file-unit-number>
                           | IOMSG = <iomsg-variable>
                           | IOSTAT = <scalar-int-variable>
                           | ERR = <label>
    The same for BACKSPACE, ENDFILE.
    """

    match = re.compile(r"(rewind|backspace|endfile)\b", re.I).match

    def process_item(self):
        clsname = self.__class__.__name__.lower()
        line = self.item.get_line()
        if not line.lower().startswith(clsname):
            self.isvalid = False
            return
        line = line[len(clsname) :].lstrip()
        if line.startswith("("):
            assert line[-1] == ")", repr(line)
            spec = line[1:-1].strip()
        else:
            spec = line
        self.specs = specs_split_comma(spec, self.item)
        return

    def tofortran(self, isfix=None):
        clsname = self.__class__.__name__.upper()
        return (
            self.get_indent_tab(isfix=isfix)
            + clsname
            + " (%s)" % (", ".join(self.specs))
        )

    def analyze(self):
        return


class Backspace(FilePositioningStatement):
    pass


class Endfile(FilePositioningStatement):
    pass


class Rewind(FilePositioningStatement):
    pass


class Open(Statement):
    """
    ::

        OPEN ( <connect-spec-list> )
        <connect-spec> = [ UNIT = ] <file-unit-number>
                         | ACCESS = <scalar-default-char-expr>
                         | ..
    """

    match = re.compile(r"open\s*\(.*\)\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[4:].lstrip()[1:-1].strip()
        self.specs = specs_split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "OPEN (%s)" % (", ".join(self.specs))

    def analyze(self):
        return


class Format(Statement):
    """
    ::

        FORMAT format-specification

        format-specification is ( [ format-item-list ] )
        format-item is [ r ] data-edit-descr
                    or control-edit-descr
                    or char-string-edit-descr
                    or [ r ] ( format-item-list )
        data-edit-descr is I w [ . m ]
                        or B w [ . m ]
                            ...
        r|w|m|d|e is int-literal-constant
        v is signed-int-literal-constant
        control-edit-descr is position-edit-descr
                           or [ r ] /
                           or :
                           ...
        position-edit-descr is T n
                            or TL n
                            ...
        sign-edit-descr is SS | SP | S
        ...

    """

    match = re.compile(r"format\s*\(.*\)\Z", re.I).match

    def process_item(self):
        item = self.item
        if item.label is None:
            # R1001:
            self.warning(
                "FORMAT statement must be labeled (F2008:C1001)." % (item.label)
            )
        line = item.get_line()[6:].lstrip()
        assert line[0] + line[-1] == "()", repr(line)
        self.specs = split_comma(line[1:-1], item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "FORMAT (%s)" % (
            ", ".join(self.specs)
        )

    def analyze(self):
        return


class Save(Statement):
    """
    ::

        SAVE [ [ :: ] saved-entity-list ]

        saved-entity is object-name
                     or proc-pointer-name
                     or / common-block-name /
        proc-pointer-name is name
        object-name is name
    """

    match = re.compile(r"save\b", re.I).match

    def process_item(self):
        assert not self.item.has_map()
        line = self.item.get_line()[4:].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        items = []
        for s in line.split(","):
            s = s.strip()
            if not s:
                continue
            if s.startswith("/"):
                assert s.endswith("/"), repr(s)
                n = s[1:-1].strip()
                assert is_name(n), repr(n)
                items.append("/%s/" % (n))
            elif is_name(s):
                items.append(s)
            else:
                self.isvalid = False
                return
        self.items = items
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        if not self.items:
            return tab + "SAVE"
        return tab + "SAVE %s" % (", ".join(self.items))

    def analyze(self):
        return


class Data(Statement):
    """
    ::

        DATA data-stmt-set [ [ , ] data-stmt-set ]...

        data-stmt-set is data-stmt-object-list / data-stmt-value-list /
        data-stmt-object is variable or data-implied-do
        data-implied-do is ( data-i-do-object-list ,
                              data-i-do-variable = scalar-int-expr ,
                              scalar-int-expr [ , scalar-int-expr ] )
        data-i-do-object is array-element
                         or scalar-structure-component
                         or data-implied-do
        data-i-do-variable is scalar-int-variable
        variable is designator
        designator is object-name
                   or array-element
                   or array-section
                   or structure-component
                   or substring
        array-element is data-ref
        array-section is data-ref [ ( substring-range ) ]

    """

    match = re.compile(r"data\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[4:].lstrip()
        stmts = []
        self.isvalid = False
        while line:
            i = line.find("/")
            if i == -1:
                return
            j = line.find("/", i + 1)
            if j == -1:
                return
            l1, l2 = line[:i].rstrip(), line[i + 1 : j].strip()
            l1 = split_comma(l1, self.item)
            l2 = split_comma(l2, self.item)
            stmts.append((l1, l2))
            line = line[j + 1 :].lstrip()
            if line.startswith(","):
                line = line[1:].lstrip()
        self.stmts = stmts
        self.isvalid = True
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        bits = []
        for o, v in self.stmts:
            bits.append("%s / %s /" % (", ".join(o), ", ".join(v)))
        return tab + "DATA " + " ".join(bits)

    def analyze(self):
        return


class Nullify(Statement):
    """
    NULLIFY ( <pointer-object-list> )
    <pointer-object> = <variable-name>
    """

    match = re.compile(r"nullify\s*\(.*\)\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[7:].lstrip()[1:-1].strip()
        self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "NULLIFY (%s)" % (
            ", ".join(self.items)
        )

    def analyze(self):
        return


class Use(Statement):
    """Parses USE statement.

    :param class Statement: Base fparser class.
    :raises AnalyzeError: If entity name is not in module.

    Fortran syntax construct::

        USE [ [ , module-nature ] :: ] module-name [ , rename-list ]
        USE [ [ , module-nature ] :: ] module-name , ONLY : [ only-list ]

        module-nature is INTRINSIC or NON_INTRINSIC
        rename is local-name => use-name
                   or OPERATOR ( local-defined-operator ) =>
                                OPERATOR ( use-defined-operator )
        only is generic-spec or only-use-name or rename
        only-use-name is use-name
    """

    match = re.compile(r"use\b", re.I).match

    def process_item(self):
        """Parse the string containing the Use and store the
        module name and list of attributes (if any)"""
        line = self.item.get_line()[3:].lstrip()
        nature = ""
        if line.startswith(","):
            i = line.find("::")
            nature = line[1:i].strip().upper()
            line = line[i + 2 :].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        if nature and not is_name(nature):
            self.isvalid = False
            return
        self.nature = nature
        i = line.find(",")
        self.isonly = False
        if i == -1:
            self.name = line
            self.items = []
        else:
            self.name = line[:i].rstrip()
            line = line[i + 1 :].lstrip()
            if line.lower().startswith("only") and line[4:].lstrip().startswith(":"):
                self.isonly = True
                line = line[4:].lstrip()[1:].lstrip()
            self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):  # pylint: disable=invalid-name
        """
        Returns the Fortran representation of this object as a string

        :param bool isfix: Whether or not to generated fixed-format Fortran
        :return: Fortran representation of this object
        :rtype: str
        """
        tab = self.get_indent_tab(isfix=isfix)
        s = "USE"
        if self.nature:
            s += ", " + self.nature + " ::"
        s += " " + self.name
        if self.isonly:
            s += ", ONLY:"
        elif self.items:
            s += ","
        if self.items:
            s += " " + ", ".join(self.items)
        return tab + s

    def analyze(self):  # pylint: disable=invalid-name
        """Returns warnings if this object is incorrect"""
        use = self.parent.a.use
        if self.name in use:
            return

        modules = self.top.a.module
        if self.name not in modules:
            fn = self.reader.find_module_source_file(self.name)
            if fn is not None:
                from fparser.common.readfortran import FortranFileReader
                from fparser.one.parsefortran import FortranParser

                self.info("looking module information from %r" % (fn))
                reader = FortranFileReader(
                    fn,
                    include_dirs=self.reader.include_dirs,
                    source_only=self.reader.source_only,
                )
                parser = FortranParser(reader)
                parser.parse()
                parser.block.a.module.update(modules)
                parser.analyze()
                modules.update(parser.block.a.module)

        if self.name not in modules:
            self.warning(
                "no information about the module %r in use statement" % (self.name)
            )
            return

        module = modules[self.name]
        use[self.name] = module
        use_provides = self.parent.a.use_provides
        renames = [split_comma(item, comma="=>") for item in self.items if "=>" in item]
        norenames = [item for item in self.items if "=>" not in item]
        all_mod_provides = dict(module.a.module_provides)
        all_mod_provides.update(module.a.use_provides)
        if self.isonly:
            # populate use_provides with items/renames only.
            for rename, orig in renames:
                self.populate_use_provides(all_mod_provides, use_provides, orig, rename)
            for name in norenames:
                self.populate_use_provides(all_mod_provides, use_provides, name)
        else:
            # norenames should be empty
            if norenames:
                self.warning(
                    "'use' without 'only' clause does not rename the "
                    "variables '%s'" % ", ".join(norenames)
                )
            # populate use_provides with renamed vars from module.
            for rename, orig in renames:
                self.populate_use_provides(all_mod_provides, use_provides, orig, rename)
            # get all the rest
            unrenamed = set(all_mod_provides) - set([b for (a, b) in renames])
            for name in unrenamed:
                self.populate_use_provides(all_mod_provides, use_provides, name)
        return

    def populate_use_provides(self, all_mod_provides, use_provides, name, rename=None):
        """Checks for entity name in the module"""
        ovar = all_mod_provides.get(name, None)
        if ovar is None:
            raise AnalyzeError(
                "entity name '%s' is not in module '%s'" % (name, self.name)
            )
        if rename:
            # XXX: rename != ovar.name -- should mark this somehow?
            name_idx = rename
        else:
            name_idx = name
        if name_idx in use_provides:
            if ovar != use_provides[name_idx]:
                self.warning(
                    "entity name '%s' is already declared in module "
                    "'%s' while adding it to '%s', overriding."
                    % (name, self.name, self.parent.name)
                )
        use_provides[name_idx] = ovar


class Exit(Statement):
    """
    EXIT [ <do-construct-name> ]
    """

    match = re.compile(r"exit\b\s*\w*\s*\Z", re.I).match

    def process_item(self):
        self.name = self.item.get_line()[4:].lstrip()
        return

    def tofortran(self, isfix=None):
        if self.name:
            return self.get_indent_tab(isfix=isfix) + "EXIT " + self.name
        return self.get_indent_tab(isfix=isfix) + "EXIT"

    def analyze(self):
        return


class Parameter(Statement):
    """
    PARAMETER ( <named-constant-def-list> )
    <named-constant-def> = <named-constant> = <initialization-expr>
    """

    match = re.compile(r"parameter\s*\(.*\)\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[9:].lstrip()[1:-1].strip()
        self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "PARAMETER (%s)" % (
            ", ".join(self.items)
        )

    def analyze(self):
        for item in self.items:
            i = item.find("=")
            assert i != -1, repr(item)
            name = item[:i].rstrip()
            value = item[i + 1 :].lstrip()
            var = self.get_variable(name)
            var.update("parameter")
            var.set_init(value)
        return


class Equivalence(Statement):
    """
    EQUIVALENCE <equivalence-set-list>
    <equivalence-set> = ( <equivalence-object> , <equivalence-object-list> )
    <equivalence-object> = <variable-name> | <array-element> | <substring>
    """

    match = re.compile(r"equivalence\s*\(.*\)\Z", re.I).match

    def process_item(self):
        items = []
        for s in self.item.get_line()[11:].lstrip().split(","):
            s = s.strip()
            assert s[0] + s[-1] == "()", repr((s, self.item.get_line()))
            s = ", ".join(split_comma(s[1:-1], self.item))
            items.append("(" + s + ")")
        self.items = items
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "EQUIVALENCE %s" % (
            ", ".join(self.items)
        )

    def analyze(self):
        return


class Dimension(Statement):
    """
    DIMENSION [ :: ] <array-name> ( <array-spec> )
                                    [ , <array-name> ( <array-spec> ) ]...

    """

    match = re.compile(r"dimension\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[9:].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "DIMENSION %s" % (
            ", ".join(self.items)
        )

    def analyze(self):
        for line in self.items:
            i = line.find("(")
            assert i != -1 and line.endswith(")"), repr(line)
            name = line[:i].rstrip()
            array_spec = split_comma(line[i + 1 : -1].strip(), self.item)
            var = self.get_variable(name)
            var.set_bounds(array_spec)
        return


class Target(Statement):
    """
    TARGET [ :: ] <object-name> ( <array-spec> )
                                  [ , <object-name> ( <array-spec> ) ]...

    """

    match = re.compile(r"target\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[6:].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "TARGET %s" % (", ".join(self.items))

    def analyze(self):
        for line in self.items:
            i = line.find("(")
            assert i != -1 and line.endswith(")"), repr(line)
            name = line[:i].rstrip()
            array_spec = split_comma(line[i + 1 : -1].strip(), self.item)
            var = self.get_variable(name)
            var.set_bounds(array_spec)
            var.update("target")
        return


class Pointer(Statement):
    """
    ::

        POINTER [ :: ] pointer-decl-list

        pointer-decl is object-name [ ( deferred-shape-spec-list ) ]
                     or proc-entity-name

    """

    match = re.compile(r"pointer\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[7:].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "POINTER %s" % (", ".join(self.items))

    def analyze(self):
        for line in self.items:
            i = line.find("(")
            if i == -1:
                name = line
                array_spec = None
            else:
                assert line.endswith(")"), repr(line)
                name = line[:i].rstrip()
                array_spec = split_comma(line[i + 1 : -1].strip(), self.item)
            var = self.get_variable(name)
            var.set_bounds(array_spec)
            var.update("pointer")
        return


class Protected(StatementWithNamelist):
    """
    PROTECTED [ :: ] <entity-name-list>
    """

    match = re.compile(r"protected\b", re.I).match

    def analyze(self):
        for name in self.items:
            var = self.get_variable(name)
            var.update("protected")
        return


class Volatile(StatementWithNamelist):
    """
    VOLATILE [ :: ] <object-name-list>
    """

    match = re.compile(r"volatile\b", re.I).match

    def analyze(self):
        for name in self.items:
            var = self.get_variable(name)
            var.update("volatile")
        return


class Value(StatementWithNamelist):
    """
    VALUE [ :: ] <dummy-arg-name-list>
    """

    match = re.compile(r"value\b", re.I).match

    def analyze(self):
        for name in self.items:
            var = self.get_variable(name)
            var.update("value")
        return


class ArithmeticIf(Statement):
    """
    IF ( <scalar-numeric-expr> ) <label> , <label> , <label>
    """

    match = re.compile(r"if\s*\(.*\)\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[2:].lstrip()
        line, l2, l3 = line.rsplit(",", 2)
        i = line.rindex(")")
        l1 = line[i + 1 :]
        self.expr = self.item.apply_map(line[1:i]).strip()
        self.labels = [l1.strip(), l2.strip(), l3.strip()]
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "IF (%s) %s" % (
            self.expr,
            ", ".join(self.labels),
        )

    def analyze(self):
        return


class Intrinsic(StatementWithNamelist):
    """
    INTRINSIC [ :: ] <intrinsic-procedure-name-list>
    """

    match = re.compile(r"intrinsic\b", re.I).match

    def analyze(self):
        for name in self.items:
            var = self.get_variable(name)
            var.update("intrinsic")
        return


class Inquire(Statement):
    """
    INQUIRE ( <inquire-spec-list> )
    INQUIRE ( IOLENGTH = <scalar-int-variable> ) <output-item-list>

    <inquire-spec> = [ UNIT = ] <file-unit-number>
                     | FILE = <file-name-expr>
                     ...
    <output-item> = <expr>
                  | <io-implied-do>
    """

    match = re.compile(r"inquire\s*\(", re.I).match

    def process_item(self):
        line = self.item.get_line()[7:].lstrip()
        i = line.index(")")
        self.specs = specs_split_comma(line[1:i].strip(), self.item)
        self.items = split_comma(line[i + 1 :].lstrip(), self.item)
        return

    def tofortran(self, isfix=None):
        if self.items:
            return self.get_indent_tab(isfix=isfix) + "INQUIRE (%s) %s" % (
                ", ".join(self.specs),
                ", ".join(self.items),
            )
        return self.get_indent_tab(isfix=isfix) + "INQUIRE (%s)" % (
            ", ".join(self.specs)
        )

    def analyze(self):
        return


class Sequence(Statement):
    """
    SEQUENCE
    """

    match = re.compile(r"sequence\Z", re.I).match

    def process_item(self):
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "SEQUENCE"

    def analyze(self):
        self.parent.update_attributes("SEQUENCE")
        return


class External(StatementWithNamelist):
    """
    EXTERNAL [ :: ] <external-name-list>
    """

    match = re.compile(r"external\b", re.I).match

    def analyze(self):
        for name in self.items:
            var = self.get_variable(name)
            var.update("external")
        return


class Namelist(Statement):
    """
    NAMELIST / <namelist-group-name> / <namelist-group-object-list> [ [ , ]
             / <namelist-group-name> / <namelist-group-object-list> ]...
    <namelist-group-object> = <variable-name>
    """

    match = re.compile(r"namelist\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[8:].lstrip()
        items = []
        while line:
            assert line.startswith("/"), repr(line)
            i = line.find("/", 1)
            assert i != -1, repr(line)
            name = line[: i + 1]
            line = line[i + 1 :].lstrip()
            i = line.find("/")
            if i == -1:
                items.append((name, line))
                line = ""
                continue
            s = line[:i].rstrip()
            if s.endswith(","):
                s = s[:-1].rstrip()
            items.append((name, s))
            line = line[i + 1 :].lstrip()
        self.items = items
        return

    def tofortran(self, isfix=None):
        bits = []
        for name, s in self.items:
            bits.append("%s %s" % (name, s))
        tab = self.get_indent_tab(isfix=isfix)
        return tab + "NAMELIST " + ", ".join(bits)


class Common(Statement):
    """
    ::

        COMMON [ / [ common-block-name ] / ] common-block-object-list \
          [ [ , ] / [ common-block-name ] / common-block-object-list ]...

        common-block-object is variable-name [ ( explicit-shape-spec-list ) ]
                            or proc-pointer-name
    """

    match = re.compile(r"common\b", re.I).match

    def process_item(self):
        item = self.item
        line = item.get_line()[6:].lstrip()
        items = []
        while line:
            if not line.startswith("/"):
                name = ""
                assert not items, repr(line)
            else:
                i = line.find("/", 1)
                assert i != -1, repr(line)
                name = line[1:i].strip()
                line = line[i + 1 :].lstrip()
            i = line.find("/")
            if i == -1:
                items.append((name, split_comma(line, item)))
                line = ""
                continue
            s = line[:i].rstrip()
            if s.endswith(","):
                s = s[:-1].rstrip()
            items.append((name, split_comma(s, item)))
            line = line[i:].lstrip()
        self.items = items
        return

    def tofortran(self, isfix=None):
        bits = []
        for name, s in self.items:
            s = ", ".join(s)
            if name:
                bits.append("/ %s / %s" % (name, s))
            else:
                bits.append(s)
        tab = self.get_indent_tab(isfix=isfix)
        return tab + "COMMON " + " ".join(bits)

    def analyze(self):
        for cname, items in self.items:
            for item in items:
                i = item.find("(")
                if i != -1:
                    assert item.endswith(")"), repr(item)
                    name = item[:i].rstrip()
                    shape = split_comma(item[i + 1 : -1].strip(), self.item)
                else:
                    name = item
                    shape = None
                var = self.get_variable(name)
                if shape is not None:
                    var.set_bounds(shape)
            # XXX: add name,var to parent_provides
        return


class Optional(StatementWithNamelist):
    """
    OPTIONAL [ :: ] <dummy-arg-name-list>
    <dummy-arg-name> = <name>
    """

    match = re.compile(r"optional\b", re.I).match

    def analyze(self):
        for name in self.items:
            var = self.get_variable(name)
            var.update("optional")
        return


class Intent(Statement):
    """
    INTENT ( <intent-spec> ) [ :: ] <dummy-arg-name-list>
    <intent-spec> = IN | OUT | INOUT

    generalization for pyf-files:
    INTENT ( <intent-spec-list> ) [ :: ] <dummy-arg-name-list>
    <intent-spec> = IN | OUT | INOUT | CACHE | HIDE | OUT = <name>
    """

    match = re.compile(r"intent\s*\(", re.I).match

    def process_item(self):
        line = self.item.get_line()[6:].lstrip()
        i = line.find(")")
        self.specs = specs_split_comma(line[1:i], self.item, upper=True)
        line = line[i + 1 :].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = [s.strip() for s in line.split(",")]
        for n in self.items:
            if not is_name(n):
                self.isvalid = False
                return
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "INTENT (%s) %s" % (
            ", ".join(self.specs),
            ", ".join(self.items),
        )

    def analyze(self):
        for name in self.items:
            var = self.get_variable(name)
            var.set_intent(self.specs)
        return


class Entry(Statement):
    """
    ::

        ENTRY entry-name [ ( [ dummy-arg-list ] ) [ suffix ] ]

        suffix is proc-language-binding-spec [ RESULT ( result-name ) ]
                 | RESULT ( result-name ) [ proc-language-binding-spec ]
        proc-language-binding-spec is language-binding-spec
        language-binding-spec is
                      BIND ( C [ , NAME = scalar-char-initialization-expr ] )
        dummy-arg is dummy-arg-name | *

    """

    match = re.compile(r"entry\s+[a-zA-Z]", re.I).match

    def process_item(self):
        line = self.item.get_line()[5:].lstrip()
        m = re.match(r"\w+", line)
        name = line[: m.end()]
        line = line[m.end() :].lstrip()
        if line.startswith("("):
            i = line.find(")")
            assert i != -1, repr(line)
            items = split_comma(line[1:i], self.item)
            line = line[i + 1 :].lstrip()
        else:
            items = []
        self.bind, line = parse_bind(line, self.item)
        self.result, line = parse_result(line, self.item)
        if line:
            assert self.bind is None, repr(self.bind)
            self.bind, line = parse_bind(line, self.item)
        assert not line, repr(line)
        self.name = name
        self.items = items
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        s = tab + "ENTRY " + self.name
        if self.items:
            s += " (%s)" % (", ".join(self.items))
        if self.result:
            s += " RESULT (%s)" % (self.result)
        if self.bind:
            s += " BIND (%s)" % (", ".join(self.bind))
        return s


class Import(StatementWithNamelist):
    """
    IMPORT [ [ :: ] <import-name-list> ]
    """

    match = re.compile(r"import(\b|\Z)", re.I).match


class Forall(Statement):
    """
    FORALL <forall-header> <forall-assignment-stmt>
    <forall-header> = ( <forall-triplet-spec-list> [ , <scalar-mask-expr> ] )
    <forall-triplet-spec> =
                    <index-name> = <subscript> : <subscript> [ : <stride> ]
    <subscript|stride> = <scalar-int-expr>
    <forall-assignment-stmt> = <assignment-stmt> | <pointer-assignment-stmt>
    """

    match = re.compile(r"forall\s*\(.*\).*=", re.I).match

    def process_item(self):
        line = self.item.get_line()[6:].lstrip()
        i = line.index(")")

        line0 = line[1:i]
        line = line[i + 1 :].lstrip()
        stmt = GeneralAssignment(self, self.item.copy(line, True))
        if stmt.isvalid:
            self.content = [stmt]
        else:
            self.isvalid = False
            return

        specs = []
        mask = ""
        for l in split_comma(line0, self.item):
            j = l.find("=")
            if j == -1:
                assert not mask, repr((mask, l))
                mask = l
                continue
            assert j != -1, repr(l)
            index = l[:j].rstrip()
            it = self.item.copy(l[j + 1 :].lstrip())
            line = it.get_line()
            k = line.split(":")
            if len(k) == 3:
                s1, s2, s3 = list(
                    map(it.apply_map, [k[0].strip(), k[1].strip(), k[2].strip()])
                )
            else:
                assert len(k) == 2, repr(k)
                s1, s2 = list(map(it.apply_map, [k[0].strip(), k[1].strip()]))
                s3 = "1"
            specs.append((index, s1, s2, s3))

        self.specs = specs
        self.mask = mask
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        lines = []
        for index, s1, s2, s3 in self.specs:
            s = "%s = %s : %s" % (index, s1, s2)
            if s3 != "1":
                s += " : %s" % (s3)
            lines.append(s)
        s = ", ".join(lines)
        if self.mask:
            s += ", " + self.mask
        return tab + "FORALL (%s) %s" % (s, str(self.content[0]).lstrip())

    def analyze(self):
        return


ForallStmt = Forall


class SpecificBinding(Statement):
    """
    PROCEDURE [ ( <interface-name> ) ]  [ [ , <binding-attr-list> ] :: ]
              <binding-name> [ => <procedure-name> ]
    <binding-attr> = PASS [ ( <arg-name> ) ]
                   | NOPASS
                   | NON_OVERRIDABLE
                   | DEFERRED
                   | <access-spec>
    <access-spec> = PUBLIC | PRIVATE

    """

    match = re.compile(r"procedure\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[9:].lstrip()
        if line.startswith("("):
            i = line.index(")")
            name = line[1:i].strip()
            line = line[i + 1 :].lstrip()
        else:
            name = ""
        self.iname = name
        if line.startswith(","):
            line = line[1:].lstrip()
        i = line.find("::")
        if i != -1:
            attrs = split_comma(line[:i], self.item)
            line = line[i + 2 :].lstrip()
        else:
            attrs = []
        attrs1 = []
        for attr in attrs:
            if is_name(attr):
                attr = attr.upper()
            else:
                i = attr.find("(")
                assert i != -1 and attr.endswith(")"), repr(attr)
                attr = "%s (%s)" % (attr[:i].rstrip().upper(), attr[i + 1 : -1].strip())
            attrs1.append(attr)
        self.attrs = attrs1
        i = line.find("=")
        if i == -1:
            self.name = line
            self.bname = ""
        else:
            self.name = line[:i].rstrip()
            self.bname = line[i + 1 :].lstrip()[1:].lstrip()
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        s = "PROCEDURE "
        if self.iname:
            s += "(" + self.iname + ") "
        if self.attrs:
            s += ", " + ", ".join(self.attrs) + " :: "
        if self.bname:
            s += "%s => %s" % (self.name, self.bname)
        else:
            s += self.name
        return tab + s


class GenericBinding(Statement):
    """
    GENERIC [ , <access-spec> ] :: <generic-spec> => <binding-name-list>

    """

    match = re.compile(r"generic\b.*::.*=\>.*\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[7:].lstrip()
        if line.startswith(","):
            line = line[1:].lstrip()
        i = line.index("::")
        self.aspec = line[:i].rstrip().upper()
        line = line[i + 2 :].lstrip()
        i = line.index("=>")
        self.spec = self.item.apply_map(line[:i].rstrip())
        self.items = split_comma(line[i + 2 :].lstrip())
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        s = "GENERIC"
        if self.aspec:
            s += ", " + self.aspec
        s += " :: " + self.spec + " => " + ", ".join(self.items)
        return tab + s


class FinalBinding(StatementWithNamelist):
    """
    FINAL [ :: ] <final-subroutine-name-list>

    """

    stmtname = "final"
    match = re.compile(r"final\b", re.I).match


class Allocatable(Statement):
    """
    ALLOCATABLE [ :: ] <object-name> [ ( <deferred-shape-spec-list> ) ]
                                     [ , <object-name>
                                         [ ( <deferred-shape-spec-list> ) ]
                                     ]...

    """

    match = re.compile(r"allocatable\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[11:].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "ALLOCATABLE " + ", ".join(self.items)

    def analyze(self):
        for line in self.items:
            i = line.find("(")
            if i == -1:
                name = line
                array_spec = None
            else:
                assert line.endswith(")")
                name = line[:i].rstrip()
                array_spec = split_comma(line[i + 1 : -1], self.item)
            var = self.get_variable(name)
            var.update("allocatable")
            if array_spec is not None:
                var.set_bounds(array_spec)
        return


class Asynchronous(StatementWithNamelist):
    """
    ASYNCHRONOUS [ :: ] <object-name-list>
    """

    match = re.compile(r"asynchronous\b", re.I).match

    def analyze(self):
        for name in self.items:
            var = self.get_variable(name)
            var.update("asynchronous")
        return


class Bind(Statement):
    """
    ::

        language-binding-spec [ :: ] bind-entity-list
        language-binding-spec is
                      BIND ( C [ , NAME = scalar-char-initialization-expr ] )
        bind-entity is entity-name> | / common-block-name /

    """

    match = re.compile(r"bind\s*\(.*\)", re.I).match

    def process_item(self):
        line = self.item.line
        self.specs, line = parse_bind(line, self.item)
        if line.startswith("::"):
            line = line[2:].lstrip()
        items = []
        for item in split_comma(line, self.item):
            if item.startswith("/"):
                assert item.endswith("/"), repr(item)
                item = "/ " + item[1:-1].strip() + " /"
            items.append(item)
        self.items = items
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "BIND (%s) %s" % (
            ", ".join(self.specs),
            ", ".join(self.items),
        )


# IF construct statements


class Else(Statement):
    """
    ELSE [<if-construct-name>]
    """

    match = re.compile(r"else\b\s*\w*\s*\Z", re.I).match

    def process_item(self):
        item = self.item
        self.name = item.get_line()[4:].strip()
        parent_name = getattr(self.parent, "name", "")
        if self.name and self.name != parent_name:
            self.warning(
                "expected if-construct-name %r but got %r, skipping."
                % (parent_name, self.name)
            )
            self.isvalid = False
        return

    def tofortran(self, isfix=None):
        if self.name:
            return self.get_indent_tab(deindent=True) + "ELSE " + self.name
        return self.get_indent_tab(deindent=True) + "ELSE"

    def analyze(self):
        return


class ElseIf(Statement):
    """
    ELSE IF ( <scalar-logical-expr> ) THEN [ <if-construct-name> ]
    """

    match = re.compile(r"else\s*if\s*\(.*\)\s*then\s*\w*\s*\Z", re.I).match

    def process_item(self):
        item = self.item
        line = item.get_line()[4:].lstrip()[2:].lstrip()
        i = line.find(")")
        assert line[0] == "("
        self.expr = item.apply_map(line[1:i])
        self.name = line[i + 1 :].lstrip()[4:].strip()
        parent_name = getattr(self.parent, "name", "")
        if self.name and self.name != parent_name:
            self.warning(
                "expected if-construct-name %r but got %r, skipping."
                % (parent_name, self.name)
            )
            self.isvalid = False
        return

    def tofortran(self, isfix=None):
        s = ""
        if self.name:
            s = " " + self.name
        return self.get_indent_tab(deindent=True) + "ELSE IF (%s) THEN%s" % (
            self.expr,
            s,
        )

    def analyze(self):
        return


# SelectCase construct statements


class Case(Statement):
    """
    Captures a Case statement::

        CASE case-selector [ case-construct-name ]
        case-selector is ( case-value-range-list ) | DEFAULT
        case-value-range is case-value
                         or case-value :
                         or : case-value
                         or case-value : case-value
        case-value is scalar-(int|char|logical)-initialization-expr

    """

    match = re.compile(r"case\b\s*(\(.*\)|DEFAULT)\s*\w*\Z", re.I).match

    def process_item(self):
        """Populate the state of this item by parsing the associated line
        of code"""
        line = self.item.get_line()[4:].lstrip()
        try:
            self.items = extract_bracketed_list_items(line, self.item)
            idx = line.rfind(")")
            self.name = line[idx + 1 :].lstrip()
        except ParseError:
            # No list in parentheses found so we must have a 'case default'
            if not line.lower().startswith("default"):
                # We should never get to here because such a string should
                # not have generated a match
                self.warning(
                    "Internal error when parsing CASE statement: {0}".format(line)
                )
                self.isvalid = False
                return
            self.items = []
            self.name = line[7:].lstrip()
        parent_name = getattr(self.parent, "name", "")
        if self.name and self.name != parent_name:
            self.warning(
                "Expected case-construct-name {0} but got {1}, "
                "skipping.".format(parent_name, self.name)
            )
            self.isvalid = False
        return

    def tofortran(self, isfix=None):
        """Return the Fortran for this object as a string"""
        tab = self.get_indent_tab(isfix=isfix)
        txt = tab + "CASE"
        if self.items:
            lst = []
            for item in self.items:
                lst.append((" : ".join(item)).strip())
            txt += " ( %s )" % (", ".join(lst))
        else:
            txt += " DEFAULT"
        if self.name:
            txt += " " + self.name
        return txt

    def analyze(self):
        return


class TypeIs(Statement):
    """
    TYPE IS <type-selector> [ <case-construct-name> ]
    <type-selector> = ( <type-value-range-list> )
    <type-value-range> = <case-value>
    <case-value> = <char>
    """

    match = re.compile(r"type\b\s*is\b\s*\(.*\)\s*\w*\Z", re.I).match

    def process_item(self):
        """Populate the state of this object by parsing the associated
        line of code"""
        line = self.item.get_line()
        # We have a 'type is (...)' statement. At this point
        # any expression used for the type specifier will have
        # been replaced with e.g. F2PY_EXPR_TUPLE_3 and so
        # will not itself contain any parentheses.
        self.items = extract_bracketed_list_items(line, self.item)
        # Get and store the case-construct-name (if any)
        idx2 = line.rfind(")")
        self.name = line[idx2 + 1 :].lstrip()
        parent_name = getattr(self.parent, "name", "")
        if self.name and self.name != parent_name:
            self.warning(
                "expected type-is-construct-name %r but got %r, skipping."
                % (parent_name, self.name)
            )
            self.isvalid = False
        return

    def tofortran(self, isfix=None):
        """Create the Fortran representation of this object and return
        it as a string"""
        tab = self.get_indent_tab(isfix=isfix)
        text = tab + "TYPE IS"
        if self.items:
            lst = []
            for item in self.items:
                lst.append((" : ".join(item)).strip())
            text += " ( %s )" % (", ".join(lst))
        else:
            raise ParseError("TYPE IS construct must have arguments")
        if self.name:
            text += " " + self.name
        return text

    def analyze(self):
        return


class ClassIs(Statement):
    """
    CLASS <class-selector>
    <class-selector> = ( IS <type-value-range-list> | DEFAULT )
    <type-value-range> = <case-value>
    <case-value> = <char>
    """

    match = re.compile(r"class\b\s*(is\b\s*\(.*\)|default)\s*\w*\Z", re.I).match

    def process_item(self):
        """Populate the state of this object by parsing the string"""
        line = self.item.get_line()[5:].lstrip()
        try:
            self.items = extract_bracketed_list_items(line, self.item)
            # We have a 'class is ...' statement. At this point
            # any expression used for the class specifier will have
            # been replaced with e.g. F2PY_EXPR_TUPLE_3 and so
            # will not contain any parentheses.
            idx = line.rfind(")")
            self.name = line[idx + 1 :].lstrip()
        except ParseError:
            # We have a 'class default' statement
            if not line.lower().startswith("default"):
                # We should never get here because such a string should
                # not have generated a match
                self.warning(
                    "Internal error when parsing CLASS statement: {0}".format(line)
                )
                self.isvalid = False
                return
            self.items = []
            self.name = line[7:].lstrip()
        parent_name = getattr(self.parent, "name", "")
        if self.name and self.name != parent_name:
            self.warning(
                "expected class-construct-name %r but got %r, skipping."
                % (parent_name, self.name)
            )
            self.isvalid = False
        return

    def tofortran(self, isfix=None):
        """Returns the Fortran for this object as a string"""
        tab = self.get_indent_tab(isfix=isfix)
        text = tab + "CLASS"
        if self.items:
            text += " IS"
            lchar = []
            for item in self.items:
                lchar.append((" : ".join(item)).strip())
            text += " ( %s )" % (", ".join(lchar))
        else:
            text += " DEFAULT"
        if self.name:
            text += " " + self.name
        return text

    def analyze(self):
        return


# Where construct statements


class Where(Statement):
    """
    WHERE ( <mask-expr> ) <where-assignment-stmt>
    """

    match = re.compile(r"where\s*\(.*\)\s*\w.*\Z", re.I).match

    def process_item(self):
        line = self.item.get_line()[5:].lstrip()
        i = line.index(")")
        self.expr = self.item.apply_map(line[1:i].strip())
        line = line[i + 1 :].lstrip()
        newitem = self.item.copy(line)
        cls = Assignment
        if cls.match(line):
            stmt = cls(self, newitem)
            if stmt.isvalid:
                self.content = [stmt]
                return
        self.isvalid = False
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        return tab + "WHERE ( %s ) %s" % (self.expr, str(self.content[0]).lstrip())

    def analyze(self):
        return


WhereStmt = Where


class ElseWhere(Statement):
    """
    ELSE WHERE ( <mask-expr> ) [ <where-construct-name> ]
    ELSE WHERE [ <where-construct-name> ]
    """

    match = re.compile(r"else\s*where\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[4:].lstrip()[5:].lstrip()
        self.expr = None
        if line.startswith("("):
            i = line.index(")")
            assert i != -1, repr(line)
            self.expr = self.item.apply_map(line[1:i].strip())
            line = line[i + 1 :].lstrip()
        self.name = line
        parent_name = getattr(self.parent, "name", "")
        if self.name and not self.name == parent_name:
            self.warning(
                "expected where-construct-name %r but got %r, "
                "skipping." % (parent_name, self.name)
            )
            self.isvalid = False
        return

    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        s = "ELSE WHERE"
        if self.expr is not None:
            s += " ( %s )" % (self.expr)
        if self.name:
            s += " " + self.name
        return tab + s

    def analyze(self):
        return


# Enum construct statements


class Enumerator(Statement):
    """
    ENUMERATOR [ :: ] <enumerator-list>
    <enumerator> = <named-constant> [ = <scalar-int-initialization-expr> ]
    """

    match = re.compile(r"enumerator\b", re.I).match

    def process_item(self):
        line = self.item.get_line()[10:].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = split_comma(line, self.item)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "ENUMERATOR " + ", ".join(self.items)


# F2PY specific statements


class FortranName(Statement):
    """
    FORTRANNAME <name>
    """

    match = re.compile(r"fortranname\s*\w+\Z", re.I).match

    def process_item(self):
        self.value = self.item.get_line()[11:].lstrip()
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "FORTRANNAME " + self.value


class Threadsafe(Statement):
    """
    THREADSAFE
    """

    match = re.compile(r"threadsafe\Z", re.I).match

    def process_item(self):
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "THREADSAFE"


class Depend(Statement):
    """
    DEPEND ( <name-list> ) [ :: ] <dummy-arg-name-list>

    """

    match = re.compile(r"depend\s*\(", re.I).match

    def process_item(self):
        line = self.item.get_line()[6:].lstrip()
        i = line.find(")")
        self.depends = split_comma(line[1:i].strip(), self.item)
        line = line[i + 1 :].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.items = split_comma(line)
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "DEPEND ( %s ) %s" % (
            ", ".join(self.depends),
            ", ".join(self.items),
        )


class Check(Statement):
    """
    CHECK ( <c-int-scalar-expr> ) [ :: ] <name>

    """

    match = re.compile(r"check\s*\(", re.I).match

    def process_item(self):
        line = self.item.get_line()[5:].lstrip()
        i = line.find(")")
        assert i != -1, repr(line)
        self.expr = self.item.apply_map(line[1:i].strip())
        line = line[i + 1 :].lstrip()
        if line.startswith("::"):
            line = line[2:].lstrip()
        self.value = line
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "CHECK ( %s ) %s" % (
            self.expr,
            self.value,
        )


class CallStatement(Statement):
    """
    CALLSTATEMENT <c-expr>
    """

    match = re.compile(r"callstatement\b", re.I).match

    def process_item(self):
        self.expr = self.item.apply_map(self.item.get_line()[13:].lstrip())
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "CALLSTATEMENT " + self.expr


class CallProtoArgument(Statement):
    """
    CALLPROTOARGUMENT <c-type-spec-list>
    """

    match = re.compile(r"callprotoargument\b", re.I).match

    def process_item(self):
        self.specs = self.item.apply_map(self.item.get_line()[17:].lstrip())
        return

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + "CALLPROTOARGUMENT " + self.specs


# Non-standard statements


class Pause(Statement):
    """
    PAUSE [ <char-literal-constant|int-literal-constant> ]
    """

    match = re.compile(r'pause\s*(\d+|\'\w*\'|"\w*"|)\Z', re.I).match

    def process_item(self):
        self.value = self.item.apply_map(self.item.get_line()[5:].lstrip())
        return

    def tofortran(self, isfix=None):
        if self.value:
            return self.get_indent_tab(isfix=isfix) + "PAUSE " + self.value
        return self.get_indent_tab(isfix=isfix) + "PAUSE"

    def analyze(self):
        return


class Comment(Statement):
    """

    Attributes

    content : str
      Content of the comment.
    is_blank : bool
      When True then Comment represents blank line.

    """

    match = lambda s: True

    def process_item(self):
        assert self.item.comment.count("\n") <= 1, repr(self.item)
        stripped = self.item.comment.lstrip()
        self.is_blank = not stripped
        self.content = stripped[1:] if stripped else ""

    def tofortran(self, isfix=None):
        if self.is_blank:
            return ""
        if isfix:
            tab = "C" + self.get_indent_tab(isfix=isfix)[1:]
        else:
            tab = self.get_indent_tab(isfix=isfix) + "!"
        return tab + self.content

    def analyze(self):
        return
