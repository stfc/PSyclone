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
Various utility functions.

Permission to use, modify, and distribute this software is given under the
terms of the NumPy License. See http://scipy.org.

NO WARRANTY IS EXPRESSED OR IMPLIED.  USE AT YOUR OWN RISK.
Author: Pearu Peterson <pearu@cens.ioc.ee>
Created: May 2006

"""

__all__ = [
    "split_comma",
    "specs_split_comma",
    "ParseError",
    "AnalyzeError",
    "get_module_file",
    "parse_bind",
    "parse_result",
    "is_name",
    "parse_array_spec",
    "CHAR_BIT",
    "str2stmt",
    "classes",
]

import logging
import glob
import io
import os
import re
import traceback


class ParseError(Exception):
    pass


class AnalyzeError(Exception):
    pass


is_name = re.compile(r"^[a-z_]\w*$", re.I).match
name_re = re.compile(r"[a-z_]\w*", re.I).match
is_entity_decl = re.compile(r"^[a-z_]\w*", re.I).match
is_int_literal_constant = re.compile(r"^\d+(_\w+|)$").match
module_file_extensions = [".f", ".f90", ".f95", ".f03", ".f08"]


def split_comma(line, item=None, comma=",", keep_empty=False, brackets=None):
    """Split (an optionally bracketed) comma-separated list into
    items and return a list containing them. If supplied then
    brackets must be a list of containing two strings, the first
    being the opening bracket and the second the closing bracket."""
    # we may have blank space so strip the line
    line = line.strip()
    if not line:
        return []
    if brackets:
        if not isinstance(brackets, tuple):
            raise ParseError("split_comma: brackets must be a tuple")
        if len(brackets) != 2:
            raise ParseError(
                "split_comma: brackets tuple must contain "
                "just two items but got: {0}",
                brackets,
            )
        open = brackets[0]
        close = brackets[1]
        if not line.startswith(open) or not line.endswith(close):
            return []
        line = line.strip(brackets[0])
        line = line.strip(brackets[1])
    items = []
    if item is None:
        for s in line.split(comma):
            s = s.strip()
            if not s and not keep_empty:
                continue
            items.append(s)
        return items
    newitem = item.copy(line, True)
    apply_map = newitem.apply_map
    for s in newitem.get_line().split(comma):
        s = apply_map(s).strip()
        if not s and not keep_empty:
            continue
        items.append(s)
    return items


def extract_bracketed_list_items(line, item=None):
    """Takes any line that contains "xxx (a,b,...) yyy" and returns
    a list of items corresponding to a, b, ... Anything outside of
    the parentheses is ignored. Only works for strings containing
    a single set of parentheses."""
    if line.count("(") > 1 or line.count(")") > 1:
        raise ParseError(
            "parse_bracketed_list: more than one opening/closing parenthesis "
            "found in string '{0}'; this is not supported".format(line)
        )
    idx1 = line.find("(")
    idx2 = line.rfind(")")
    if idx1 < 0 or idx2 < 0 or idx2 < idx1:
        raise ParseError(
            "parse_bracketed_list: failed to find expression within "
            "parentheses in '{0}'".format(line)
        )
    items = split_comma(line[idx1 : idx2 + 1], item, brackets=("(", ")"))
    if item:
        for idx in range(len(items)):
            itm = item.copy(items[idx])
            rlst = []
            for rpart in itm.get_line().split(":"):
                rlst.append(itm.apply_map(rpart.strip()))
            items[idx] = rlst
    return items


def parse_array_spec(line, item=None):
    items = []
    for spec in split_comma(line, item):
        items.append(tuple(split_comma(spec, item, comma=":", keep_empty=True)))
    return items


def specs_split_comma(line, item=None, upper=False):
    specs0 = split_comma(line, item)
    specs = []
    for spec in specs0:
        i = spec.find("=")
        if i != -1:
            kw = spec[:i].strip().upper()
            v = spec[i + 1 :].strip()
            specs.append("%s = %s" % (kw, v))
        else:
            if upper:
                spec = spec.upper()
            specs.append(spec)
    return specs


def parse_bind(line, item=None):
    if not line.lower().startswith("bind"):
        return None, line
    if item is not None:
        newitem = item.copy(line, apply_map=True)
        newline = newitem.get_line()
    else:
        newitem = None
    newline = newline[4:].lstrip()
    i = newline.find(")")
    assert i != -1, repr(newline)
    args = []
    for a in specs_split_comma(newline[1:i].strip(), newitem, upper=True):
        args.append(a)
    rest = newline[i + 1 :].lstrip()
    if item is not None:
        rest = newitem.apply_map(rest)
    return args, rest


def parse_result(line, item=None):
    if not line.lower().startswith("result"):
        return None, line
    line = line[6:].lstrip()
    i = line.find(")")
    assert i != -1, repr(line)
    name = line[1:i].strip()
    assert is_name(name), repr(name)
    return name, line[i + 1 :].lstrip()


def filter_stmts(content, classes):
    """Pop and return classes instances from content."""
    stmts = []
    indices = []
    for i in range(len(content)):
        stmt = content[i]
        if isinstance(stmt, classes):
            stmts.append(stmt)
            indices.append(i)
    indices.reverse()
    for i in indices:
        del content[i]
    return stmts


def get_module_files(directory, _cache={}):
    if directory in _cache:
        return _cache[directory]
    module_line = re.compile(r"(\A|^)module\s+(?P<name>\w+)\s*(!.*|)$", re.I | re.M)
    d = {}
    files = []
    for ext in module_file_extensions:
        files += glob.glob(os.path.join(directory, "*" + ext))
    for fn in files:
        f = open(fn, "r")
        for name in module_line.findall(f.read()):
            name = name[1]
            if name in d:
                print(d[name], "already defines", name)
                continue
            d[name] = fn
    _cache[directory] = d
    return d


def get_module_file(name, directory, _cache={}):
    fn = _cache.get(name, None)
    if fn is not None:
        return fn
    if name.endswith("_module"):
        for ext in module_file_extensions:
            f1 = os.path.join(directory, name[:-7] + ext)
            if os.path.isfile(f1):
                _cache[name] = fn
                return f1
    files = []
    for ext in module_file_extensions:
        files += glob.glob(os.path.join(directory, "*" + ext))
    for fn in files:
        if module_in_file(name, fn):
            _cache[name] = fn
            return fn
    return None


def module_in_file(name, filename):
    name = name.lower()
    pattern = re.compile(r"\s*module\s+(?P<name>[a-z]\w*)", re.I).match
    encoding = {"encoding": "UTF-8"}
    f = io.open(filename, "r", **encoding)
    for line in f:
        m = pattern(line)
        if m and m.group("name").lower() == name:
            f.close()
            return filename
    f.close()


def str2stmt(string, isfree=True, isstrict=False):
    """Convert Fortran code to Statement tree."""
    from .readfortran import Line, FortranStringReader
    from .parsefortran import FortranParser

    reader = FortranStringReader(string, isfree, isstrict)
    parser = FortranParser(reader)
    parser.parse()
    parser.analyze()
    block = parser.block
    while len(block.content) == 1:
        block = block.content[0]
    return block


def show_item_on_failure(func, _exception_depth=[0]):
    """
    Decorator for analyze methods.
    """

    def new_func(self):
        try:
            func(self)
        except AnalyzeError as msg:
            clsname = self.__class__.__name__
            self.error("%s.analyze error: %s" % (clsname, msg))
            traceback.print_exc()
        except ParseError as msg:
            self.error("parse error: %s" % (msg))
        except Exception as msg:
            _exception_depth[0] += 1
            if _exception_depth[0] == 1:
                self.error("exception triggered here: %s %s" % (Exception, msg))
            raise
        _exception_depth[0] = 0

    return new_func


_classes_cache = {}


class meta_classes(type):
    """Meta class for ``classes``."""

    __abstractmethods__ = False

    def __getattr__(self, name):
        # Expose created classes only as attributes to ``classes`` type.
        cls = _classes_cache.get(name)
        if cls is None:
            raise AttributeError("instance does not have attribute %r" % (name))
        return cls


class classes(type, metaclass=meta_classes):
    """Make classes available as attributes of this class.

    To add a class to the attributes list, one must use::

      class Name(metaclass=classes):

    in the definition of the class.

    In addition, apply the following tasks:

    * decorate analyze methods with show_item_on_failure
    """

    def __new__(metacls, name, bases, dict):
        if "analyze" in dict:
            dict["analyze"] = show_item_on_failure(dict["analyze"])
        cls = type.__new__(metacls, name, bases, dict)
        _classes_cache[name] = cls
        return cls
