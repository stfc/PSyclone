# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council.
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
# Authors: A. R. Porter and R. W. Ford, STFC Daresbury Lab

'''This module provides generic utility functions.'''

import sys
import re
from psyclone.errors import InternalError


def within_virtual_env():
    '''
    Utility function that identifies whether we are running in a Python
    virtual environment. Works for virtualenv and Python 3's venv.

    :returns: True if we're running in a virtual environment.
    :rtype: bool

    '''
    # pylint: disable=no-member
    return (hasattr(sys, 'real_prefix') or
            (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix))


def a_or_an(string):
    '''Returns 'an' if the supplied string starts with a vowel or 'h',
    otherwise returns 'a'.

    :param str string: the string to check.

    :returns: 'an' if the supplied string starts with a vowel or 'h', \
        otherwise returns 'a'.
    :rtype: str

    '''
    char = string[0].lower()
    if char in ["a", "e", "i", "o", "u", "h"]:
        return "an"
    return "a"


def transformation_documentation_wrapper(cls, *args, inherit=True, **kwargs):
    def get_inherited_parameters(cls):
        docs = cls.apply.__doc__
        parent_docstring = cls.__mro__[1].__dict__["apply"].__doc__
        if parent_docstring is not None:
            parent_lines = parent_docstring.splitlines()
        else:
            parent_lines = []
        added_docs = ""
        for x, line in enumerate(parent_lines):
            if ":param" in line:
                param_str = re.search(":[a-zA-Z0-9\\s]*:", line)
                if param_str is None or param_str.group() in docs:
                    continue
                added_docs += "\n" + line
                z = x+1
                type_found = False
                while z < len(parent_lines):
                    if (":param" in parent_lines[z]
                       or ":raises" in parent_lines[z]):
                        # If we didn't find the :type: docstring,
                        # we need to create it to inherit the docstring
                        # correctly.
                        # FIXME To review - maybe this section should be
                        # refactored into a function?
                        if not type_found:
                            # Find the parameter name
                            param_name_grp = re.search(
                                    ":param ([a-zA-Z0-9_]*):", line
                            )
                            param_name = param_str.group(1)
                            valid_opts = cls.__mro__[1].get_valid_options()
                            if param_name not in valid_opts.keys():
                                raise InternalError(
                                    f"Invalid documentation found when "
                                    f"generating inherited documentation "
                                    f"for class '{cls.__name__}'."
                                )
                            type_string = valid_opts[param_name]['typename']
                            if type_string is None:
                                raise InternalError(
                                    f"Invalid documentation found when "
                                    f"generating inherited documentation "
                                    f"for class '{cls.__name__}' as the "
                                    f"'{param_name}' arg has no known type."
                                )
                            type_doc = "        "
                            type_doc += f":type {param_name}: {type_string}"
                            added_docs += "\n" + type_doc
                        break
                    if ":type" in parent_lines[z]:
                        type_found = True
                    if not parent_lines[z].isspace():
                        added_docs += "\n" + parent_lines[z]
                    z = z + 1
                else:
                    # If we don't break out of the loop we still need to check
                    # if we find the type documentation for the argument.
                    if not type_found:
                        # Find the parameter name
                        param_name_grp = re.search(
                                ":param ([a-zA-Z0-9_]*):", line
                        )
                        param_name = param_name_grp.group(1)
                        valid_opts = cls.__mro__[1].get_valid_options()
                        if param_name not in valid_opts.keys():
                            raise InternalError(
                                f"Invalid documentation found when "
                                f"generating inherited documentation "
                                f"for class '{cls.__name__}'."
                            )
                        type_string = valid_opts[param_name]['typename']
                        if type_string is None:
                            raise InternalError(
                                f"Invalid documentation found when "
                                f"generating inherited documentation "
                                f"for class '{cls.__name__}' as the "
                                f"'{param_name}' arg has no known type."
                            )
                        type_doc = "        "
                        type_doc += f":type {param_name}: {type_string}"
                        added_docs += "\n" + type_doc
        return added_docs

    def update_apply(cls, added_parameters):
        doc = cls.apply.__doc__
        # Find the last instance of :param or :type
        doc_lines = doc.splitlines()
        for i, line in enumerate(doc_lines):
            if ":param" in line or ":type" in line:
                last_instance = i
                x = i+1
                while x < len(doc_lines):
                    if not (":param" in doc_lines[x] or ":type" in
                            doc_lines[x] or ":raise" in doc_lines[x]):
                        # This is part of the previous section.
                        last_instance = x
                        x = x + 1
                    else:
                        break
        new_docs = ""
        for i in range(last_instance+1):
            new_docs += doc_lines[i] + "\n"

        new_docs += added_parameters + "\n"

        for i in range(last_instance+1, len(doc_lines)):
            new_docs += doc_lines[i] + "\n"

        cls.apply.__doc__ = new_docs

    def wrapper():
        if inherit:
            added_parameters = get_inherited_parameters(cls)
        else:
            added_parameters = ""
        update_apply(cls, added_parameters)
        # TODO VALIDATE isn't updated yet.

        return cls
    return wrapper(*args, **kwargs)
