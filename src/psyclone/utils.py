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

import os
import re
import sys
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


def stringify_annotation(annotation) -> str:
    ''' Simple PSyclone method to turn a Python type annotation to a string
    when sphinx is not available.

    :param annotation: The type annotation to convert to a string.
    :type annotation: TypeAliasType

    :returns: The string representation of annotation.
    '''
    return str(annotation)


def transformation_documentation_wrapper(cls, *args, inherit=True, **kwargs):
    '''
    Updates the apply and validate methods' docstrings for the supplied cls,
    according to the value of inherit.

    :param Class cls: The class whose docstrings are to be updated.
    :param inherit: whether to inherit argument docstrings from cls' parent's
                    apply method. If the provided argument is a list, instead
                    the docstrings are updated from each class included in
                    the provided list, to support metatransformations. If
                    inherit is False, the wrapper will just update the
                    Transformation's validate docstring from its own
                    apply docstring.
    :type inherit: Union[list[Class], bool]
    '''
    def get_inherited_parameters(cls, func, inheriting_func) -> str:
        '''
        Returns the docstring to be inherited by func from inheriting_func.

        :param Class cls: The class containing func.
        :param Callable func: The function to which the docstrings will be
                              added.
        :param Callable inheriting_func: The function in which to inherit the
                                         docstrings from.

        :returns: The docstrings to be added to func.

        :raises InternalError: if inheriting_func contains docstring for an
                               argument that isn't part of
                               cls.get_valid_options.
        :raises InternalError: if inheriting_func doesn't have a type
                               for an argument that doesn't have a typehint.
        '''
        docs = func.__doc__
        parent_docstring = inheriting_func.__doc__
        if parent_docstring is not None:
            parent_lines = parent_docstring.splitlines()
        else:
            parent_lines = []
        added_docs = ""
        for x, line in enumerate(parent_lines):
            if ":param" in line:
                param_str = re.search(":[a-zA-Z0-9_\\s]*:", line)
                if param_str is None or param_str.group() in docs:
                    continue
                param_line = line + os.linesep
#                added_docs += line + os.linesep
                z = x+1
                type_found = False
                while z < len(parent_lines):
                    # or ":raises" in parent_lines[z]):
                    # FIXME Raises not yet handled
                    if (":param" in parent_lines[z]):
                        # If we didn't find the :type: docstring,
                        # we need to create it to inherit the docstring
                        # correctly.
                        # FIXME To review - maybe this section should be
                        # refactored into a function?
                        if not type_found:
                            # Find the parameter name
                            param_name_grp = re.search(
                                    "([a-zA-Z0-9_]*):",
                                    line[line.index(":param")+6:]
                            )
                            param_name = param_name_grp.group(0)
                            param_name = param_name[:param_name.index(":")]
                            valid_opts = cls.get_valid_options()
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
                            # FIXME This needs to be if its in param_name_grp
                            if type_string in param_line:
                                added_docs += param_line
                                break
                            # Add the type into the param string
                            param_index = line.index(":param")
                            param_line = (param_line[:param_index+7]
                                          + type_string + " " +
                                          param_line[param_index+7:])
                            added_docs += param_line
                        break
                    if ":type" in parent_lines[z]:
                        type_found = True
                        stripped_line = param_line.lstrip()
                        added_docs += 8*" " + stripped_line + os.linesep
                        stripped_line = parent_lines[z].lstrip()
                        added_docs += 8*" " + stripped_line + os.linesep
                    elif not parent_lines[z].isspace():
                        stripped_line = parent_lines[z].lstrip()
                        param_line += 12*" " + stripped_line + os.linesep
                    z = z + 1
                else:
                    # If we don't break out of the loop we still need to check
                    # if we find the type documentation for the argument.
                    if not type_found:
                        # Find the parameter name
                        param_name_grp = re.search(
                                "([a-zA-Z0-9_]*):",
                                line[line.index(":param")+6:]
                        )
                        param_name = param_name_grp.group(0)
                        param_name = param_name[:param_name.index(":")]
                        valid_opts = cls.get_valid_options()
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
                        # FIXME This needs to be if its in param_name_grp?
                        if type_string in param_line:
                            added_docs += param_line
                            break
                        # Add the type into the param string
                        param_index = line.index(":param")
                        param_line = (param_line[:param_index+7]
                                      + type_string + " " +
                                      param_line[param_index+7:])
                        added_docs += param_line
        # Add an extra indented blank line?
        added_docs += 8*" " + os.linesep
        return added_docs

    def update_func_docstring(func, added_parameters: str) -> None:
        '''
        Adds the docstrings specified in added_parameters to the
        docstring of func.

        :param Callable func: The func which is to have its docstrings updated.
        :param added_parameters: The lines of docstring to add into the
                                 docstring of func.
        '''
        doc = func.__doc__
        # Find the last instance of :param or :type
        if doc is not None:
            doc_lines = doc.splitlines()
        else:
            doc_lines = []
        last_instance = 0
        for i, line in enumerate(doc_lines):
            if ":param" in line or ":type" in line:
                last_instance = i
                x = i+1
                while x < len(doc_lines):
                    if doc_lines[x].isspace():
                        break
                    if not (":param" in doc_lines[x] or ":type" in
                            doc_lines[x] or ":raise" in doc_lines[x]):
                        # This is part of the previous section.
                        last_instance = x
                        x = x + 1
                    else:
                        break
        new_docs = ""
        for i in range(min(last_instance+1, len(doc_lines))):
            new_docs += doc_lines[i] + os.linesep
        # Remove any trailing whitespace, then add a newline
        new_docs = new_docs.rstrip() + os.linesep
        new_docs += added_parameters + os.linesep

        for i in range(last_instance+1, len(doc_lines)):
            new_docs += doc_lines[i] + os.linesep

        # Remove any trailing whitespace, then add a newline
        new_docs = new_docs.rstrip() + os.linesep
        func.__doc__ = new_docs

    def wrapper():
        '''
        The wrapping function of the decorator.

        '''
        if isinstance(inherit, list):
            added_parameters = ""
            for superclass in inherit:
                added_parameters += get_inherited_parameters(
                    superclass, cls.apply,
                    superclass.apply
                )
        elif inherit:
            added_parameters = get_inherited_parameters(
                    cls.__mro__[1], cls.apply,
                    cls.__mro__[1].__dict__["apply"]
            )
        else:
            added_parameters = ""
        update_func_docstring(cls.apply, added_parameters)
        # Update the validate docstring
        add_parameters = get_inherited_parameters(cls, cls.validate, cls.apply)
        update_func_docstring(cls.validate, add_parameters)

        return cls
    return wrapper(*args, **kwargs)
