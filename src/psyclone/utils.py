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
# Authors: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module provides generic utility functions.'''

from collections import OrderedDict
import sys
from psyclone.errors import InternalError
from psyclone.docstring_parser import (
    DocstringData,
)


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
    # List of argument doctrings to never inherit.
    _uninheritable_args = ["options"]

    def update_func_docstring(func, added_parameters: DocstringData) -> None:
        '''
        Adds the docstrings specified in added_parameters to the
        docstring of func.

        :param Callable func: The func which is to have its docstrings updated.
        :param added_parameters: The DocstringData to add into the
                                 docstring of func.
        '''
        func_data = DocstringData.create_from_object(func)

        # We only merge the arguments.
        added_parameters.desc = None
        added_parameters.raises = []
        added_parameters.returns = None

        func_data.merge(added_parameters, replace_args=False)
        func.__doc__ = func_data.gen_docstring(function=func)

    def wrapper():
        '''
        The wrapping function of the decorator.

        :raises InternalError: if cls is not a Transformation.
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import Transformation
        if not issubclass(cls, Transformation):
            raise InternalError(
                f"transformation_documentation_wrapper expected a "
                f"Transformation but got '{cls.__name__}'"
            )
        if isinstance(inherit, list):
            added_parameters = DocstringData(
                desc=None, arguments=OrderedDict(),
                raises=[], returns=None)
            for superclass in inherit:
                inherited_params = \
                    DocstringData.create_from_object(superclass.apply)
                added_parameters.merge(inherited_params)
        elif inherit:
            added_parameters = \
                DocstringData.create_from_object(
                    cls.__mro__[1].__dict__["apply"]
                 )
        else:
            added_parameters = None
        if added_parameters is not None:
            # Remove any arguments we don't want to inherit.
            for arg in list(added_parameters.arguments.keys()):
                if arg in _uninheritable_args:
                    del added_parameters.arguments[arg]
            update_func_docstring(cls.apply, added_parameters)
        # Update the validate docstring
        added_parameters = DocstringData.create_from_object(cls.apply)
        if added_parameters is not None:
            update_func_docstring(cls.validate, added_parameters)

        return cls
    return wrapper(*args, **kwargs)
