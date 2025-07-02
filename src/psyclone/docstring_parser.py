# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Authors: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
#
# This code is influenced by the docstring_parser package created by Marcin
# Kurczewski. Their licence is:
#
# The MIT License (MIT)
# Copyright (c) 2018 Marcin Kurczewski
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
'''This module contains the PSyclone docstring parsing code. This
implementation influenced by the docstring_parser package created by
Marcin Kurczewski - https://github.com/rr-/docstring_parser.
'''

from collections import OrderedDict
from dataclasses import dataclass
import inspect
import os
import re
from typing import Any, Callable, List, Union

from psyclone.errors import DocParseError


@dataclass
class ArgumentData():
    ''' Dataclass object to represent  a :param...: docstring element.

    :param str name: The name of the parameter.
    :param str datatype: The string representation of the parameter's
                         datatype.
    :param str desc: The description of the parameter.
    :param bool inline_type: whether the type of this parameter should be
                             inline or a separate :type param: docstring line.
    '''
    name: str
    datatype: str
    desc: str
    inline_type: bool

    def gen_docstring(self, function: Union[None, Callable[..., Any]] = None)\
            -> str:
        '''
        :param function: The function who the generated docstring will be for.
                    Default option is None. If no function is supplied, there
                    can be no type annotation and so the type information is
                    included inline in the :param or in a separate :type
                    entry.
        :returns: The docstring represented by this ArgumentData.
        '''
        rstr = ":param "
        if function:
            # If the argument is in function's parameter list and has a type
            # hint then we don't add the type information.
            signature = inspect.signature(function)
            val = signature.parameters.get(self.name)
            if (val is not None and val.annotation is not
                    inspect.Parameter.empty):
                rstr += f"{self.name}: {self.desc}"
                return rstr

        if self.inline_type:
            rstr += f"{self.datatype} {self.name}: {self.desc}"
        else:
            rstr += f"{self.name}: {self.desc}{os.linesep}"
            rstr += f":type {self.name}: {self.datatype}"

        return rstr


@dataclass
class RaisesData():
    ''' Dataclass object to represent a :raises...: docstring element.

    :param str desc: The description of the raises docstring.
    :param str exception: The exception or error raised.
    '''
    desc: str
    exception: str

    def gen_docstring(self) -> str:
        '''
        :returns: The docstring for the input raisedata.
        '''
        return f":raises {self.exception}: {self.desc}"


@dataclass
class ReturnsData():
    ''' Dataclass object to represent a :returns: docstring element.

    :param str desc: The description of the returns docstring.
    :param str datatype: The string representation of the return datatype.
    '''
    desc: str
    datatype: object

    def gen_docstring(self) -> str:
        '''
        :returns: The docstring for this ReturnsData object.
        '''
        rstr = f":returns: {self.desc}"
        if self.datatype is not None:
            rstr += f"{os.linesep}:rtype: {self.datatype}"
        return rstr


@dataclass
class DocstringData():
    '''
    Dataclass object representing a full docstring.

    :param str desc: The description of the docstring.
    :param OrderedDict[str, ArgumentData] arguments: An OrderedDict containing
        the ArgumentData that respresents the parameter section of the
        docstring, indexed by the parameter name.
    :param list[RaisesData] raises: A list containing the RaisesData that
        represents the raises section of the docstring.
    :param Union[ReturnsData, None] returns: The ReturnsData that represents
        the returns section of the docstring. Can be set to None if there is
        no return docstring.
    '''
    desc: str
    arguments: OrderedDict
    raises: list
    returns: ReturnsData

    def add_data(self, docstring_element:
                 Union[ArgumentData, RaisesData, ReturnsData, str]) -> None:
        '''
        Adds the docstring_element to the DocstringData object. If there is a
        previous docstring_element in this DocstringData it will be
        overwritten.

        :docstring_element: The docstring_element object to be added to the
                            DocstringData.

        :raises DocParseError: If a docstring element not supported by
                               PSyclone is found.
        '''
        if isinstance(docstring_element, ArgumentData):
            self.arguments[docstring_element.name] = docstring_element
        elif isinstance(docstring_element, RaisesData):
            self.raises.append(docstring_element)
        elif isinstance(docstring_element, ReturnsData):
            self.returns = docstring_element
        elif isinstance(docstring_element, str):
            self.desc = docstring_element
        else:
            raise DocParseError(
                f"Found docstring element not supported by PSyclone, expected"
                f" :raise, :param, :returns, :type, or :rtype but found "
                f"'{docstring_element}'."
            )

    def merge(self, other_data, replace_desc: bool = False,
              replace_args: bool = False, replace_returns: bool = False):
        '''
        Merges the other_data DocstringData object into this one.
        The user can specify if data in other_data overwrites the data
        in this DocstringData by setting the replace arguments to True.

        If any objects in this DocstringData are None they will be overwritten
        always.

        :param other_data: the DocstringData object to merge into this object.
        :type other_data: :py:class:`psyclone.docstring_parser.DocstringData`
        :param replace_desc: whether to replace the desc with that of
                             other_data.
        :param replace_args: whether to replace duplicate arguments with that
                             of other_data.
        :param replace_returns: whether to overwrite the returns data with that
                                of other_data.
        '''
        # Merge the desc if needed.
        if ((self.desc is None or replace_desc)
                and other_data.desc is not None):
            self.desc = other_data.desc

        # Add the arguments.
        for arg in other_data.arguments:
            if arg in self.arguments.keys() and not replace_args:
                continue
            self.arguments[arg] = other_data.arguments[arg]

        # Add the raises.
        for raises in other_data.raises:
            self.raises.append(raises)

        # Merge the returns if needed.
        if ((self.returns is None or replace_returns)
                and other_data.returns is not None):
            self.returns = other_data.returns

    def gen_docstring(
            self, indentation: str = "    ",
            function: Union[None, Callable[..., Any]] = None
    ) -> str:
        '''
        Generates the docstring from the input docdata. The indentation of the
        docstring can be specified and by default is 4 spaces.

        :param indentation: The indentation to use. Default is "    ".
        :param function: The function (e.g. apply or validate function of a
                    Transformation) who the generated docstring will be for.
                    Default option is None. If no function is supplied, there
                    can be no type annotations and so the type information
                    is included inline in the :param or in a separate :type
                    entry.

        :returns: The docstring representation of this DocData object.
        '''
        if self.desc is not None:
            # Need to indent the docstring description if it is multiline.
            if os.linesep in self.desc:
                lines = self.desc.split(os.linesep)
                description = ""
                for line in lines:
                    description += indentation + line + os.linesep
            else:
                description = indentation + self.desc + os.linesep
        else:
            description = ""

        argstrings = []
        for arg in self.arguments:
            argstring = self.arguments[arg].gen_docstring(function)
            if os.linesep in argstring:
                lines = argstring.split(os.linesep)
                argstring = indentation + lines[0] + os.linesep
                for line in lines[1:]:
                    if ":type" not in line:
                        argstring += indentation*2 + line + os.linesep
                    else:
                        argstring += indentation + line + os.linesep
                # Remove the last newline character
                argstring = argstring[:-1]
            else:
                argstring = indentation + argstring
            argstrings.append(argstring)

        raisestrings = []
        for element in self.raises:
            raisesdocstring = element.gen_docstring()
            if os.linesep in raisesdocstring:
                lines = raisesdocstring.split(os.linesep)
                raisestring = indentation + lines[0] + os.linesep
                for line in lines[1:]:
                    raisestring += indentation*2 + line.rstrip() + os.linesep
                # Remove the last \n from the generated string
                raisestring = raisestring[:-1]
            else:
                raisestring = indentation + raisesdocstring
            raisestrings.append(raisestring)

        if self.returns is not None:
            returnstring = self.returns.gen_docstring()
            if os.linesep in returnstring:
                lines = returnstring.split(os.linesep)
                returnstring = ""
                for line in lines:
                    returnstring += indentation + line + os.linesep
            else:
                returnstring = indentation + returnstring + os.linesep
        else:
            returnstring = ""

        docstring = description
        docstring += os.linesep.join(argstrings)
        if len(argstrings) > 0:
            docstring += os.linesep

        # Add an empty line before params and returns.
        if len(argstrings) > 0 and self.returns is not None:
            docstring += os.linesep
        docstring += returnstring

        # Add an empty line between returns and raises
        if ((self.returns is not None or len(argstrings) > 0)
                and len(raisestrings) > 0):
            docstring += os.linesep
        docstring += os.linesep.join(raisestrings)
        if len(raisestrings) > 0:
            docstring += os.linesep

        return docstring

    @classmethod
    def create_from_object(cls, obj: Any):
        '''
        Converts the docstring of obj into a DocstringData object. Only
        supports docstring used in PSyclone, so some valid docstring may
        result in failure.

        :param Any obj: The object whose docstring will be parsed.

        :returns: A DocstringData object representing the objects docstring or
                  None if obj has no docstring.
        :rtype: Union[:py:class:`psyclone.docstring_parser.DocData`, None]

        :raises DocParseError: if a docstring element cannot be parsed.
        :raises DocParseError: if a type docstring is found with no
                               corresponding parameter docstring.
        '''
        text = obj.__doc__

        if text is None:
            return None

        # Remove indentation from the documentation
        text = inspect.cleandoc(text)

        # Find the first instance of ^: as the start of the meta information.
        match = re.search("^:(param|type|raise|return|rtype)", text,
                          flags=re.M)
        if match:
            desc_chunk = text[:match.start()]
            meta_chunk = text[match.start():]
        else:
            desc_chunk = text
            meta_chunk = ""

        # We don't care about breaking down the description in PSyclone
        # for now, so just keep the desc_chunk as text.

        # Store types and return types as separate things to integrate later.
        types = {}
        rtype = None
        docstring_data = DocstringData(
            desc="", arguments=OrderedDict(), raises=[],
            returns=None
        )

        docstring_data.add_data(desc_chunk)

        # Loop through the meta chunk for each line in that section that has
        # :...:
        for match in re.finditer(
            r"(^:.*?)(?=^:|\Z)", meta_chunk, flags=re.S | re.M
        ):
            chunk = match.group(0)

            # Split the text into the section between the two :s and after.
            try:
                args_chunk, desc_chunk = chunk.lstrip(":").split(":", 1)
            except ValueError as ex:
                # Can be cause by something like "\n::" in the docstring.
                raise DocParseError(
                    f'Error parsing meta information near "{chunk}".'
                ) from ex
            args = args_chunk.split()
            desc = desc_chunk.strip()

            # If we have a multiline description then we need to clean it up.
            # The docstring already removes the \n if it contains \ , so
            # we remove the leftover indentation
            if "  " in desc:
                lines = desc.split("  ")
                desc = inspect.cleandoc(lines[0]) + os.linesep
                for line in lines[1:]:
                    desc_line = inspect.cleandoc(line)
                    # If this section of the split was only whitespace
                    # we can ignore it.
                    if desc_line != "":
                        desc = desc + desc_line + os.linesep
                # Remove the trailing \n
                desc = desc[:-1]

            # If we have a multiline description without \ to break lines then
            # it instead contains \n which we can clean up similarly.
            if os.linesep in desc:
                lines = desc.split(os.linesep)
                desc = lines[0] + os.linesep
                for line in lines[1:]:
                    desc += inspect.cleandoc(line) + os.linesep
                # Remove the trailing \n
                desc = desc.rstrip()

            # Special handling for :type: or :rtype: lines.
            if len(args) == 2 and args[0] == "type":
                types[args[1]] = desc
            elif len(args) in [1, 2] and args[0] == "rtype":
                rtype = desc
            else:
                # Get the information.
                docdata = create_docstring_data(args, desc, obj)
                docstring_data.add_data(docdata)

        # Store the separate (i.e. not inline) type information that was
        # extracted.
        for param in types:
            if param not in docstring_data.arguments.keys():
                raise DocParseError(
                    f"Found a type string with no corresponding parameter: "
                    f"'{param}' type found with no parameter docstring."
                )
            docstring_data.arguments[param].datatype = types[param]
            docstring_data.arguments[param].inline_type = False

        # Store the return type if specified.
        if rtype:
            docstring_data.returns.datatype = rtype

        return docstring_data


def create_docstring_data(args: List[str], desc: str,
                          function: Callable[..., Any]) ->\
        Union[ArgumentData, RaisesData, ReturnsData]:
    '''
    Creates a docstring data object (i.e. ArgumentData, RaisesData or
    ReturnsData) from an argument list and description.

    :param args: The argument list (e.g. ['param', 'args']) for the docstring
                 entry.
    :param desc: The description corresponding to the docstring element.
    :param function: The function that is having its docstring analysed.

    :raises DocParseError: If an unsupported docstring input is found.

    :returns: An object representing the input arg and desc.
    '''
    # Import here to disable circular dependency if Sphinx isn't available.
    try:
        # pylint: disable=import-outside-toplevel
        from sphinx.util.typing import stringify_annotation
    except ImportError:
        # pylint: disable=import-outside-toplevel
        from psyclone.utils import stringify_annotation
    # If its a param then we can create an ArgumentData for this.
    if args[0] == "param":
        if len(args) == 2:
            # Parameter of form :param name: desc.
            inline_type = False
            datatype = None
            name = args[1]
            # Check if there is a datatype on the argument itself and store
            # it.
            signature = inspect.signature(function)
            val = signature.parameters.get(name, None)
            if (val is not None and val.annotation
                    is not inspect.Parameter.empty):
                datatype = stringify_annotation(
                    val.annotation
                )
                inline_type = True
        elif len(args) > 2:
            # Parameter of form :param type name: desc.
            inline_type = True
            datatype = " ".join(args[1:-1])
            name = args[-1]
        else:
            raise DocParseError(
                f"Found parameter docstring of unsupported type, expected "
                f":param arg: or :param type arg: but found "
                f":{' '.join(args)}:"
            )
        return ArgumentData(
            name=name, desc=desc, datatype=datatype, inline_type=inline_type
        )
    elif args[0] == "raises":
        # Raises must be of form :raises Error: desc
        if len(args) != 2:
            raise DocParseError(
                f"Found raises docstring of unsupported type, expected "
                f":raises Error: but found :{' '.join(args)}:"
            )
        return RaisesData(exception=args[1], desc=desc)
    elif args[0] in ["return", "returns"]:
        if len(args) == 1:
            # Return of form :returns: desc
            datatype = None
        else:
            raise DocParseError(
                f"Found return docstring of unsupported type, expected "
                f":returns: but found :{' '.join(args)}:"
            )
        return ReturnsData(
            datatype=datatype, desc=desc
        )
    # Any other docstring is unsupported
    raise DocParseError(
        f"Found unsupported docstring: :{' '.join(args)}: "
    )


# For Sphinx AutoAPI documentation generation
__all__ = ['ArgumentData', 'RaisesData', 'ReturnsData', 'DocstringData',
           'create_docstring_data']
