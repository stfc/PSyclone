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
'''This module contains the PSyclone docstring parsing code.'''

from collections import OrderedDict
from dataclasses import dataclass
import inspect
import re
from typing import Any, List, Union

from psyclone.errors import InternalError

try:
    from sphinx.util.typing import stringify_annotation
except ImportError:
    # Fix for Python-3.7 where sphinx didn't yet rename this.
    # TODO 2837: Can remove this 3.7 sphinx import
    try:
        from sphinx.util.typing import stringify as stringify_annotation
    # Igoring coverage from the no sphinx workaround as too difficult to do
    except ImportError:  # pragma: no cover
        from psyclone.utils import stringify_annotation  # pragma: no cover


@dataclass
class ArgumentData():
    name: str
    datatype: object
    desc: str
    inline_type: bool


@dataclass
class RaisesData():
    desc: str
    exception: str


@dataclass
class ReturnsData():
    desc: str
    datatype: object
    inline_type: bool


@dataclass
class DocstringData():
    desc: str
    arguments: OrderedDict
    raises: list
    returns: ReturnsData

    def add_data(self, docstring_element:
                 Union[ArgumentData, RaisesData, ReturnsData, str]) -> None:
        '''
        Adds the docstring_element to the DocstringData object.

        :docstring_element: The docstring_element object to be added to the
                            DocstringData.
        '''
        # FIXME We can do some checking that its not already present? But
        # maybe we want to allow overwriting sometimes - optional argument.
        if isinstance(docstring_element, ArgumentData):
            self.arguments[docstring_element.name] = docstring_element
        elif isinstance(docstring_element, RaisesData):
            self.raises.append(docstring_element)
        elif isinstance(docstring_element, ReturnsData):
            self.returns = docstring_element
        elif isinstance(docstring_element, str):
            self.desc = docstring_element
        else:
            raise InternalError(
                f"Found docstring element not supported by PSyclone, expected"
                f" :raise, :param, :returns, :type, or :rtype but found "
                f"'{docstring_element}'."
            )


def create_docstring_data(args: List[str], desc: str, obj: Any) ->\
        Union[ArgumentData, RaisesData, ReturnsData]:
    '''
    Creates a docstring data object (i.e. ArgumentData, RaisesData or
    ReturnsData) from an argument list and description.

    :param args: The argument list (e.g. ['param', 'args']) for the docstring
                 entry.
    :param desc: The description corresponding to the docstring element.
    :param obj: The object that is having its docstring analysed.

    :raises InternalError: If an unsupported docstring input is found.

    :returns: An object representing the input arg and desc.
    '''
    # If its a param then we can create an ArgumentData for this.
    if args[0] == "param":
        if len(args) == 2:
            inline_type = False
            datatype = None
            name = args[1]
            # Check if there is a datatype on the argument itself and store
            # it.
            signature = inspect.signature(obj)
            for k, v in signature.parameters.items():
                if k != name:
                    continue
                if v.annotation is not inspect.Parameter.empty:
                    datatype = stringify_annotation(
                        v.annotation
                    )
                    inline_type = True
        elif len(args) == 3:
            inline_type = True
            datatype = args[1]
            name = args[2]
        else:
            raise InternalError(
                f"Found parameter docstring of unsupported type, expected "
                f":param arg: or :param type arg: but found "
                f":{' '.join(args)}:"
            )
        return ArgumentData(
            name=name, desc=desc, datatype=datatype, inline_type=inline_type
        )
    elif args[0] == "raises":
        if len(args) != 2:
            raise InternalError(
                f"Found raises docstring of unsupported type, expected "
                f":raises Error: but found :{' '.join(args)}:"
            )
        return RaisesData(exception=args[1], desc=desc)
    elif args[0] == "returns":
        if len(args) == 1:
            inline_type = False
            datatype = None
        elif len(args) == 2:
            inline_type = True
            datatype = args[1]
        else:
            raise InternalError(
                f"Found return docstring of unsupported type, expected "
                f":returns: or :returns type: but found :{' '.join(args)}:"
            )
        return ReturnsData(
            inline_type=inline_type, datatype=datatype, desc=desc
        )
    # Any other docstring is unsupported
    raise InternalError(
        f"Found unsupported docstring: :{' '.join(args)}: "
    )


def gen_docstring_from_RaisesData(raisedata: RaisesData) -> str:
    '''
    :param raisedata: The RaisesData object to create docstring for.
    :returns: The docstring for the input raisedata.
    '''

    return f":raises {raisedata.exception}: {raisedata.desc}"


def gen_docstring_from_ArgumentData(
        argdata: ArgumentData, obj: Union[None, Any] = None
) -> str:
    '''
    :param argdata: The ArgumentData object to create docstring for.
    :returns: The docstring for the input argdata.
    '''
    rstr = ":param "
    if obj:
        signature = inspect.signature(obj)
        for k, v in signature.parameters.items():
            if k != argdata.name:
                continue
            if v.annotation is not inspect.Parameter.empty:
                rstr += f"{argdata.name}: {argdata.desc}"
                return rstr

    if argdata.inline_type:
        rstr += f"{argdata.datatype} {argdata.name}: {argdata.desc}"
    else:
        rstr += f"{argdata.name}: {argdata.desc}\n"
        rstr += f":type {argdata.name}: {argdata.datatype}"

    return rstr


def gen_docstring_from_ReturnsData(rdata: ReturnsData) -> str:
    '''
    :param rdata: The ReturnsData object to create docstring for.
    :returns: The docstring for the input rdata.
    '''
    if rdata.inline_type:
        return f":returns {rdata.datatype}: {rdata.desc}"
    else:
        rstr = f":returns: {rdata.desc}\n"
        rstr += f":rtype: {rdata.datatype}"
        return rstr


def gen_docstring_from_DocstringData(
        docdata: DocstringData, indentation: str = "    ",
        obj: Union[None, Any] = None
) -> str:
    '''
    Generates the docstring from the input docdata. The indentation of the
    docstring can be specified and by default is 4 spaces.

    :param docdata: The docdata object to convert to docstring.
    :param indentation: The indentation to use. Default is "    ".
    :param obj: The object who the generated docstring will be for. Default
                option is None, which means to generate the full docstring
                including all type lines.

    :returns: The docstring for the input docdata.
    '''
    if docdata.desc is not None:
        description = indentation + docdata.desc + "\n"
    else:
        description = ""

    argstrings = []
    for arg in docdata.arguments:
        argstring = gen_docstring_from_ArgumentData(
                docdata.arguments[arg], obj
        )
        if "\n" in argstring:
            lines = argstring.split("\n")
            argstring = ""
            for line in lines:
                argstring += indentation + line + "\n"
            argstring = argstring[:-1]
        else:
            argstring = indentation + argstring
        argstrings.append(argstring)

    raisestrings = []
    for element in docdata.raises:
        raisestring = indentation + gen_docstring_from_RaisesData(element)
        raisestrings.append(raisestring)

    if docdata.returns is not None:
        returnstring = gen_docstring_from_ReturnsData(docdata.returns)
        if "\n" in returnstring:
            first_line, rest = returnstring.split("\n", 1)
            returnstring = indentation + first_line + "\n"
            returnstring += indentation + rest + "\n"
        else:
            returnstring = indentation + returnstring + "\n"
    else:
        returnstring = ""

    docstring = description

    docstring += "\n".join(argstrings)
    if len(argstrings) > 0:
        docstring += "\n"

    # Add an empty line between param and raises
    if len(argstrings) > 0 and len(raisestrings) > 0:
        docstring += "\n"
    docstring += "\n".join(raisestrings)
    if len(raisestrings) > 0:
        docstring += "\n"

    docstring += returnstring

    return docstring


def parse_psyclone_docstring_from_object(
        obj: Any
) -> Union[DocstringData, None]:
    '''
    Converts the docstring of obj into a DocstringData object. Only supports
    docstring used in PSyclone, so some valid docstring may result in failure.

    :param Any obj: The object whose docstring will be parsed.

    :raises ValueError: if a docstring element cannot be parsed.

    :returns: A DocstringData object representing the objects docstring or
              None if obj has no docstring.
    '''
    text = obj.__doc__

    if text is None:
        return None

    # Remove indentation from the documentation
    text = inspect.cleandoc(text)

    # Find the first instance of ^: as the start of the meta information.
    match = re.search("^:", text, flags=re.M)
    if match:
        desc_chunk = text[:match.start()]
        meta_chunk = text[match.start():]
    else:
        desc_chunk = text
        meta_chunk = ""

    # Can break the ValueError with something like this
    #    meta_chunk += "\n::"
    # We don't care about breaking down the description in PSyclone
    # for now, so just keep the desc_chunk as text.

    # Store types and return types as separate things to integrate later.
    types = {}
    rtype = None
    docstring_data = DocstringData(
        description="", arguments=OrderedDict(), raises=[],
        returns=None
    )

    docstring_data.add_data(desc_chunk)

    # Loop through the meta chunk for each line that section that has
    # :...:
    for match in re.finditer(
        r"(^:.*?)(?=^:|\Z)", meta_chunk, flags=re.S | re.M
    ):
        chunk = match.group(0)

        # If chunk is None we should stop (how can this be?)
        if not chunk:
            continue

        # Split the text into the section between the two :s and after.
        try:
            args_chunk, desc_chunk = chunk.lstrip(":").split(":", 1)
        except ValueError as ex:
            raise ValueError(
                f'Error parsing meta information near "{chunk}".'
            ) from ex
        args = args_chunk.split()
        desc = desc_chunk.strip()

        # If we have a multiline description then we need to clean it up. The
        # docstring already removes the \n if it contains \ , so we remove the
        # leftover indentation
        if "  " in desc:
            first_line, rest = desc.split("  ", 1)
            desc = first_line + "\n" + inspect.cleandoc(rest)

        # If we have a multiline description without \ to break lines then it
        # instead contains \n which we can clean up similarly.
        if "\n" in desc:
            first_line, rest = desc.split("\n", 1)
            desc = first_line + "\n" + inspect.cleandoc(rest)

        # Special handling for :type: or :rtype: lines.
        if len(args) == 2 and args[0] == "type":
            types[args[1]] = desc
        elif len(args) in [1, 2] and args[0] == "rtype":
            rtype = desc
        else:
            # Get the information.
            docdata = create_docstring_data(args, desc)
            docstring_data.add_data(docdata)

    for param in types:
        if param not in docstring_data.arguments.keys():
            assert False
        docstring_data.arguments[param].datatype = types[param]
        docstring_data.arguments[param].inline_type = False

    if rtype:
        docstring_data.returns.datatype = rtype
        docstring_data.returns.inline_type = False

    return docstring_data

# For Sphinx AutoAPI documentation generation
__all__ = ['ArgumentData', 'RaisesData', 'ReturnsData', 'DocstringData',
           'create_docstring_data', 'gen_docstring_from_RaisesData',
           'gen_docstring_from_ArgumentData',
           'gen_docstring_from_ReturnsData',
           'gen_docstring_from_DocstringData',
           'parse_psyclone_docstring_from_object']
