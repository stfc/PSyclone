from collections import OrderedDict
from dataclasses import dataclass
import inspect
import re
from typing import List, Union

from psyclone.errors import InternalError

@dataclass
class ArgumentData():
    name: str
    datatype: object
    desc: str
    inline_type: bool

@dataclass
class RaisesData():
    desc : str
    exception: str

@dataclass
class ReturnsData():
    desc: str
    datatype: object
    inline_type: bool

@dataclass
class DocstringData():
    description : str
    arguments : OrderedDict
    raises : list
    returns : object

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
            self.description = docstring_element
        else:
            raise InternalError(
                f"Found docstring element not supported by PSyclone, expected"
                f" :raise, :param, :returns, :type, or :rtype but found "
                f"{docstring_element}."
            )


def create_docstring_data(args: List[str], desc: str) ->\
        Union[ArgumentData, RaisesData, ReturnsData]:
    '''
    Creates a docstring data object (i.e. ArgumentData, RaisesData or
    ReturnsData) from an argument list and description.

    :param args: The argument list (e.g. ['param', 'args']) for the docstring
                 entry.
    :param desc: The descrption corresponding to the docstring element.
    '''
    # If its a param then we can create an ArgumentData for this.
    if args[0] == "param":
        if len(args) == 2:
            inline_type = False
            datatype = None
            name = args[1]
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
            data_type = args[1]
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

    return f":raises {raisedata.exception}: {raisedata.desc}"


def gen_docstring_from_ArgumentData(argdata: ArgumentData) -> str:

    rstr = ":param "
    if argdata.inline_type:
        rstr += f"{argdata.datatype} {argdata.name}: {argdata.desc}"
    else:
        rstr += f"{argdata.name}: {argdata.desc}\n"
        rstr += f":type {argdata.name} {argdata.datatype}"
    
    return rstr

def gen_docstring_from_ReturnsData(rdata: ReturnsData) -> str:

    if rdata.inline_type:
        return f":returns {datatype}: {desc}"
    else:
        rstr = f":returns: {desc}\n"
        rstr += f":rtype {datatype}:"
        return rstr



def gen_docstring_from_DocstringData(docdata: DocstringData, indentation: str = "    ") -> str:
    '''

    '''
    if docdata.description is not None:
        description = indentation + docdata.description + "\n"
    else:
        description = ""

    argstrings = []
    for arg in docdata.arguments:
        argstring = gen_docstring_from_ArgumentData(docdata.arguments[arg])
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
        raisestring = indentation + gen_docstring_from_RaiseData(element)
        raisestrings.append(raisestring)

    if docdata.returns is not None:
        returnstring = gen_docstring_from_ReturnsData(docdata.returns)
        if "\n" in returnstring:
            first_line, rest = returnstring.split("\n", 1)
            returnstring = indentation + first_line + "\n"
            returnstring = indetation + rest + "\n"
    else:
        returnstring = ""

    docstring = description

    docstring += "\n".join(argstrings)
    if len(argstrings) > 0:
        docstring += "\n"

    docstring == "\n".join(raisestrings)
    if len(raisestrings) > 0:
        docstring += "\n"

    docstring += returnstring

    return docstring



def parse_psyclone_docstring_from_object(obj) -> DocstringData:
    '''TODO'''
    text = obj.__doc__

    if text is None:
        return {}

    # Remove indentation from the documentation
    text = inspect.cleandoc(text)

    # Find the first instance of ^: as the start of the meta information.
    match = re.search("^:", text, flags=re.M)
    if match:
        desc_chunk = text[: match.start()]
        meta_chunk = text[match.start() :]
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
        description = "", arguments = OrderedDict(), raises = [],
        returns = None
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
        elif len(args) in [1,2] and args[0] == "rtype":
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


    return docstring_data
