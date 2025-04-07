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
'''This module contains the tests for the docstring_parser module.'''

import pytest

from collections import OrderedDict
from psyclone.docstring_parser import (
    ArgumentData, RaisesData, ReturnsData, DocstringData,
    create_docstring_data, gen_docstring_from_RaisesData,
    gen_docstring_from_ArgumentData,
    gen_docstring_from_ReturnsData,
    gen_docstring_from_DocstringData,
    parse_psyclone_docstring_from_object,
)
from psyclone.errors import InternalError


def test_argumentdata():
    '''Test the ArgumentData dataclass behaves as expected.'''
    adata = ArgumentData(name="name", datatype="datatype",
                         desc="desc", inline_type=True)
    assert adata.name == "name"
    assert adata.datatype == "datatype"
    assert adata.desc == "desc"
    assert adata.inline_type


def test_raisesdata():
    '''Test the RaisesData dataclass behaves as expected.'''
    rdata = RaisesData(desc="desc", exception="Error")
    assert rdata.desc == "desc"
    assert rdata.exception == "Error"


def test_returnsdata():
    '''Test the ReturnsData dataclass behaves as expected.'''
    rdata = ReturnsData(desc="desc", datatype="datatype", inline_type=False)
    assert rdata.desc == "desc"
    assert rdata.datatype == "datatype"
    assert not rdata.inline_type


def test_docstringdata_base():
    '''Test the DocstringData dataclass behaves as expected.'''
    arguments = OrderedDict()
    returns = ReturnsData(desc="desc", datatype="datatype", inline_type=False)
    raises = []

    docdata = DocstringData(desc="desc", arguments=arguments, raises=raises,
                            returns=returns)
    assert docdata.desc == "desc"
    assert docdata.arguments is arguments
    assert docdata.raises is raises
    assert docdata.returns is returns


def test_docstringdata_add_data():
    'Test the add_data function of the DocstringData dataclass.'''
    docdata = DocstringData(desc=None, arguments=OrderedDict(), raises=[],
                            returns=None)

    docdata.add_data("desc")
    assert docdata.desc == "desc"

    adata = ArgumentData(name="name", datatype="datatype",
                         desc="desc", inline_type=True)
    docdata.add_data(adata)
    assert docdata.arguments["name"] is adata

    adata = ArgumentData(name="name2", datatype="datatype",
                         desc="desc", inline_type=True)
    docdata.add_data(adata)
    assert docdata.arguments["name2"] is adata

    rdata = ReturnsData(desc="desc", datatype="datatype", inline_type=False)
    docdata.add_data(rdata)
    assert docdata.returns is rdata

    rdata = RaisesData(desc="desc", exception="Error")
    docdata.add_data(rdata)
    assert docdata.raises[0] is rdata
    rdata = RaisesData(desc="desc2", exception="Error")
    docdata.add_data(rdata)
    assert docdata.raises[1] is rdata

    with pytest.raises(InternalError) as excinfo:
        docdata.add_data(1)
    assert ("Found docstring element not supported by PSyclone, expected"
            " :raise, :param, :returns, :type, or :rtype but found "
            "'1'." in str(excinfo.value))


def test_docstringdata_merge():
    '''Test the merge function of the DocstringData dataclass.'''
    docdata = DocstringData(desc=None, arguments=OrderedDict(), raises=[],
                            returns=None)

    docdata2 = DocstringData(desc=None, arguments=OrderedDict(), raises=[],
                             returns=None)
    adata = ArgumentData(name="name", datatype="datatype",
                         desc="desc", inline_type=True)
    docdata2.add_data(adata)
    adata2 = ArgumentData(name="name2", datatype="datatype",
                          desc="desc", inline_type=True)
    docdata2.add_data(adata2)

    docdata.merge(docdata2)

    assert docdata.arguments["name"] is adata
    assert docdata.arguments["name2"] is adata2

    # Check we don't overwrite arguments without replace set.
    docdata3 = DocstringData(desc=None, arguments=OrderedDict(), raises=[],
                             returns=None)
    adata3 = ArgumentData(name="name", datatype="datatype",
                          desc="desc", inline_type=True)
    docdata3.add_data(adata3)
    docdata.merge(docdata3)
    assert docdata.arguments["name"] is not adata3
    docdata.merge(docdata3, replace_args=True)
    assert docdata.arguments["name"] is adata3

    # Merge a description.
    docdata4 = DocstringData(desc="desc", arguments=OrderedDict(), raises=[],
                             returns=None)
    docdata.merge(docdata4)
    assert docdata.desc == "desc"

    # Don't overwrite without param
    docdata5 = DocstringData(desc="desc2", arguments=OrderedDict(), raises=[],
                             returns=None)
    docdata.merge(docdata5)
    assert docdata.desc == "desc"
    docdata.merge(docdata5, replace_desc=True)
    assert docdata.desc == "desc2"

    # Merge raises
    docdata6 = DocstringData(desc="desc", arguments=OrderedDict(), raises=[],
                             returns=None)
    rdata = RaisesData(desc="desc2", exception="Error")
    docdata6.add_data(rdata)
    docdata.merge(docdata6)
    assert docdata.raises[0] is rdata

    # Merge returns
    docdata7 = DocstringData(desc="desc", arguments=OrderedDict(), raises=[],
                             returns=None)
    rdata = ReturnsData(desc="desc", datatype="datatype", inline_type=False)
    docdata7.add_data(rdata)
    docdata.merge(docdata7)
    assert docdata.returns is rdata

    # Don't overwrite without param
    docdata8 = DocstringData(desc="desc", arguments=OrderedDict(), raises=[],
                             returns=None)
    rdata2 = ReturnsData(desc="desc2", datatype="datatype", inline_type=False)
    docdata8.add_data(rdata2)
    docdata.merge(docdata8)
    assert docdata.returns is not rdata2
    docdata.merge(docdata8, replace_returns=True)
    assert docdata.returns is rdata2


def dummy_function(typed_arg: int, untyped_arg):
    '''Dummy function for testing functionality.'''


def test_create_docstring_data():
    '''Test the create_docstring_data function.'''
    # Test :param typed_arg: with a declared type in the function input.
    args = ["param", "typed_arg"]
    output = create_docstring_data(args, "desc", dummy_function)
    assert isinstance(output, ArgumentData)
    assert output.datatype == "int"
    assert output.inline_type
    assert output.name == "typed_arg"
    assert output.desc == "desc"

    # Test :param untyped_arg: with no declared type in the function input.
    # This is like the case where the docstring would have a :type...: line
    args = ["param", "untyped_arg"]
    output = create_docstring_data(args, "desc", dummy_function)
    assert isinstance(output, ArgumentData)
    assert output.datatype is None
    assert not output.inline_type
    assert output.name == "untyped_arg"
    assert output.desc == "desc"

    # Test :param type argument:
    args = ["param", "str", "arg"]
    output = create_docstring_data(args, "desc", dummy_function)
    assert isinstance(output, ArgumentData)
    assert output.datatype == "str"
    assert output.inline_type
    assert output.name == "arg"
    assert output.desc == "desc"

    # Test invalid param string
    args = ["param", "one", "two", "three"]
    with pytest.raises(InternalError) as excinfo:
        create_docstring_data(args, "desc", dummy_function)
    assert ("Found parameter docstring of unsupported type, expected "
            ":param arg: or :param type arg: but found "
            ":param one two three:" in str(excinfo.value))

    # Test invalid raises string
    args = ["raises", "one", "two"]
    with pytest.raises(InternalError) as excinfo:
        create_docstring_data(args, "desc", dummy_function)
    assert ("Found raises docstring of unsupported type, expected "
            ":raises Error: but found :raises one two:" in str(excinfo.value))

    # Test valid raises string
    args = ["raises", "InternalError"]
    output = create_docstring_data(args, "desc", dummy_function)
    assert isinstance(output, RaisesData)
    assert output.exception == "InternalError"
    assert output.desc == "desc"

    # Test invalid returns string
    args = ["returns", "one", "two"]
    with pytest.raises(InternalError) as excinfo:
        create_docstring_data(args, "desc", dummy_function)
    assert ("Found return docstring of unsupported type, expected "
            ":returns: or :returns type: but found :returns one two"
            in str(excinfo.value))

    # Test valid returns string without type info.
    args = ["returns"]
    output = create_docstring_data(args, "desc", dummy_function)
    assert isinstance(output, ReturnsData)
    assert not output.inline_type
    assert output.datatype is None
    assert output.desc == "desc"

    # Test valid returns string with type info.
    args = ["returns", "str"]
    output = create_docstring_data(args, "desc", dummy_function)
    assert isinstance(output, ReturnsData)
    assert output.inline_type
    assert output.datatype == "str"
    assert output.desc == "desc"

    # Test invalid input.
    args = ["invalid", "input"]
    with pytest.raises(InternalError) as excinfo:
        create_docstring_data(args, "desc", dummy_function)
    assert ("Found unsupported docstring: :invalid input:"
            in str(excinfo.value))


def test_gen_docstring_from_RaisesData():
    '''Test the gen_docstring_from_RaisesData function.'''
    rdata = RaisesData(desc="desc", exception="Error")
    output = gen_docstring_from_RaisesData(rdata)
    assert output == ":raises Error: desc"


def test_gen_docstring_from_ArgumentData():
    '''Test the gen_docstring_from_ArgumentData function.'''
    adata = ArgumentData(name="name", datatype="datatype",
                         desc="desc", inline_type=True)
    output = gen_docstring_from_ArgumentData(adata)
    assert output == ":param datatype name: desc"

    adata = ArgumentData(name="name", datatype="datatype",
                         desc="desc", inline_type=False)
    output = gen_docstring_from_ArgumentData(adata)
    assert output == ":param name: desc\n:type name: datatype"
    # Test looking in dummy_functions argument list and not finding the
    # argument.
    output = gen_docstring_from_ArgumentData(adata, dummy_function)
    assert output == ":param name: desc\n:type name: datatype"

    adata = ArgumentData(name="typed_arg", datatype="int",
                         desc="desc", inline_type=False)
    output = gen_docstring_from_ArgumentData(adata, dummy_function)
    assert output == ":param typed_arg: desc"


def test_gen_docstring_from_ReturnsData():
    '''Test the gen_docstring_from_ReturnsData function.'''
    rdata = ReturnsData(desc="desc", datatype="datatype", inline_type=False)
    output = gen_docstring_from_ReturnsData(rdata)
    assert output == ":returns: desc\n:rtype: datatype"

    rdata = ReturnsData(desc="desc", datatype="datatype", inline_type=True)
    output = gen_docstring_from_ReturnsData(rdata)
    assert output == ":returns datatype: desc"


def test_gen_docstring_from_DocstringData():
    ''' Test the gen_docstring_from_DocstringData function.'''

    # Check we get nothing for an empty DocstringData
    docdata = DocstringData(desc=None, arguments=OrderedDict(), raises=[],
                            returns=None)
    output = gen_docstring_from_DocstringData(docdata)
    assert output == ""

    docdata.desc = "desc"
    output = gen_docstring_from_DocstringData(docdata)
    assert output == "    desc\n"

    adata = ArgumentData(name="name", datatype="datatype",
                         desc="desc", inline_type=True)
    docdata.add_data(adata)

    adata = ArgumentData(name="name2", datatype="datatype",
                         desc="desc", inline_type=False)
    docdata.add_data(adata)

    output = gen_docstring_from_DocstringData(docdata)
    correct = '''    desc
    :param datatype name: desc
    :param name2: desc
    :type name2: datatype
'''
    assert output == correct

    rdata = RaisesData(desc="desc", exception="Error")
    docdata.add_data(rdata)
    rdata = RaisesData(desc="desc2", exception="Error2")
    docdata.add_data(rdata)
    output = gen_docstring_from_DocstringData(docdata)
    correct = '''    desc
    :param datatype name: desc
    :param name2: desc
    :type name2: datatype

    :raises Error: desc
    :raises Error2: desc2
'''
    assert output == correct

    rdata = ReturnsData(desc="desc", datatype="datatype", inline_type=False)
    docdata.add_data(rdata)
    output = gen_docstring_from_DocstringData(docdata)
    correct = '''    desc
    :param datatype name: desc
    :param name2: desc
    :type name2: datatype

    :raises Error: desc
    :raises Error2: desc2
    :returns: desc
    :rtype: datatype
'''
    assert output == correct

    rdata = ReturnsData(desc="desc", datatype="datatype", inline_type=True)
    docdata.add_data(rdata)
    output = gen_docstring_from_DocstringData(docdata)
    correct = '''    desc
    :param datatype name: desc
    :param name2: desc
    :type name2: datatype

    :raises Error: desc
    :raises Error2: desc2
    :returns datatype: desc
'''
    assert output == correct


def test_parse_psyclone_docstring_from_object():
    ''' Test the parse_psyclone_docstring_from_object call. '''

    # Method with no docstring should return None.
    def no_docs():
        pass

    # To avoid coverage complaint.
    no_docs()

    assert parse_psyclone_docstring_from_object(no_docs) is None

    # Method with only a description should return a DocstringData with
    # only a description
    def desc_only():
        '''A description and nothing else'''

    out_data = parse_psyclone_docstring_from_object(desc_only)
    assert out_data.desc == "A description and nothing else"
    assert len(out_data.arguments.keys()) == 0
    assert len(out_data.raises) == 0
    assert out_data.returns is None

    # Test the full docstring creation.
    def docstring(myparam):
        '''The description

        :param myparam: a parameter.
        :type myparam: type
        :param bool myparam2: a second parameter
        with a multiline docstring.
        :param bool myparam3: a third parameter with\
                a backslash docstring.

        :raises InternalError: an error

        :returns: something
        :rtype: type
        '''

    out_data = parse_psyclone_docstring_from_object(docstring)
    assert out_data.desc == "The description\n\n"
    assert len(out_data.arguments.keys()) == 3
    assert out_data.arguments["myparam"].name == "myparam"
    assert out_data.arguments["myparam"].desc == "a parameter."
    assert out_data.arguments["myparam"].datatype == "type"
    assert not out_data.arguments["myparam"].inline_type
    assert (out_data.arguments["myparam2"].desc == "a second parameter\n"
            "with a multiline docstring.")
    assert (out_data.arguments["myparam3"].desc == "a third parameter with\n"
            "a backslash docstring.")
    assert len(out_data.raises) == 1
    assert out_data.raises[0].exception == "InternalError"
    assert out_data.raises[0].desc == "an error"
    assert out_data.returns.desc == "something"
    assert out_data.returns.datatype == "type"
    assert not out_data.returns.inline_type

    # Test the ValueError.

    def valueerror_docstring():
        ''' Some description with only one colon after.

        :nothing here '''

    with pytest.raises(ValueError) as excinfo:
        out_data = parse_psyclone_docstring_from_object(valueerror_docstring)
    assert ('Error parsing meta information near ":nothing here "' in
            str(excinfo.value))

    def internalerror_docstring():
        ''' Some description

        :type param: uhoh
        '''

    with pytest.raises(InternalError) as excinfo:
        out_data = parse_psyclone_docstring_from_object(
            internalerror_docstring
        )
    assert ("Found a type string with no corresponding parameter: "
            "'param' type found with no parameter docstring."
            in str(excinfo.value))


def test_docstring_is_reversible():
    '''Test that the outputs from the DocstringParser are reversible, i.e.
    that updating the __doc__ with the output from the docstring geneator
    function still creates the correct DocData objects.'''

    def docstring_object(param1, param2, param3, **kwargs):
        '''
        This is my docstring, its very long and we need to check that we can
        correctly generate multiline docstring reversibly without causing
        any issues.

        :param param1: param1
        :type param1: param1type
        :param param2: param2
        :type param2: param2type
        :param param3: param2
        :type param3: param2type

        :raises Error: when bad things happen.

        :returns int: if things go well.
        '''

    def docstring_object2(param4):
        '''
        Merging docstring object.

        :param param4: param4
        :type param4: param4type
        '''

    basedata = parse_psyclone_docstring_from_object(docstring_object)
    # Functionality of these is tested in other tests, this is a sanity check.
    assert len(basedata.arguments) == 3
    assert len(basedata.raises) == 1
    assert basedata.returns is not None
    assert basedata.desc is not None

    # Create a processed docstring
    processed_doc = gen_docstring_from_DocstringData(basedata)

    # Generate a new basedata from the generated docstring.
    docstring_object.__doc__ = processed_doc
    basedata2 = parse_psyclone_docstring_from_object(docstring_object)
    assert len(basedata2.arguments) == 3
    assert list(basedata2.arguments.keys())[0] == "param1"
    assert list(basedata2.arguments.keys())[1] == "param2"
    assert list(basedata2.arguments.keys())[2] == "param3"
    assert len(basedata2.raises) == 1
    assert basedata2.returns is not None
    assert basedata2.desc is not None

    # Get the docstring object 2 docdata
    doc2data = parse_psyclone_docstring_from_object(docstring_object2)
   
    # Merge it into basedata
    basedata.merge(doc2data)
    assert "param4" in basedata.arguments.keys()

    processed_doc = gen_docstring_from_DocstringData(basedata)

    # Generate a new basedata from the meregd docstring
    docstring_object.__doc__ = processed_doc
    basedata3 = parse_psyclone_docstring_from_object(docstring_object)
    assert len(basedata3.arguments) == 4
    assert list(basedata3.arguments.keys())[0] == "param1"
    assert list(basedata3.arguments.keys())[1] == "param2"
    assert list(basedata3.arguments.keys())[2] == "param3"
    assert list(basedata3.arguments.keys())[3] == "param4"
    assert len(basedata3.raises) == 1
    assert basedata3.returns is not None
    assert basedata3.desc is not None
