# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2026, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''This module implements tests for the generic utility functions.'''

import inspect
import pytest
import sys

from typing import Union

from psyclone.errors import InternalError
from psyclone.transformations import Transformation
from psyclone.utils import (
    within_virtual_env, a_or_an,
    transformation_documentation_wrapper,
    stringify_annotation,
)


def test_within_virtual_env(monkeypatch):
    '''Test the 'within_virtual_env' function. We need to monkeypatch as
    there is no way to control what environment we are in.

    '''
    # There is a 'real_prefix' attribute.
    monkeypatch.setattr(sys, 'real_prefix', "real_prefix", raising=False)
    assert within_virtual_env()
    # There is no 'real_prefix' attribute. There is a 'base_prefix'
    # attribute with a different name to the 'prefix' attribute.
    monkeypatch.delattr(sys, 'real_prefix')
    monkeypatch.setattr(sys, 'base_prefix', "base_prefix", raising=False)
    monkeypatch.setattr(sys, 'prefix', "prefix")
    assert within_virtual_env()
    # There is no 'real_prefix' attribute. There is a 'base_prefix'
    # atribute with the same name to the 'prefix' attribute.
    monkeypatch.setattr(sys, 'base_prefix', "prefix")
    assert not within_virtual_env()
    # There are no 'real_prefix' or 'base_prefix' attributes.
    monkeypatch.delattr(sys, 'base_prefix')
    assert not within_virtual_env()


def test_a_or_an():
    '''Test the a_or_an() function.'''
    assert a_or_an("Aardvark") == "an"
    assert a_or_an("aardvark") == "an"
    assert a_or_an("honor") == "an"
    assert a_or_an("real") == "a"
    assert a_or_an("integer") == "an"
    assert a_or_an("logical") == "a"
    # Test remaining vowels.
    for vowel in ["e", "o", "u"]:
        assert a_or_an(vowel) == "an"


def test_transformation_doc_wrapper_non_transformation():
    '''Test the transformation_doc_wrapper throws an error if provided
    a non-Transformation class.'''
    with pytest.raises(InternalError) as excinfo:
        transformation_documentation_wrapper(int)

    assert ("transformation_documentation_wrapper expected a Transformation "
            "but got 'int'" in str(excinfo.value))


def test_transformation_doc_wrapper_single_inheritence():
    '''Test the transformation_doc_wrapper.'''

    # Create a base transformation class
    class BaseTrans(Transformation):

        def validate(self, node, opt1, opt2, **kwargs):
            '''
            Super validate docstring
            '''

        def apply(self, node, opt1: bool = False, opt2=None, **kwargs):
            '''
            Super apply docstring

            :param opt1: opt1 docstring.
            :param opt2: opt2 docstring.
            :type opt2: opt2 type.
            '''

    class InheritingTrans(BaseTrans):

        def validate(self, node, opt3, **kwargs):
            '''
            Sub validate docstring
            '''

        def apply(self, node, opt3: int = 1, **kwargs):
            '''
            Sub apply docstring

            :param opt3: opt3 docstring.
            '''

    assert "opt2" not in BaseTrans.validate.__doc__

    transformation_documentation_wrapper(BaseTrans, inherit=False)

    assert ":param bool opt1: opt1 docstring." in BaseTrans.validate.__doc__
    assert ":param opt2: opt2 docstring." in BaseTrans.validate.__doc__
    assert ":type opt2: opt2 type." in BaseTrans.validate.__doc__

    assert "opt2" not in InheritingTrans.apply.__doc__
    assert "opt3" not in InheritingTrans.validate.__doc__
    transformation_documentation_wrapper(InheritingTrans, inherit=False)

    assert (":param int opt3: opt3 docstring." in
            InheritingTrans.validate.__doc__)

    transformation_documentation_wrapper(InheritingTrans, inherit=True)

    assert (":param bool opt1: opt1 docstring." in
            InheritingTrans.validate.__doc__)
    assert ":param opt2: opt2 docstring." in InheritingTrans.validate.__doc__
    assert ":type opt2: opt2 type." in InheritingTrans.validate.__doc__
    assert ":param bool opt1: opt1 docstring." in InheritingTrans.apply.__doc__
    assert ":param opt2: opt2 docstring." in InheritingTrans.apply.__doc__
    assert ":type opt2: opt2 type." in InheritingTrans.apply.__doc__

    assert "Super apply docstring" not in InheritingTrans.apply.__doc__


def test_transformation_doc_wrapper_multi_inheritence():
    '''Test the transformation_doc_wrapper.'''

    # Create a base transformation class
    class BaseTrans1(Transformation):

        def validate(self, node, opt1, **kwargs):
            '''
            Super validate docstring
            '''

        def apply(self, node, opt1: bool = False, **kwargs):
            '''
            Super apply docstring

            :param opt1: opt1 docstring.
            '''

    # Create a base transformation class
    class BaseTrans2(Transformation):

        def validate(self, node, opt2, **kwargs):
            '''
            Super validate docstring
            '''

        def apply(self, node, opt2: bool = False, **kwargs):
            '''
            Super apply docstring

            :param bool opt2: opt2 docstring.
            '''

    class InheritingTrans(BaseTrans1, BaseTrans2):

        def validate(self, node, opt3, **kwargs):
            '''
            Sub validate docstring
            '''

        def apply(self, node, opt3: int = 1, **kwargs):
            '''
            Sub apply docstring

            :param opt3: opt3 docstring.
            '''

    transformation_documentation_wrapper(
        InheritingTrans,
        inherit=[BaseTrans1, BaseTrans2]
    )
    print(InheritingTrans.apply.__doc__)
    assert "param bool opt1: opt1 docstring." in InheritingTrans.apply.__doc__
    assert "param bool opt2: opt2 docstring." in InheritingTrans.apply.__doc__
    assert ("param bool opt1: opt1 docstring."
            in InheritingTrans.validate.__doc__)
    assert ("param bool opt2: opt2 docstring." in
            InheritingTrans.validate.__doc__)


def test_transformation_doc_wrapper_no_docstring():
    '''
    Test the transformation doc wrapper doesn't break when there are no
    docstrings or arguments.
    '''

    # No docstrings so need pass and coverage of pass...
    class BaseTrans(Transformation):

        def apply(self, node, **kwargs):
            pass

        def validate(self, node):
            pass

    class InheritingTrans(BaseTrans):

        def validate(self, node):
            pass

        def apply(self, node, **kwargs):
            pass

    transformation_documentation_wrapper(
            InheritingTrans,
            inherit=True
    )

    # Need coverage for BaseTrans
    instance = BaseTrans()
    instance.validate(None)
    instance.apply(None)

    # Need coverage for InheritingTrans
    instance = InheritingTrans()
    instance.validate(None)
    instance.apply(None)


def test_transformation_doc_wrapper_uninheritable():
    '''Test the transformation doc wrapper doesn't inherit parameters'
    docstrings that are defined in the _uninheritable_args list.'''

    # Create a base transformation class
    class BaseTrans(Transformation):

        def validate(self, node, opt1, opt2, **kwargs):
            '''
            Super validate docstring
            '''

        def apply(self, node, opt1: bool = False, opt2=None,
                  options=None, **kwargs):
            '''
            Super apply docstring

            :param opt1: opt1 docstring.
            :param opt2: opt2 docstring.
            :type opt2: opt2 type.
            :param options: options dictionary.
            :type options: dict
            '''

    # Create a second base transformation class to test multiple
    # inheritance.
    class BaseTrans2(Transformation):

        def validate(self, node, opt2, **kwargs):
            '''
            Super validate docstring
            '''

        def apply(self, node, opt2: bool = False, options=None, **kwargs):
            '''
            Super apply docstring

            :param bool opt2: opt2 docstring.
            :param options: options dictionary.
            :type options: dict
            '''

    class InheritingTrans1(BaseTrans):

        def validate(self, node, opt3, **kwargs):
            '''
            Sub validate docstring
            '''

        def apply(self, node, opt3: int = 1, **kwargs):
            '''
            Sub apply docstring

            :param opt3: opt3 docstring.
            '''

    # Test that the options parameter docstring is not inherited from the
    # superclass.
    transformation_documentation_wrapper(InheritingTrans1, inherit=True)
    assert ":param options:" not in InheritingTrans1.apply.__doc__

    # Class to test multiple inheritance behaviour.
    class InheritingTrans2(BaseTrans, BaseTrans2):

        def validate(self, node, opt3, **kwargs):
            '''
            Sub validate docstring
            '''

        def apply(self, node, opt3: int = 1, **kwargs):
            '''
            Sub apply docstring

            :param opt3: opt3 docstring.
            '''

    # Test that the options parameter docstring is not inherited from either
    # of the superclasses.
    transformation_documentation_wrapper(
        InheritingTrans2,
        inherit=[BaseTrans, BaseTrans2]
    )
    assert ":param options:" not in InheritingTrans2.apply.__doc__


def test_stringify_annotation():
    '''Test the stringify_annotation method does as expected.'''
    def func(temp: bool, temp2: Union[bool, int]):
        ''' Test function for annotations.'''

    signature = inspect.signature(func)
    for k, v in signature.parameters.items():
        # For first parameter temp
        if "temp" == k:
            anno = stringify_annotation(v.annotation)
            assert "<class 'bool'>" == anno

        # For second parameter temp2
        if "temp2" == k:
            anno = stringify_annotation(v.annotation)
            # Python >= 3.14 uses the second format
            assert "typing.Union[bool, int]" == anno or "bool | int" == anno
