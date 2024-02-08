# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the StructureReference class. '''

import pytest

from psyclone.core import Signature, VariablesAccessInfo
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir import symbols, nodes
from psyclone.tests.utilities import check_links


def test_struc_ref_init():
    '''Tests the constructor.'''

    sym = symbols.symbol.Symbol("test")
    s_ref = nodes.StructureReference(sym)

    assert s_ref._overwrite_datatype is None

    with pytest.raises(TypeError) as excinfo:
        _ = nodes.StructureReference("hello")
    assert ("The StructureReference symbol setter expects a PSyIR Symbol "
            "object but found 'str'." in str(excinfo.value))


def test_struc_ref_create():
    ''' Tests for the create method. '''
    region_type = symbols.StructureType.create([
        ("startx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC,
         None)])
    region_type_symbol = symbols.DataTypeSymbol("region_type", region_type)
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None),
        ("region", region_type_symbol, symbols.Symbol.Visibility.PRIVATE,
         None),
        ("sub_grids", symbols.ArrayType(region_type_symbol, [3]),
         symbols.Symbol.Visibility.PUBLIC, None),
        ("data", symbols.ArrayType(symbols.REAL_TYPE, [10, 10]),
         symbols.Symbol.Visibility.PUBLIC, None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.create(ssym, ["nx"])
    assert sref.symbol is ssym
    assert len(sref.children) == 1
    assert sref.children[0].name == "nx"
    check_links(sref, sref.children)
    # Reference to scalar member of structure member of structure
    rref = nodes.StructureReference.create(ssym, ["region", "startx"])
    assert rref.children[0].name == "region"
    assert rref.children[0].children[0].name == "startx"
    check_links(rref.children[0], rref.children[0].children)
    # Reference to an element of an array member of the structure
    aref = nodes.StructureReference.create(
        ssym,
        [("data", [nodes.Literal("1", symbols.INTEGER_TYPE),
                   nodes.Literal("5", symbols.INTEGER_TYPE)])])
    assert isinstance(aref.children[0], nodes.ArrayMember)
    assert aref.children[0].name == "data"
    assert isinstance(aref.children[0].children[1], nodes.Literal)
    assert aref.children[0].children[1].value == "5"
    check_links(aref, aref.children)
    check_links(aref.children[0], aref.children[0].children)
    # Reference to an array of structures within a structure
    structarray_ref = nodes.StructureReference.create(
        ssym, [("sub_grids", [nodes.Literal("1", symbols.INTEGER_TYPE)])])
    assert isinstance(structarray_ref.children[0], nodes.ArrayMember)
    # Reference to a scalar member of an element of an array of structures
    # contained in a structure
    dref = nodes.StructureReference.create(
        ssym,
        [("sub_grids", [nodes.Literal("2", symbols.INTEGER_TYPE)]), "startx"])
    assert isinstance(dref.children[0], nodes.ArrayOfStructuresMember)
    assert isinstance(dref.children[0].children[0], nodes.Member)
    assert isinstance(dref.children[0].children[1], nodes.Literal)
    check_links(dref, dref.children)
    check_links(dref.children[0], dref.children[0].children)


def test_struc_ref_create_errors():
    ''' Tests for the validation checks in the create method. '''
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(None, [])
    assert ("'symbol' argument to StructureReference.create() should be a "
            "DataSymbol but found 'NoneType'" in str(err.value))
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", symbols.UnresolvedType()), [],
            overwrite_datatype=1)
    assert ("The 'overwrite_datatype' argument to StructureReference.create() "
            "should be a DataType but found 'DataSymbol'." in str(err.value))
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("fake", symbols.INTEGER_TYPE), [])
    assert ("symbol that is (or could be) a structure, however symbol "
            "'fake' has type 'Scalar" in str(err.value))
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", symbols.UnresolvedType()), 1)
    assert ("'members' argument to StructureReference._create() must be a "
            "list but found 'int'" in str(err.value))
    with pytest.raises(ValueError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", symbols.UnresolvedType()), [])
    assert ("one or more structure 'members' that are being accessed but "
            "got an empty list for symbol 'grid'" in str(err.value))
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC,
         None)])
    tsymbol_known = symbols.DataTypeSymbol("grid_type", grid_type)
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", tsymbol_known), [1])
    assert ("'members' passed to StructureType._create() must consist of "
            "either 'str' or 2-tuple entries but found 'int' in the last "
            "entry while attempting to create reference to symbol 'grid'" in
            str(err.value))
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", tsymbol_known), [1, "hello"])
    assert ("'members' passed to StructureType._create() must consist of "
            "either 'str' or 2-tuple entries but found 'int' while "
            "attempting to create reference to symbol 'grid'" in
            str(err.value))


def test_struc_ref_validate_child():
    ''' Tests for the _validate_child method. '''
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC,
         None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.create(ssym, ["nx"])
    # a StructureReference is only allowed (at most) one child.
    with pytest.raises(GenerationError) as err:
        sref.addchild("wrong")
    assert ("Item 'str' can't be child 1 of 'StructureReference'" in
            str(err.value))
    # If present, the first child has to be a MemberReference
    with pytest.raises(GenerationError) as err:
        sref.children[0] = "wrong"
    assert ("Item 'str' can't be child 0 of 'StructureReference'" in
            str(err.value))


def test_struc_ref_str():
    ''' Test the __str__ method of StructureReference. '''
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.create(ssym, ["nx"])
    assert (str(sref) == "StructureReference[name:'grid']\n"
            "Member[name:'nx']")


def test_reference_accesses():
    ''' Test the reference_accesses method.
    '''
    dref = nodes.StructureReference.create(
        symbols.DataSymbol(
            "grid",
            symbols.DataTypeSymbol("grid_type", symbols.UnresolvedType())),
        ["data"])
    var_access_info = VariablesAccessInfo()
    dref.reference_accesses(var_access_info)

    assert var_access_info.all_signatures == [Signature(("grid", "data"))]
    # By default all accesses are marked as read
    assert str(var_access_info) == "grid%data: READ"


def test_struc_ref_semantic_nav():
    ''' Test the 'member' property of the StructureReference. '''
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.create(ssym, ["nx"])
    assert sref.member is sref.children[0]
    # Break the first child to check that we get the expected error
    sref._children = ["broken"]
    with pytest.raises(InternalError) as err:
        _ = sref.member
    assert ("StructureReference malformed or incomplete. It must have a "
            "single child that must be a (sub-class of) Member, but "
            "found: ['broken']" in str(err.value))


def test_struc_ref_datatype():
    '''Test the datatype() method of StructureReference.'''
    atype = symbols.ArrayType(symbols.REAL_TYPE, [10, 8])
    rtype = symbols.StructureType.create([
        ("gibber", symbols.BOOLEAN_TYPE, symbols.Symbol.Visibility.PUBLIC,
         None)])
    # TODO #1031. Currently cannot create an array of StructureTypes
    # directly - have to have a DataTypeSymbol.
    rtype_sym = symbols.DataTypeSymbol("gibber_type", rtype)
    artype = symbols.ArrayType(rtype_sym, [10, 3])
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None),
        ("data", atype, symbols.Symbol.Visibility.PRIVATE, None),
        ("roger", rtype, symbols.Symbol.Visibility.PUBLIC, None),
        ("titty", artype, symbols.Symbol.Visibility.PUBLIC, None)])
    # Symbol with type defined by StructureType
    ssym0 = symbols.DataSymbol("grid", grid_type)
    # Reference to scalar member of structure
    sref0 = nodes.StructureReference.create(ssym0, ["nx"])
    assert sref0.datatype == symbols.INTEGER_TYPE

    # Symbol with type defined by DataTypeSymbol
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.create(ssym, ["nx"])
    assert sref.datatype == symbols.INTEGER_TYPE
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    two = nodes.Literal("2", symbols.INTEGER_TYPE)
    sref2 = nodes.StructureReference.create(ssym, [("data", [one, two])])
    assert sref2.datatype == symbols.REAL_TYPE

    # Reference to scalar member of structure member
    gref = nodes.StructureReference.create(ssym, ["roger", "gibber"])
    assert gref.datatype == symbols.BOOLEAN_TYPE

    # Reference to structure member of structure
    rref = nodes.StructureReference.create(ssym, ["roger"])
    assert rref.datatype == rtype

    # Reference to single element of array of structures within a structure
    singleref = nodes.StructureReference.create(
        ssym, [("titty", [one.copy(), two.copy()])])
    assert singleref.datatype == rtype_sym

    # Reference to sub-array of structure members of structure
    myrange = nodes.Range.create(two.copy(),
                                 nodes.Literal("4", symbols.INTEGER_TYPE))
    arref = nodes.StructureReference.create(
        ssym, [("titty", [nodes.Literal("3", symbols.INTEGER_TYPE), myrange])])
    dtype = arref.datatype
    assert isinstance(dtype, symbols.ArrayType)
    assert dtype.intrinsic == rtype_sym
    assert len(dtype.shape) == 1
    assert dtype.shape[0].lower == one
    assert isinstance(dtype.shape[0].upper, nodes.BinaryOperation)

    # Reference to whole array of structures that are a member of a structure
    fullref = nodes.StructureReference.create(ssym, ["titty"])
    dtype = fullref.datatype
    assert dtype == artype

    # Check that we can enforce a certain data type for a reference:
    # nx is defined to be an integer above:
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.\
        create(ssym, ["nx"], overwrite_datatype=symbols.REAL_TYPE)
    assert sref.datatype == symbols.REAL_TYPE


def test_structure_reference_unresolved_type():
    '''
    Check that the datatype() method behaves as expected when it
    encounters members of UnresolvedType or UnsupportedType.

    '''
    atype = symbols.ArrayType(
        symbols.UnsupportedFortranType(
            "type(atype), dimension(10,8), pointer :: aptr"), [10, 8])
    grid_type = symbols.StructureType.create([
        ("mesh", symbols.UnresolvedType(), symbols.Symbol.Visibility.PUBLIC,
         None),
        ("aptr", atype, symbols.Symbol.Visibility.PUBLIC, None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Structure of UnresolvedType
    deft_sym = symbols.DataSymbol("john", symbols.UnresolvedType())
    jref = nodes.StructureReference.create(deft_sym, ["value"])
    assert jref.datatype == symbols.UnresolvedType()
    # Structure of UnsupportedType
    ut_sym = symbols.DataSymbol(
                "teasel",
                symbols.UnsupportedFortranType("some type decln"))
    tref = nodes.StructureReference.create(ut_sym, ["yard"])
    assert tref.datatype == symbols.UnresolvedType()
    # Structure with type given by DataTypeSymbol that is of UnresolvedType
    utypesym = symbols.DataTypeSymbol("my_type", symbols.UnresolvedType())
    mysym = symbols.DataSymbol("my_sym", utypesym)
    myref = nodes.StructureReference.create(mysym, ["flag"])
    assert myref.datatype == symbols.UnresolvedType()
    # Member of structure that is of deferred type
    meshref = nodes.StructureReference.create(ssym, ["mesh", "polly"])
    assert meshref.datatype == symbols.UnresolvedType()
    # Member of structure that is an array of UnsupportedType.
    two = nodes.Literal("2", symbols.INTEGER_TYPE)
    four = nodes.Literal("4", symbols.INTEGER_TYPE)
    myrange = nodes.Range.create(two.copy(), four.copy())
    aref = nodes.StructureReference.create(ssym, [("aptr",
                                                   [two.copy(), myrange])])
    assert len(aref.datatype.shape) == 1
    assert isinstance(aref.datatype, symbols.ArrayType)
    assert isinstance(aref.datatype.intrinsic, symbols.UnsupportedFortranType)
    # An array made of individual elements of a member array.
    # my_sym(:)%aptr(2,2)
    array_grid_type = symbols.ArrayType(grid_type_symbol, [four.copy()])
    array_sym = symbols.DataSymbol("thing", array_grid_type)
    aref2 = nodes.ArrayOfStructuresReference.create(
        array_sym, [myrange.copy()],
        [("aptr", [two.copy(), two.copy()])])
    assert len(aref2.datatype.shape) == 1
    assert isinstance(aref2.datatype.intrinsic, symbols.UnsupportedFortranType)
    # An array of arrays - not supported.
    # my_sym(2:4)%aptr
    aref3 = nodes.ArrayOfStructuresReference.create(
        array_sym, [myrange.copy()], ["aptr"])
    with pytest.raises(NotImplementedError) as err:
        _ = aref3.datatype
    assert "Array of arrays not supported: " in str(err.value)
