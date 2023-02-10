# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''This module generates LFRic-specific PSyIR classes from lists of
definitions.

'''
# pylint: disable=unused-import
# pylint: disable=exec-used

from collections import namedtuple

from psyclone.domain.lfric import LFRicConstants
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Literal
from psyclone.psyir.symbols import (ContainerSymbol, DataSymbol, DeferredType,
                                    ImportInterface, ScalarType, ArrayType,
                                    INTEGER_TYPE)

# Define LFRic module symbols.

# The first Module namedtuple argument specifies the name of the
# module and the second argument declares the name(s) of any symbols
# declared by the module.

Module = namedtuple('Module', ["name", "vars"])
MODULES = [
    Module(LFRicConstants().UTILITIES_MOD_MAP["constants"]["module"],
           ["i_def", "r_def", "r_solver", "r_tran", "l_def"])]

# Generate LFRic module symbols from definitions
for module in MODULES:
    MODULE_NAME = module.name
    # Create the module (using a PSyIR ContainerSymbol)
    exec(f"{MODULE_NAME.upper()} = ContainerSymbol('{MODULE_NAME}')\n")

    # Create the variables specified by the module (using PSyIR DataSymbols)
    for module_var in module.vars:
        exec(f"{module_var.upper()} = DataSymbol('{module_var}', INTEGER_TYPE,"
             f" interface=ImportInterface({MODULE_NAME.upper()}))")

# Define generic LFRic scalar datatypes and symbols

# The GenericScalar namedtuple has 3 properties: the first determines
# the names of the resultant datatype and datasymbol classes, the
# second specifies the intrinsic PSyIR type and the third specifies
# the precision required by referencing symbols already declared
# above.

GenericScalar = namedtuple('GenericScalar', ["name", "intrinsic", "precision"])
GENERIC_SCALAR_DATATYPES = [
    GenericScalar("lfric integer scalar", "integer", "i_def"),
    GenericScalar("lfric real scalar", "real", "r_def"),
    GenericScalar("lfric logical scalar", "boolean", "l_def")]

# Generate generic LFRic scalar datatypes and symbols from definitions
for info in GENERIC_SCALAR_DATATYPES:
    NAME = "".join(info.name.title().split())
    INTRINSIC = info.intrinsic.upper()
    PRECISION = info.precision
    # Create the specific datatype
    exec(
        f"class {NAME}DataType(ScalarType):\n"
        f"    def __init__(self, precision=None):\n"
        f"        if not precision:\n"
        f"            precision = {PRECISION.upper()}\n"
        f"        super({NAME}DataType, self).__init__(\n"
        f"            ScalarType.Intrinsic.{INTRINSIC}, precision)\n")
    # Create the specific symbol
    exec(
        f"class {NAME}DataSymbol(DataSymbol):\n"
        f"    def __init__(self, name, precision=None, **kwargs):\n"
        f"        super().__init__(\n"
        f"            name, {NAME}DataType(precision=precision),\n"
        f"            **kwargs)\n")

__all__ = []
