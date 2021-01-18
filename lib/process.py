#!/usr/bin/env python

# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''This module reads a jinja template file, processes it
and renders the result to stdout. It provides the three variables
ALL_TYPES, DIMENSIONS, and PREFIX to the template, based on
command line parameters. Typically, these options are then used
by the template to provide functions for each scalar type listed in
ALL_TYPES, and for arrays with any number of dimension specified
in DIMENSIONS and each type listed in ALL_TYPES. PREFIX can be
used to add a prefix to static functions defined in the template.

'''
from __future__ import print_function
import argparse
import sys
from jinja2 import Environment

# Pylint does not like lower case names here, so disable it globally
# pylint: disable=invalid-name

parser = argparse.ArgumentParser(
    description='Process a jinja template for PSyData.')
parser.add_argument('template_name',
                    help="Name of the template file to process.")
parser.add_argument('-types', help="Comma-separated list of types, "
                                   "e.g. real,int,double (no spaces).",
                    default="real,int,double")
parser.add_argument("-dims", help="Comma-separated list of dimensions, "
                                  "e.g. 1,2,4 (no spaces)",
                    default="1,2,3,4")

parser.add_argument("-prefix", help="Prefix to add to the generated PSyData "
                                    "function names",
                    default="")
# A certain implementation of a generic subroutine can only be
# specified once to be generic. So if a derived class wants to overwrite
# say `ProvideScalarInt`, this subroutine must be added to the generic
# subroutine in the derived class, not in the base class. On the
# other hand, many derived classes will not overwrite many (if any)
# functions in the base class, so in this case it is convenient to declare
# these functions in the base class. This can often avoid the need to use
# Jinja for a derived class. For example, a GOcean library will only
# implement functions for the the GOCean-specific field type, and rely on
# the base class to provide the implementations for all standard Fortran
# types).
# In order to support this, the `process.py` sript provides two options
# to control the creation of the `DeclareXXX` and `ProvideXXX` generic
# interfaces, which control if a base class specifies that the `declareXXX`
# and `provideXXX` functions are part of the generic interface or not.
# As example, if a derived class relies on a Jinja base class to provide
# implementations for the `declareXXX` functions, it should process the
# Jinja template with the option `-generic-declare`. This will then
# add the generic interface for all `DeclareXXX` functions.
# The process script will pass the options as GENERIC_DECLARE and
# GENERIC_PROVIDE variables to the Jinja template.

parser.add_argument("-generic-declare", action="store_true",
                    help="Declare generic interfaces for "
                    "PreDeclareVariable functions.", default=False)
parser.add_argument("-generic-provide", action="store_true",
                    help="Declare generic interfaces for "
                    "ProvideVariable functions.", default=False)

args = parser.parse_args()

# ---------------------------------------------------------
# This is a mapping from the command line option name
# to the tuple that is required for the jinja templates:
TYPE_DATA = {"real": ("Real", "real(kind=real32)", 32),
             "double": ("Double", "real(kind=real64)", 64),
             "int": ("Int", "integer(kind=int32)", 32),
             "long": ("Long", "integer(kind=int64)", 64)}

# ---------------------------------------------------------
# Check type information:
types = [type.lower() for type in args.types.split(",")]

# If types is empty (e.g. in profiling no types are actually
# required), the parsers assigns ['']. Convert to an empty list:
if types == ['']:
    types = []

for my_type in types:
    if my_type not in TYPE_DATA:
        print("Type '{0}' is not supported.".format(my_type), file=sys.stderr)
        print("Use one or more of {0}"
              .format(",".join(list(TYPE_DATA.keys()))), file=sys.stderr)
        sys.exit(-1)
all_types = [TYPE_DATA[my_type] for my_type in types]

# ---------------------------------------------------------
# check dimension
dims = args.dims.split(",")

# Convert to empty list if an empty dims argument was given:
if dims == ['']:
    dims = []

for dim in dims:
    try:
        int_dim = int(dim)
    except ValueError:
        print("Dimension value '{0}' is not valid.".format(dim),
              file=sys.stderr)
        sys.exit(-1)
    if int_dim < 1 or int_dim > 7:
        print("Dimension value '{0}' is not between 1 and 7.".format(dim),
              file=sys.stderr)
        sys.exit(-1)

dims = [int(dim) for dim in dims]
# ---------------------------------------------------------
with open(args.template_name, "r") as file:
    template_string = "".join(file.readlines())

env = Environment(trim_blocks=True, lstrip_blocks=True)
template = env.from_string(template_string)

print(template.render(ALL_TYPES=all_types, ALL_DIMS=dims,
                      PREFIX=args.prefix,
                      GENERIC_DECLARE=args.generic_declare,
                      GENERIC_PROVIDE=args.generic_provide))
