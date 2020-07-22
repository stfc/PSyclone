#!/usr/bin/env python

'''This modules reads a jinja template file, processes it
and renders the result to stdout.
'''

import argparse
import sys
from jinja2 import Environment

# Pylint does not like lower case names here, so disable it globally
# pylint: disable=invalid-name

parser = argparse.ArgumentParser(
    description='Process a jinja template for PSyData.')
parser.add_argument('template_name', help="Name of the template to process.")
parser.add_argument('-types', help="Comma-separated list of types, "
                                   "e.g. real,int,double.",
                    default="real,int,double")
parser.add_argument("-dims", help="Comma-separated list of dimensions, "
                                  "e.g. 1,2,4.",
                    default="1,2,3,4")

parser.add_argument("-prefix", help="Prefix for PSyData functions.",
                    default="")

args = parser.parse_args()

# ---------------------------------------------------------
# This is a mapping from the command line option name
# to the tuple that is required for the jinja templates:
TYPE_DATA = {"real": ("Real", "real(kind=real32)", 32),
             "double": ("Double", "real(kind=real64)", 64),
             "int": ("Int", "integer(kind=int32)", 32)}

# ---------------------------------------------------------
# Check type information:
types = [type.lower() for type in args.types.split(",")]
for my_type in types:
    if my_type not in TYPE_DATA:
        print("Type '{0}' is not supported.".format(my_type))
        print("Use one or more of {0}"
              .format(",".join(list(TYPE_DATA.keys()))))
        sys.exit(-1)
all_types = [TYPE_DATA[my_type] for my_type in types]

# ---------------------------------------------------------
# check dimension
dims = args.dims.split(",")
for dim in dims:
    try:
        int_dim = int(dim)
    except ValueError:
        print("Dimension value '{0}' is not valid.".format(dim))
        sys.exit(-1)
    if int_dim < 1 or int_dim > 7:
        print("Dimension value '{0}' is not between 1 and 7.".format(dim))
        sys.exit(-1)

dims = [int(dim) for dim in dims]
# ---------------------------------------------------------
with open(args.template_name, "r") as file:
    template_string = "".join(file.readlines())

env = Environment(trim_blocks=True, lstrip_blocks=True)
template = env.from_string(template_string)

print(template.render(ALL_TYPES=all_types, DIMENSIONS=dims,
                      PREFIX=args.prefix))
