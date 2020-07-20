#!/usr/bin/env python

'''This modules reads a jinja template file, processes it
and renders the result to stdout.
'''

import sys
from jinja2 import Environment

if len(sys.argv) != 2:
    print("Usage: process.py input-template")
    sys.exit(-1)

# Pylint does not like lower case names here
# pylint: disable=invalid-name
with open(sys.argv[1], "r") as file:
    template_string = "".join(file.readlines())

env = Environment(trim_blocks=True, lstrip_blocks=True)
template = env.from_string(template_string)
print(template.render())
