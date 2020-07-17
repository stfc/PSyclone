#!/usr/bin/env python

from jinja2 import Environment, FileSystemLoader

import sys

if len(sys.argv)!=2:
    print("Usage: process.py input-template")
    sys.exit(-1)

with open(sys.argv[1], "r") as file:
    input = "".join(file.readlines())

env = Environment()
template = env.from_string(input)
print(template.render())