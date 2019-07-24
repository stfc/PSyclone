#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter STFC Daresbury Lab
# Modified work Copyright (c) 2018 by J. Henrichs, Bureau of Meteorology

'''
    This module provides the PSyclone 'main' routine which is intended
    to be driven from the bin/psyclone executable script. 'main'
    takes an algorithm file as input and produces modified algorithm
    code and generated PSy code. A function, 'generate', is also provided
    which has the same functionality as 'main' but can be called
    from within another Python program.
'''

from __future__ import absolute_import, print_function

# Those APIs that do not have a separate Algorithm layer
API_WITHOUT_ALGORITHM = ["nemo"]


def generate(filename, api="", kernel_path="", script_name=None,
             line_length=False,
             distributed_memory=None,
             kern_out_path="",
             kern_naming="multiple"):
    '''Takes a PSyclone algorithm specification as input and outputs the
    associated generated algorithm and psy codes suitable for
    compiling with the specified kernel(s) and support
    infrastructure. Uses the :func:`parse.algorithm.parse` function to
    parse the algorithm specification, the :class:`psyGen.PSy` class
    to generate the PSy code and the :class:`alg_gen.Alg` class to
    generate the modified algorithm code.

    :param str filename: The file containing the algorithm specification.
    :param str kernel_path: The directory from which to recursively \
                            search for the files containing the kernel \
                            source (if different from the location of the \
                            algorithm specification).
    :param str script_name: A script file that can apply optimisations \
                            to the PSy layer (can be a path to a file or \
                            a filename that relies on the PYTHONPATH to \
                            find the module).
    :param bool line_length: A logical flag specifying whether we care \
                             about line lengths being longer than 132 \
                             characters. If so, the input (algorithm \
                             and kernel) code is checked to make sure \
                             that it conforms. The default is False.
    :param bool distributed_memory: A logical flag specifying whether to \
                                    generate distributed memory code. The \
                                    default is set in the config.py file.
    :param str kern_out_path: Directory to which to write transformed \
                              kernel code.
    :param bool kern_naming: the scheme to use when re-naming transformed \
                             kernels.
    :return: 2-tuple containing fparser1 ASTs for the algorithm code and \
             the psy code.
    :rtype: (:py:class:`fparser.one.block_statements.BeginSource`, \
             :py:class:`fparser.one.block_statements.Module`)

    :raises IOError: if the filename or search path do not exist
    :raises GenerationError: if an invalid API is specified.
    :raises GenerationError: if an invalid kernel-renaming scheme is specified.

    For example:

    >>> from psyclone.generator import generate
    >>> alg, psy = generate("algspec.f90")
    >>> alg, psy = generate("algspec.f90", kernel_path="src/kernels")
    >>> alg, psy = generate("algspec.f90", script_name="optimise.py")
    >>> alg, psy = generate("algspec.f90", line_length=True)
    >>> alg, psy = generate("algspec.f90", distributed_memory=False)

    '''
    pass
