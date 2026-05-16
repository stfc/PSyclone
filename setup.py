#!/usr/bin/env python

# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#          I. Kavcic and P. Elson, Met Office
#          J. Henrichs, Bureau of Meteorology

"""
Metadata and most configuration are now in pyproject.toml.
This script is retained for the dynamic calculation of data_files.
"""

import os
from setuptools import setup

BASE_PATH = os.path.dirname(os.path.abspath(__file__))

if __name__ == '__main__':

    def get_files(
        directory: str, install_path: str, valid_suffixes: list[str]
    ) -> list[tuple[str,list[str]]]:
        '''Utility routine that creates a list of 2-tuples, each consisting of
        the target installation directory and a list of files
        (specified relative to the project root directory).

        :param directory: the directory containing the required files.
        :param install_path: the location where the files will be placed.
        :param valid_suffixes: the suffixes of the required files.

        :returns: a list of 2-tuples, each consisting of the target
            installation directory and a list of files (specified relative
            to the project root directory).

        '''
        examples = []
        for dirpath, _, filenames in os.walk(directory):
            if ("__" not in dirpath) and filenames:
                rel_path = os.path.relpath(dirpath, directory)
                files = []
                for filename in filenames:
                    if any(filename.endswith(suffix) for suffix in
                           valid_suffixes):
                        files.append(
                            os.path.join(os.path.basename(install_path),
                                         rel_path, filename))
                if files:
                    examples.append((os.path.join(install_path, rel_path),
                                     files))
        return examples

    # Since the data_files argument doesn't accept wildcards we have to
    # explicitly list every file we want.
    # INSTALL_PATH controls where the files will be installed.
    # VALID_SUFFIXES controls the type of files to include.
    EGS_DIR = os.path.join(BASE_PATH, "examples")
    INSTALL_PATH = os.path.join("share", "psyclone", "examples")
    VALID_SUFFIXES = ["90", "py", "md", ".c", ".cl", "Makefile", ".mk"]
    EXAMPLES = get_files(EGS_DIR, INSTALL_PATH, VALID_SUFFIXES)

    TUTORIAL_DIR = os.path.join(BASE_PATH, "tutorial")
    INSTALL_PATH = os.path.join("share", "psyclone", "tutorial")
    VALID_SUFFIXES = [".ipynb"]
    TUTORIAL = get_files(TUTORIAL_DIR, INSTALL_PATH, VALID_SUFFIXES)

    LIBS_DIR = os.path.join(BASE_PATH, "lib")
    INSTALL_PATH = os.path.join("share", "psyclone", "lib")
    VALID_SUFFIXES = ["90", "sh", "py", "md", "Makefile", ".mk",
                      ".jinja", "doxyfile"]
    LIBS = get_files(LIBS_DIR, INSTALL_PATH, VALID_SUFFIXES)

    setup(
        data_files=[
            ('share/psyclone',
             ['config/psyclone.cfg'])] + EXAMPLES + TUTORIAL + LIBS,
    )
