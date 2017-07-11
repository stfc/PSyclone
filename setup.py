#!/usr/bin/env python

# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
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

"""Setup script. Used by easy_install and pip."""

from setuptools import setup, find_packages

PACKAGES = find_packages(where="src")

NAME = 'PSyclone'
AUTHOR = ("Rupert Ford <rupert.ford@stfc.ac.uk>, "
          "Andrew Porter <andrew.porter@stfc.ac.uk>")
AUTHOR_EMAIL = 'rupert.ford@stfc.ac.uk'
URL = 'https://github.com/stfc/psyclone'
DOWNLOAD_URL = 'https://github.com/stfc/psyclone'
DESCRIPTION = 'PSyclone - a compiler for Finite Element/Volume/Difference DSLs in Fortran'
LONG_DESCRIPTION = '''\
PSyclone is a compiler for Fortran-embedded Domain Specific Languages
targetting Finite Element/Volume/Difference methods in earth-system
modelling.

See https://github.com/stfc/psyclone for more information.
'''
LICENSE='OSI Approved :: BSD 3-Clause License'

CLASSIFIERS = [
    'Development Status :: 3 - Alpha',
    'Environment :: Console',
    'Intended Audience :: Developers',
    'Intended Audience :: Science/Research',
    'Natural Language :: English',
    'Programming Language :: Fortran',
    'Programming Language :: Python',
    'Topic :: Scientific/Engineering',
    'Topic :: Software Development',
    'Topic :: Utilities',
    'Operating System :: POSIX',
    'Operating System :: Unix',
    'Operating System :: MacOS']

MAJOR = 1
MINOR = 4
MICRO = 1
SHORT_VERSION = "{0:d}.{1:d}".format(MAJOR, MINOR)
VERSION = "{0:d}.{1:d}.{2:d}".format(MAJOR, MINOR, MICRO)


def write_version_py(filename=None):
    ''' Write a python module containing the current version
    of PSyclone. This is used when generating documentation. '''
    import os
    if not filename:
        filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "src", "psyclone", "version.py")
    content = '''
# This file is GENERATED from PSyclone setup.py
short_version="{short_version:s}"
version="{version:s}"
'''
    vfile = open(filename, 'w')
    try:
        vfile.write(content.format(short_version=SHORT_VERSION,
                                   version=VERSION))
    finally:
        vfile.close()


if __name__ == '__main__':

    # Rewrite the version file everytime
    import os
    if os.path.exists('src/psyclone/version.py'):
        os.remove('src/psyclone/version.py')
    write_version_py()

    setup(
        name=NAME,
        version=VERSION,
        author=AUTHOR,
        author_email=(AUTHOR_EMAIL),
        license=LICENSE,
        url=URL,
        description=DESCRIPTION,
        long_description=LONG_DESCRIPTION,
        classifiers=CLASSIFIERS,
        packages=PACKAGES,
        package_dir={"": "src"},
        install_requires=['pyparsing', 'fparser'],
        include_package_data=True,
        scripts=['bin/psyclone', 'bin/genkernelstub'])
