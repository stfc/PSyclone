# Modified work Copyright (c) 2017-2019 Science and Technology
# Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

__all__ = ["set_read_options", "set_parse_options", "get_fortran_code_group"]
from optparse import OptionGroup, NO_DEFAULT


def set_read_options(parser):
    parser.set_usage(
        """\
%prog [options] <Fortran files>

Description:
  %prog reads Fortran codes."""
    )
    parser.add_option(
        "--task",
        default="show",
        choices=["show"],
        help="Specify reading task. Default: %default.",
    )
    parser.add_option_group(get_fortran_code_group(parser))


def set_parse_options(parser):
    parser.set_usage(
        """\
%prog [options] <Fortran files>

Description:
  %prog parses Fortran codes."""
    )
    parser.add_option(
        "--task",
        default="show",
        choices=["show", "none"],
        help="Specify parsing result task. Default: %default.",
    )
    parser.add_option_group(get_fortran_code_group(parser))


def set_fparser_options(parser):
    """Command line options used by the fparser2 script.

    :param parser: The OptionParser object.
    :type parser: :py:class:`optparse.OptionParser`

    """

    parser.set_usage(
        """\
%prog [options] <Fortran files>

Description:
  %prog parses Fortran code."""
    )
    parser.add_option(
        "--task",
        default="show",
        choices=["show", "repr", "none"],
        help="Specify parsing result task. Default: %default.",
    )
    parser.add_option(
        "--std",
        default="f2003",
        choices=["f2003", "f2008"],
        help="Specify the Fortran standard to use. Default: %default.",
    )


def get_fortran_code_group(parser):
    group = OptionGroup(
        parser,
        "Fortran code options",
        description="Specify information about Fortran codes.",
    )
    group.add_option(
        "--mode",
        default="auto",
        choices=["auto", "free", "fix", "f77", "pyf"],
        help="Specify Fortran code mode. Default: %default.",
    )
    return group
