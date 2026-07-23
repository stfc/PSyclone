#!/bin/bash
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2026, Science and Technology Facilities Council
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
# Author: J.Henrichs, Bureau of Meteorology

function update_copyright() {
	# This function replaces the last year of a copyright year
	# like 2020-2021 with the current year (e.g. 2020-2024). This
	# is done by the first substitution. The second substitution
	# replaces a single year like 2020 with e.g. 2020-2024

	# Note the usage of ',' in the second statement to avoid that
	# it matches the first format (2020-2023)
	year=$(date +%Y)
	# Ignore case (/I)
	sed -i $1 -e s"/\(Copyright.*[0-9]\{4\}\)-[0-9]\{4\}/\1-$year/I" \
	          -e s"/\(Copyright[^0-9]*[0-9]\{4\}\),/\1-$year,/I"
}

# Determine the PSyclone root directory
ROOT=$(readlink -f $(dirname $0)/..)

# Change into the PSyclone root directory
cd $ROOT

# Make the function accessible outside of this script
export -f update_copyright

# Options for find to exclude the external directory. Note that -print is
# required in the find statements to avoid printing the pruned externals directory
FIND_OPT="-path ./external -prune -o -iname"

echo "Updating python scripts"
find . $FIND_OPT "*.py"       -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating documentation"
find . $FIND_OPT "*.rst"      -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating shell scripts"
find . $FIND_OPT "*.sh"       -print -exec bash -c "update_copyright \{}" \; >/dev/null
find . $FIND_OPT "*.csh"      -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating Makefiles"
find . $FIND_OPT "Makefile*"  -print -exec bash -c "update_copyright \{}" \; >/dev/null
find . $FIND_OPT "*.mk"       -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating Fortran programs"
find . $FIND_OPT "*.[fFxX]90" -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating .h files"
find . $FIND_OPT "*.h" -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating READMEs"
find . $FIND_OPT "README*"    -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating configs"
find . $FIND_OPT "*.cfg"      -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating jinja"
find . $FIND_OPT "*.jinja"    -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating yaml"
find . $FIND_OPT "*.yaml"     -print -exec bash -c "update_copyright \{}" \; >/dev/null
find . $FIND_OPT "*.yml"      -print -exec bash -c "update_copyright \{}" \; >/dev/null

echo "Updating misc"
for i in ./doc/developer_guide/pip_requirements.txt     \
		 ./apt.txt ./bin/psyclonefc ./bin/psyclone-kern \
		 ./bin/psyclone ./bin/psyad ./LICENSE; do
	update_copyright $i >/dev/null
done