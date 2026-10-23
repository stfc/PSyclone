#!/bin/bash
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
