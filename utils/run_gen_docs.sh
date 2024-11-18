#! /bin/bash

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd "$SCRIPTPATH/.."

#
# Convenience script to create documentation (and test it)
#

# Stop at any error
set -e

cd "$SCRIPTPATH/.."
cd doc/developer_guide; make html SPHINXOPTS="-W --keep-going"

cd "$SCRIPTPATH/.."
cd doc/reference_guide; make html SPHINXOPTS="-W --keep-going"

cd "$SCRIPTPATH/.."
cd doc/user_guide; make html SPHINXOPTS="-W --keep-going"
