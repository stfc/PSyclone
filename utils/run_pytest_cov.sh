#!/bin/bash

#
# Hint: For vscode
#
# - The .xml file can be also generated with configuration option
#   `python.testing.pytestArgs`
#   by adding the two (separate) items
#   - `--cov-report`
#   - `xml:cov.xml`
#
# - To show the coverage in the code, the extension `coverage-gutters` can be used
#   In a particular source code file, execute the command "Coverage Gutters: Display Coverage"
#

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd "$SCRIPTPATH/.."

PSYCLONE_MODULE="psyclone"
if [[ ! -z $1 ]]; then
	PSYCLONE_MODULE=$1
fi

SRC_DIR="src/psyclone/tests"
if [[ ! -z $2 ]]; then
	SRC_DIR=$2
fi

COV_REPORT="xml:cov.xml"

# Additional options
# Also write to Terminal
#OPTS=" --cov-report term"

#echo "Running 'pytest --cov $PSYCLONE_MODULE --cov-report term-missing -n $(nproc) $SRC_DIR'"
#pytest --cov $PSYCLONE_MODULE -v --cov-report term-missing -n $(nproc) $SRC_DIR
echo "Running 'pytest --cov $PSYCLONE_MODULE --cov-report $COV_REPORT -n $(nproc) $SRC_DIR'"
pytest --cov $PSYCLONE_MODULE -v --cov-report $COV_REPORT $OPTS -n $(nproc) $SRC_DIR

