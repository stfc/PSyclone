#!/bin/bash

#
# Hint: For vscode
#
# Install the extension `coverage-gutters` and execute the command "Coverage Gutters: Display Coverage"
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
OPTS=" --cov-report term"

if [[ -e cov.xml ]]; then
	echo "Removing previous reporting file 'cov.xml'"
	rm -rf cov.xml
fi

echo "Running 'pytest --cov $PSYCLONE_MODULE --cov-report $COV_REPORT -n $(nproc) $SRC_DIR'"
time pytest --cov $PSYCLONE_MODULE --cov-report $COV_REPORT $OPTS -n $(nproc) $SRC_DIR

