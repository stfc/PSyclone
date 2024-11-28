#!/bin/bash

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd "$SCRIPTPATH/.."

NPROC=$(nproc)

echo "Running 'pytest -n ${NPROC} src/psyclone/tests'"
time pytest -n ${NPROC} src/psyclone/tests

