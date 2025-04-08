#!/bin/bash

FILE="$(realpath "$0")"
SCRIPTPATH="$( cd -- "$(dirname "$FILE")" >/dev/null 2>&1 ; pwd -P )"
cd "$SCRIPTPATH/.."

#
# Hint for vscode:
# ===========================
#
# Install black-formatter
#
# In settings:
#  - Activate 'Code Actions On Save'
#  - Set 'Format on Save Mode' to 'modifications'
#

#
# GIT related information:
# ===========================
#
# This script can be also used as a git hook in .git/hooks/pre-push
# It then ensures that the whole of PSyclone is linted successfully
# before the push is executed.
#
# If this script exits with a non-zero status nothing will be pushed.
#
# This hook is called with the following parameters:
#
# $1 -- Name of the remote to which the push is being done
# $2 -- URL to which the push is being done
#
# If pushing without using a named remote those arguments will be equal.
#
# Information about the commits which are being pushed is supplied as lines
# to the standard input in the form:
#
#   <local ref> <local oid> <remote ref> <remote oid>
#


remote="$1"
url="$2"

if ! command -v flake8; then
    echo "WARNING: source not linted because flake8 unavailable"
    exit 0
fi

flake8 src/psyclone

if [[ $? -ne 0 ]]; then
    echo "Linting failed"
    exit 1
else
    echo "Linting succeeded"
fi

exit 0
