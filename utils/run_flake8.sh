#!/bin/bash

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd "$SCRIPTPATH/.."

#
# Hint for vscode:
#
# Install black-formatter
#
# In settings:
#  - Activate 'Code Actions On Save'
#  - Set 'Format on Save Mode' to 'modifications'
#

# An example hook script to verify what is about to be pushed.  Called by "git
# push" after it has checked the remote status, but before anything has been
# pushed.  If this script exits with a non-zero status nothing will be pushed.
#
# This hook is called with the following parameters:
#
# $1 -- Name of the remote to which the push is being done
# $2 -- URL to which the push is being done
#
# If pushing without using a named remote those arguments will be equal.
#
# Information about the commits which are being pushed is supplied as lines to
# the standard input in the form:
#
#   <local ref> <local oid> <remote ref> <remote oid>
#
# This script ensures that the whole of PSyclone is linted successfully
# before the push is executed.
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
