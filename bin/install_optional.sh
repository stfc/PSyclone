#!/bin/bash
# Simple bash script to install optional dependencies for PSyclone.
# Which packages to install is controlled by environment variables that
# can be set using the Travis 'env' option.

if [ "$WITH_TERMCOLOR" = "1" ]; then
    pip install termcolor
fi
