#!/bin/sh

test -d doc || (echo "Run me from f2py directory as tools/mk_apidocs.sh" && exit 1) || exit 1
HTML_TARGET=html
rm -rf doc/_build/ doc/source/generated $HTML_TARGET/*
echo `date -u` ": documentation is being updated... try again in few minutes." >  $HTML_TARGET/index.html
PYTHONPATH=`pwd`:$PYTHONPATH
cd doc && make html || exit 1
cd -
exit 0

