#!/bin/sh
test "$USER" != pearu && echo "Only pearu can upload F2PY apidocs" && exit 1
test html || (echo "Run me from f2py directory as tools/upload_apidocs.sh" && exit 1) || exit 1

cd html && scp -vr ./* pearu,f2py@web.sourceforge.net:/home/groups/f/f2/f2py/htdocs/docs/
cd -