#!/usr/bin/env python

import os
import sys
from os.path import join, basename, dirname, splitext
from glob import glob

from numpy.distutils import log
from distutils.dep_util import newer


NAME = 'f2py'
AUTHOR = 'Pearu Peterson'
AUTHOR_EMAIL = 'pearu.peterson@gmail.com'
LICENSE = 'BSD'
URL = 'http://code.google.com/p/f2py/'
DOWNLOAD_URL = 'http://code.google.com/p/f2py/downloads/list'
DESCRIPTION = 'The F2PY Project'
LONG_DESCRIPTION = '''\
The F2PY project is created to unify the efforts of supporting easy
connection between Fortran and Python languages. The project will
provide software, documentation, and support to solve a difficult task
of automatically generating Python wrappers to Fortran libraries.

The following packages are provided:

  fparser - Fortran 66/77/90/95/2003 parser

The aim is to have a tool, called f2py, that can automatically
generate Python wrappers to any Fortran program, be it written in
Fortran 66/77 or in Fortran 90/95/2003.

See http://code.google.com/p/f2py/ for more information.
'''
CLASSIFIERS = """\
Development Status :: 3 - Alpha
Intended Audience :: Developers
Intended Audience :: Science/Research
License :: OSI Approved
Natural Language :: English
Programming Language :: C
Programming Language :: Fortran
Programming Language :: Python
Topic :: Scientific/Engineering
Topic :: Software Development
Topic :: Software Development :: Code Generators
Operating System :: Microsoft :: Windows
Operating System :: POSIX
Operating System :: Unix
Operating System :: MacOS
"""

PLATFORMS = ['Linux', 'Windows', 'MacOS']
MAJOR               = 0
MINOR               = 3
MICRO               = 1
ISRELEASED          = not True
VERSION             = '%d.%d.%d' % (MAJOR, MINOR, MICRO)

if os.path.exists('MANIFEST'): os.remove('MANIFEST')

def write_version_py(filename='f2py/version.py'):
    cnt = """
# THIS FILE IS GENERATED FROM F2PY SETUP.PY
short_version='%(version)s'
version='%(version)s'
release=%(isrelease)s

if not release:
    version += '.dev'
    import os
    hg_version_file = os.path.join(os.path.dirname(__file__),
                                   '__hg_version__.py')
    hg_branch_file = os.path.join(os.path.dirname(__file__),'.hg',
                                   'branch')
    svn_version_file = os.path.join(os.path.dirname(__file__),
                                   '__svn_version__.py')
    svn_entries_file = os.path.join(os.path.dirname(__file__),'.svn',
                                   'entries')
    if os.path.isfile(svn_version_file):
        import imp
        svn = imp.load_module('f2py.__svn_version__',
                              open(svn_version_file),
                              svn_version_file,
                              ('.py','U',1))
        version += svn.version
    elif os.path.isfile(hg_version_file):
        import imp
        hg = imp.load_module('f2py.__hg_version__',
                              open(hg_version_file),
                              hg_version_file,
                              ('.py','U',1))
        version += hg.version
    elif os.path.isfile(svn_entries_file):
        import subprocess
        try:
            svn_version = subprocess.Popen(["svnversion", os.path.dirname (__file__)], stdout=subprocess.PIPE).communicate()[0]
        except:
            pass
        else:
            version += svn_version.strip()
    elif os.path.isfile(hg_branch_file):
        import subprocess
        try:
            hg_version = subprocess.Popen(["hg identify --num", os.path.dirname (__file__)], stdout=subprocess.PIPE).communicate()[0]
        except:
            pass
        else:
            version += hg_version.strip()

print version
"""
    a = open(filename, 'w')
    try:
        a.write(cnt % {'version': VERSION, 'isrelease': str(ISRELEASED)})
    finally:
        a.close()


def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration(None,parent_package,top_path)
    config.add_subpackage('f2py')
    config.add_subpackage('fparser')
    config.get_version('f2py/version.py')

    scripts = []
    scripts += glob(join(config.local_path, 'fparser','scripts', '*.py'))
    scripts += glob(join(config.local_path, 'f2py','scripts', '*.py'))

    wininst = 'bdist_wininst' in sys.argv

    for script in scripts:

        subpackage = basename(dirname(dirname(script)))
        
        if basename (script).startswith (subpackage):
            config.add_scripts(script)
            continue

        def generate_a_script(build_dir, script=script, config=config):
            dist = config.get_distribution()
            install_lib = dist.get_command_obj('install_lib')
            if not install_lib.finalized:
                install_lib.finalize_options()

            script_replace_text = ''
            install_lib = install_lib.install_dir
            if install_lib is not None:
                script_replace_text = '''
import sys
if %(d)r not in sys.path:
    sys.path.insert(0, %(d)r)
''' % dict(d=install_lib)

            start_mark = '### START UPDATE SYS.PATH ###'
            end_mark = '### END UPDATE SYS.PATH ###'
            name = basename(script)
            if name.startswith (subpackage):
                target_name = name
            elif wininst:
                target_name = subpackage + '_' + name
            else:
                target_name = subpackage + '.' + splitext(name)[0]
            target = join(build_dir, target_name)
            if newer(script, target) or 1:
                log.info('Creating %r', target)
                f = open (script, 'r')
                text = f.read()
                f.close()

                i = text.find(start_mark)
                if i != -1:
                    j = text.find (end_mark)
                    if j == -1:
                        log.warn ("%r missing %r line", script, start_mark)
                    new_text = text[:i+len (start_mark)] + script_replace_text + text[j:]
                else:
                    new_text = text

                f = open(target, 'w')
                f.write(new_text)
                f.close()
        config.add_scripts(generate_a_script)

    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup

    # Rewrite the version file everytime
    if os.path.exists('f2py/version.py'): os.remove('f2py/version.py')
    write_version_py()

    setup(
        name = NAME,
        author = AUTHOR,
        author_email = AUTHOR_EMAIL,
        license = LICENSE,
        url = URL,
        download_url = DOWNLOAD_URL,
        description = DESCRIPTION,
        long_description = LONG_DESCRIPTION,
        classifiers = filter(None, CLASSIFIERS.split('\n')),
        platforms = PLATFORMS,
        configuration=configuration)
