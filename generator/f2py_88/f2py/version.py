
# THIS FILE IS GENERATED FROM F2PY SETUP.PY
short_version='0.3.1'
version='0.3.1'
release=False

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
