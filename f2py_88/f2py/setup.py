
from glob import glob
from os.path import join, isdir, dirname

def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('f2py',parent_package,top_path)
    if isdir(join(top_path or '.', '.svn')):
        config.make_svn_version_py()
    if isdir(join(top_path or '.', '.hg')):
        try: config.make_hg_version_py() # requires numpy rev 8527 or newer
        except AttributeError: pass
    return config
