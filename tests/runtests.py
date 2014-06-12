#! /usr/bin/env python
from optparse import OptionParser
import os,re,sys,fnmatch
def get_immediate_subdirectories(dir):
    p = re.compile('\d+_')
    return [name for name in os.listdir(dir)
            if os.path.isdir(os.path.join(dir, name)) and p.match(name) is not None]

usage = "usage: %prog [top_level_directory]"
version = "1.0"
parser = OptionParser(usage)
(options, args) = parser.parse_args()

if len(args) > 1:
    parser.error("incorrect number of arguments")
elif (len(args) == 1):
    root=args[0]
    # check it is a valid directory
    if not os.path.isdir(root):
        parser.error("specified directory does not exist")
    rootList=[root]
else:
    rootList=get_immediate_subdirectories(os.getcwd())

print "Running tests from: "+str(rootList)

def locate(pattern, rootList=rootList):
    '''Locate path to all files matching supplied filename pattern in and below
    supplied root directory.'''
    rootDir=os.path.abspath(os.path.dirname(sys.argv[0]))
    print pattern
    p = re.compile(pattern)
    for root in rootList:
        os.chdir(rootDir) # needed as os.walk changes the base directory
        basePath=os.path.abspath(root)
    	for path, dirs, files in os.walk(basePath):
            if '.svn' in dirs:
                dirs.remove('.svn')
            filenames=[]
            for candidate in files:
                if p.match(candidate) is not None:
                    filenames.append(candidate)
            for filename in filenames:
                yield path, filename

for path,filename in locate('.*tests?\.py',rootList):
    print path,filename
    os.chdir(path)
    try:
        moduleName=filename.split('.')[0]
        print moduleName
        import importlib
        my_module = importlib.import_module(moduleName)
        unittest.main()
    except Exception as e:
        print e
