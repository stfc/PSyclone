#!/usr/bin/bash

# This script takes a version of the lfric_core repository, copies
# all the required source files into the PSyclone tree, and preprocesses
# all files (into a separate directory, to avoid issues with file system
# that do not fully support mixed case).

function help() {

	echo "$0: lfric_core-location"
	echo
	echo "This script takes the location of an lfric core repository"
	echo "(either svn URL or a path to a directory), copies all"
	echo "source files into the PSyclone directory tree and preprocesses"
	echo "all files."
	echo "It is up to the user to then add and commit the changes back"
	echo "to the PSyclone git repo."
	exit -1
}

if [ "$#" -ne 1 ]; then
	help
fi

source=$1

if [[ $source == https* ]]; then
	echo "Using subversion link is not yet supported."
	exit
fi

# Verify that we have indeed an lfric core root directory:
if [[ ! -d $source/components ]]; then
        echo "'$source' does not seem to be an LFRic_core checkout, can't find 'components'."
        exit
fi

# Get the root dir of this script, which is in external/lfric_infrastructure
ROOT_DIR=$( cd -- "$( dirname -- "$(readlink -f ${BASH_SOURCE[0]})" )" &> /dev/null && pwd )


if [[ -d ./src ]]; then
	rm -rf src.backup
	mv src src.backup
fi

echo Copying source files
# First create a copy of the original source files. This will make
# it easier to pre-process the files with different settings if required
cp -r $source/infrastructure/source  $ROOT_DIR/src

PPFLAGS="-DNO_MPI -DRDEF_PRECISION=64 -DR_SOLVER_PRECISION=64  \
        -DR_TRAN_PRECISION=64 -DR_BL_PRECISION=64"

# There are some compiler specific directives in the examples to enable
# compiler-specific traceback, and in case of NVIDIA to avoid some runtime
# issues in namelist_mod. Since none of this is important for PSyclone's
# compilation tests (we don't read namelist files, nor do we even execute
# most of the programs)

# We preprocess ALL files, even .f90 - this way the number of directories
# to manage is half the size.
source_pp=$ROOT_DIR/preprocessed
mkdir -p $ROOT_DIR/preprocessed

CPP=${CPP:-cpp}
# Preprocess all files
all_files=$(find $ROOT_DIR/src -iname "*.f90")
#all_files="/home/joerg/work/psyclone/external/lfric_infrastructure/preprocessed/kernel_metadata/kernel_mod.F90"

echo Preprocessing files
for file in $all_files; do
	# Convert the absolute name to a relative name
	rel_name=${file##$ROOT_DIR/src/}
	rel_path=$(dirname $rel_name)
	mkdir -p $source_pp/$rel_path
	# Single apostrophes (e.g. in "and Queen's Printer") create a
	# preprocessor warning. Ignore these warnings
	cpp -traditional-cpp -P $PPFLAGS $file >$source_pp/$rel_name  2>/dev/null
done

echo Creating dependencies
# Create all the dependencies using the fparser script (of which
# we have a copy in PSyclone):
# Preprocess all files
all_files=$(find $source_pp -iname "*.f90")

./create_dependencies.py $all_files  >dependency

