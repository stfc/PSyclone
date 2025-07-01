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
# First create a copy of the original source files. This will allow us
# to pre-process the files with different settings if required,
# since always all files in preprocessed will come from src (and are
# therefore identical between all potential preprocessed directories)
cp -r $source/infrastructure/source  $ROOT_DIR/src

PPFLAGS="-DNO_MPI -DRDEF_PRECISION=64 -DR_SOLVER_PRECISION=64  \
        -DR_TRAN_PRECISION=64 -DR_BL_PRECISION=64"

# There are some compiler specific directives in the examples to enable
# compiler-specific traceback, and in case of NVIDIA to avoid some runtime
# issues in namelist_mod. Since none of this is important for PSyclone's
# compilation tests (we don't read namelist files, nor do we even execute
# most of the programs), this is for now ignored.

echo Preprocessing files

# We preprocess ALL files, even .f90 - this way we have one loop to create
# all required directories and files
preprocessed=$ROOT_DIR/preprocessed
mkdir -p $preprocessed

CPP=${CPP:-cpp}
# Preprocess all files - iname will cause f90 and F90 to be returned
all_files=$(find $ROOT_DIR/src -iname "*.f90")

for file in $all_files; do
	# Convert the absolute name to a relative name
	rel_name=${file##$ROOT_DIR/src/}
	rel_path=$(dirname $rel_name)
	# Convert F90 to f90:
	out_file=$preprocessed/${rel_name%.*}.f90
	mkdir -p $preprocessed/$rel_path
	# Single apostrophes (e.g. in "and Queen's Printer") create a
	# preprocessor warning. Ignore these warnings
	cpp -traditional-cpp -P $PPFLAGS $file >$out_file  2>/dev/null
done

echo Running Templaterator
all_templates=$(find $ROOT_DIR/src -iname "*.t90")
for template in $all_templates; do
	rel_name=${template##$ROOT_DIR/src/}
	rel_path=$(dirname $rel_name)

	for kind in real32 real64 int32; do
		if [[ $kind == real* ]]; then
			type="real"
		else
			type="integer"
		fi
		args="-s kind=$kind -s type=$type"
		out_file=$preprocessed/${rel_name%_mod.t90}_${kind}_mod.f90
		echo $template
 		$source/infrastructure/build/tools/Templaterator $args $template -o $out_file
	done
done

echo Creating dependencies
# Create all the dependencies using the fparser script (of which
# we have a copy in PSyclone):
# Preprocess all files
pushd preprocessed

all_files=""
for i in $(find $preprocessed -iname "*.f90"); do
    all_files="$all_files $(realpath -s --relative-to=$preprocessed $i)"
done

../create_dependencies.py $all_files  >dependency

# Copy the makefile include file that defines the required include flags:
cp ../lfric_include_flags.inc .

make -f ../Makefile netcdf

popd
