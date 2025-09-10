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

lfric_core=$1
lfric_infra_src=$lfric_core/infrastructure/source

if [[ $lfric_core == https* ]]; then
	echo "Using subversion link is not yet supported."
	exit
fi

# Verify that we have indeed an lfric core root directory:
if [[ ! -d $lfric_core/components ]]; then
        echo "'$lfric_core' does not seem to be an LFRic_core checkout, can't find 'components'."
        exit
fi

# Get the root dir of this script, which is in external/lfric_infrastructure
ROOT_DIR=$( cd -- "$( dirname -- "$(readlink -f ${BASH_SOURCE[0]})" )" &> /dev/null && pwd )
SOURCE=$ROOT_DIR/src
if [[ -d ./src ]]; then
	rm -rf src.backup
	mv src src.backup
fi

# Compilation of several tests in the test directory require
# additional files from the LFRic apps repository, which are
# just added to the lfric library. For now we just maintain
# a single copy of these files to reduce dependencies to LFRic apps.
# If compilation errors occur, it might be required to manually
# update these files from a current LFRic apps repository.
mkdir -p $SOURCE/apps
cp $ROOT_DIR/apps/*.f90 $SOURCE/apps

mkdir -p $SOURCE/components
cp $lfric_core/components/driver/source/driver_collections_mod.f90 $SOURCE/components

# Add svn info to the source directory
svn info $lfric_core >$SOURCE/svn_info

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

# Get the default preprocessor if the user has no CPP defined:
CPP=${CPP:-cpp}

# Preprocess all files - `-iname`` will cause f90 and F90 to be returned
all_files=$(find $lfric_infra_src -iname "*.f90")

for file in $all_files; do
	# Convert the absolute name to a relative name by removing
	# the lcric_infa_src path (which `find` has added to
	# all paths).
	# The ## specifies removal of the following string
	# from the variable, i.e. a="/a/b/c.f"; rel=${a##/a/}
	# would assign ``b/c.f``
	rel_name=${file##$lfric_infra_src/}
	# Convert all suffixes to f90. The `%` removes everything after the
	# given string, which is `.*`, i.e. it removes the suffix.
	out_file=$SOURCE/${rel_name%.*}.f90

	# Get the directory path (which is relative to the infrastructure
	# source directory):
	rel_path=$(dirname $rel_name)
	mkdir -p $SOURCE/$rel_path
	# Single apostrophes (e.g. in "and Queen's Printer") create a
	# preprocessor warning. Ignore these warnings
	$CPP -traditional-cpp -P $PPFLAGS $file >$out_file  2>/dev/null
done

# The LFRic infrastructure needs many global collections. There is a separate
# function that initialises all of them at once. Using it reduces code and
# should make our example more robust to changes in the LFRic infrastructure.
# So copy that one file from the components directory and add it to the build.
cp $lfric_core/components/driver/source/driver_collections_mod.f90 \
	 $SOUREC/components

# The LFRic infrastructure depends on some files under components.
# Copy these files into the src tree:
cp $lfric_core/components/driver/source/driver_model_data_mod.f90 $SOURCE/components
cp $lfric_core/components/driver/source/driver_modeldb_mod.f90 $SOURCE/components
$CPP -traditional-cpp -P $PPFLAGS \
	$lfric_core/components/driver/source//io_context_collection_mod.F90 \
	> $SOURCE/components/io_context_collection_mod.f90

echo Running Templaterator
all_templates=$(find $lfric_infra_src -iname "*.t90")
for template in $all_templates; do
	rel_name=${template##$lfric_infra_src}
	rel_path=$(dirname $rel_name)

	for kind in real32 real64 int32; do
		if [[ $kind == real* ]]; then
			type="real"
		else
			type="integer"
		fi
		args="-s kind=$kind -s type=$type"
		out_file=$SOURCE/${rel_name%_mod.t90}_${kind}_mod.f90
		$lfric_core/infrastructure/build/tools/Templaterator $args $template -o $out_file
	done
done

pushd src
# Create a dummy Makefile, which delegates the target to the
# Makefile in this directory.

cat << EOF >Makefile
# This Makefile is automatically created when updating
# the LFRic infrastructure by ../update.sh
# ====================================================

# Don't modify the file directly, if required, update ../update.sh
# to update the Makefile.

# This Makefile only delegates the target to the parent's Makefile.
# This way the main Makefile can be shared among different pre-processed
# directories (if required), and is easily visible in git.
#
default:
	\$(MAKE) -f ../Makefile liblfric

clean:
	\$(MAKE) -f ../Makefile clean

allclean:
	\$(MAKE) -f ../Makefile allclean
EOF

all_files=""
for i in $(find $SOURCE -iname "*.f90"); do
    all_files="$all_files $(realpath -s --relative-to=$SOURCE $i)"
done

# Create all the dependencies using the fparser script (of which
# we have a copy in PSyclone):
echo "Creating dependencies.mk for $(echo $all_files | wc -w) files"
../create_dependencies.py $all_files  >dependencies.mk

# Copy the makefile include file that defines the required include flags:
cp ../lfric_include_flags.mk .

echo Compiling infrastructure library
make -f ../Makefile liblfric -j 4

popd
