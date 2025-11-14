
# MAKEFILE_LIST is a Gnu-make variable that contains all of the
# arguments passed to the first invocation of Make. The last entry
# in this list is the current file.
this_file := $(abspath $(lastword $(MAKEFILE_LIST)))

# PSyclone directory is up two from this file
ROOT_DIR := $(abspath $(dir $(this_file))../../..)
PSYCLONE = psyclone --config $(ROOT_DIR)/config/psyclone.cfg --psykal-dsl lfric
