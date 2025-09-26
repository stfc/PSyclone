# LFRic Infrastructure

This directory contains a pared-down version of the LFRic core
repository. It includes all required source files to compile
and link LFRic examples with PSyclone.

The script ``update.sh`` is provided which allows updating
the infrastructure library when required::

	./update.sh $SOME_PATH_TO/lfric_core

Any LFRic Makefile in PSyclone should include the file
``external/lfric_infrastructure/src/lfric_include_flags.mk``,
which will define the makefile variable ``LFRIC_INCLUDE_FLAGS`` to contain
all required include flags for compilation.
