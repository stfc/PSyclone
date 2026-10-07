# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Get all directories with a Makefile, which will be in all examples,
# and then remove the Makefile from the list to get the list of all
# directories (otherwise we would also get __pycache__)
EXAMPLES=$(sort $(patsubst %/Makefile,%,$(wildcard */Makefile)))

allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
compile_EXAMPLES=$(addprefix compile_,$(EXAMPLES))
test_run_EXAMPLES=$(addprefix test_run_,$(EXAMPLES))
test_EXAMPLES=$(addprefix test_,$(EXAMPLES))
transform_EXAMPLES=$(addprefix transform_,$(EXAMPLES))

.PHONY:  $(allclean_EXAMPLES) $(clean_EXAMPLES) $(compile_EXAMPLES)  \
		$(test_run_EXAMPLES) $(test_EXAMPLES)                        \
		allclean clean test test_run

allclean: $(allclean_EXAMPLES)
clean: $(clean_EXAMPLES)
test: $(test_EXAMPLES)
test_run: $(test_run_EXAMPLES)

# Note that not all examples in the user training compile (since some
# shows errors), they will all run the 'test' target if compile or
# run is selected (so everything is tested as far as possible).
$(allclean_EXAMPLES):
	$(MAKE) -C $(patsubst allclean_%,%,$@) allclean

$(clean_EXAMPLES):
	$(MAKE) -C $(patsubst clean_%,%,$@) clean

$(compile_EXAMPLES):
	$(MAKE) -C $(patsubst compile_%,%,$@) compile

$(test_EXAMPLES):
	$(MAKE) -C $(patsubst test_%,%,$@) test

$(test_run_EXAMPLES):
	$(MAKE) -C $(patsubst test_run_%,%,$@) test_run

