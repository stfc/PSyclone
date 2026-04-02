# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2026, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ------------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology

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

