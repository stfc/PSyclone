# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# The Makefile target `test` runs tests on the gitlab CI (i.e. without
# compilation), `test_run` will compile all binaries (as far as possible
# since some examples cannot be compiled), and execute the binary (and
# if tests are available for checking the binary they will be triggered
# as well). In case that an example cannot be compiled, `test_run` will
# still execute all `test` targets. So, `test_run` is the most complete
# set of tests (and is executed as part of PSyclone's integration tests).
# The `clean` target removes all temporary files (e.g. transformed code,
# object # and mod files). Using `allclean` will additionally clean all
# libraries (e.g. infrastructure, psydata).

.PHONY: allclean clean test test_run

allclean clean test test_run:
	$(MAKE) -C lfric_users $@
	$(MAKE) -C gocean $@
	$(MAKE) -C transformation $@
	$(MAKE) -C lfric $@
