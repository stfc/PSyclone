#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------
# Authors: A. R. Porter and N. Nobre, STFC Daresbury Lab

'''
Python driver script to run PSyclone on (pre-processed) NEMO source files, i.e.
those produced by the build system in <MY_CONFIG_NAME>/BLD/ppsrc/nemo.
For example::

>>> process_nemo.py -s ./kernels_trans.py -o <MY_CONFIG_NAME>/MY_SRC \
  <MY_CONFIG_NAME>/BLD/ppsrc/nemo/*90

Or, if you have Gnu 'parallel':

>>> parallel process_nemo.py -s ./kernels_trans.py -o <MY_CONFIG_NAME>/MY_SRC \
  {} ::: <MY_CONFIG_NAME>/BLD/ppsrc/nemo/*90

'''

from __future__ import print_function
import os
import sys
from time import perf_counter

# Files that we will only add profiling to
PROFILE_ONLY = ["bdyini.f90", "bdydta.f90", "bdyvol.f90",
                "diaobs.f90",
                "diawri.f90",  # Unused & has Return in profile region
                "dommsk.f90",
                "fldread.f90",
                "icbclv.f90", "icbdyn.f90", "icbrst.f90",
                "icbthm.f90", "icbutl.f90", "icbdia.f90", "icbini.f90",
                "icb_oce.f90", "icbstp.f90", "icbtrj.f90",
                "ice.f90",  # lines are too long
                "icedyn_adv_pra.f90",  # lines are too long
                "iceforcing.f90",
                "iceistate.f90",
                "icethd_ent.f90", "icethd_zdf.f90",
                "icethd_dh.f90", "iom.f90", "iom_nf90.f90",
                "obs_inter_h2d.f90", "obs_grid.f90", "obs_averg_h2d.f90",
                "obs_profiles_def.f90", "obs_sort.f90", "obs_types.f90",
                "obs_surf_def.f90", "obs_read_prof.f90", "obs_read_surf.f90",
                "obs_write.f90",
                "stopar.f90",
                "tide_mod.f90", "zdfosm.f90"]

# Files that we won't touch at all
EXCLUDED_FILES = [
    # Re-defines idim intrinsic
    "obs_utils.f90",
    # Unsupported module SAVE attribute
    "obs_profiles.f90",
    # Array accessed inside WHERE does not use array notation
    "diurnal_bulk.f90",
    # mpif.h include is lost
    "mppini.f90", "mpp_map.f90", "obs_mpp.f90", "icblbc.f90",
    "timing.f90", "lib_mpp.f90",
    "nemogcm.f90",
    # Fns defined within fn are lost
    "storng.f90"]


if __name__ == "__main__":
    import argparse

    PARSER = argparse.ArgumentParser(
        description="Process the specified NEMO source files "
        "using PSyclone")
    PARSER.add_argument('input_files', metavar='input_file', type=str,
                        nargs='+',
                        help="One or more NEMO pre-processed source files")
    PARSER.add_argument('-o', dest='out_dir',
                        help="Destination directory for processed source "
                        "files")
    PARSER.add_argument('-s', dest='script_file',
                        help="PSyclone transformation script")
    PARSER.add_argument('-x', dest='exit_on_error', action='store_true',
                        help="Exit immediately if PSyclone fails")
    ARGS = PARSER.parse_args()

    # Check whether the PSyclone command has been specified in an environment
    # variable. We default to just using the 'psyclone' in the PATH.
    PSYCLONE_CMD = os.environ.get("PSYCLONE", "psyclone")

    # Keep a list of files for which PSyclone fails
    FAILED_FILES = []

    for ffile in ARGS.input_files:

        if not os.path.isfile(ffile):
            print(f"Cannot find file '{ffile}' - skipping")
            continue

        file_name = os.path.basename(ffile)
        if ARGS.out_dir:
            out_file = os.path.join(ARGS.out_dir, file_name)
        else:
            out_file = file_name

        args = [PSYCLONE_CMD, "--limit", "output", "-api", "nemo"]
        if file_name in EXCLUDED_FILES:
            print(f"Skipping {ffile} entirely.")
            continue
        if file_name in PROFILE_ONLY:
            print(f"Instrumenting {file_name} for profiling...")
            extra_args = ["-p", "invokes",
                          "-oalg", "/dev/null",
                          "-opsy", out_file, ffile]
        else:
            print(f"Processing {file_name}...")
            extra_args = []
            if ARGS.script_file:
                extra_args = ["-s", ARGS.script_file]
            extra_args += ["-oalg", "/dev/null",
                           "-opsy", out_file, ffile]
        # Since we're in Python we could call psyclone.generator.main()
        # directly but PSyclone is not designed to be called repeatedly
        # in that way and doesn't clear up state between invocations.
        tstart = perf_counter()
        rtype = os.system(" ".join(args + extra_args))
        tstop = perf_counter()

        if rtype != 0:
            print(f"Running PSyclone on {ffile} failed\n")
            if ARGS.exit_on_error:
                sys.exit(1)
            FAILED_FILES.append(ffile)
        else:
            print(f"Time taken for {file_name}: {tstop - tstart:.2f}s")

        print("--------------------\n--------------------\n")

    print("All done.")
    if FAILED_FILES:
        print(f"PSyclone failed on the following source files: {FAILED_FILES}")
