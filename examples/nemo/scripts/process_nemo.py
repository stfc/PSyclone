#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
from __future__ import print_function
import os
import sys

'''
Python driver script to run PSyclone on (pre-processed) NEMO source files, i.e.
those produced by the build system in <MY_CONFIG_NAME>/BLD/ppsrc/nemo.
For example::

>>> process_nemo.py -s ./kernels_trans.py -o <MY_CONFIG_NAME>/MY_SRC <MY_CONFIG_NAME>/BLD/ppsrc/nemo/*90

Or, if you have Gnu 'parallel':

>>> parallel process_nemo.py -s ./kernels_trans.py -o <MY_CONFIG_NAME>/MY_SRC {} ::: <MY_CONFIG_NAME>/BLD/ppsrc/nemo/*90

'''

# Files that we won't attempt to process with PSyclone
EXCLUDED_FILES = ["bdyini.f90", "diaobs.f90", "dommsk.f90",
                  "fldread.f90",
                  "icbclv.f90", "icbdyn.f90", "icblbc.f90", "icbrst.f90",
                  "icbthm.f90", "icbutl.f90", "icbdia.f90", "icbini.f90",
                  "icb_oce.f90", "icbstp.f90", "icbtrj.f90",
                  "ice.f90", "icealb.f90", "icecor.f90", "icedyn.f90",
                  "icedyn_adv.f90", "icedyn_adv_pra.f90", "icedyn_adv_umx.f90",
                  "iceforcing.f90",
                  "iceistate.f90", "iceitd.f90", "icestp.f90", "icetab.f90",
                  "icethd_ent.f90", "icethd_zdf.f90",
                  "icethd_dh.f90", "icevar.f90", "iom.f90", "lbclnk.f90",
                  "obs_inter_h2d.f90", "obs_grid.f90", "obs_averg_h2d.f90",
                  "obs_profiles_def.f90", "obs_sort.f90", "obs_types.f90",
                  "obs_utils.f90",
                  "sbcblk.f90", "sbcblk_algo_ncar.f90", "sbcisf.f90",
                  "sbcice_cice.f90", "storng.f90", "stopar.f90",
                  "tide_mod.f90", "timing.f90"]

if __name__ == "__main__":
    import glob
    import argparse
    from psyclone.generator import main

    parser = argparse.ArgumentParser(
        description="Process the specified NEMO source files "
        "using PSyclone")
    parser.add_argument('input_files', metavar='input_file', type=str,
                        nargs='+',
                        help="One or more NEMO pre-processed source files")
    parser.add_argument('-o', dest='out_dir',
                        help="Destination directory for processed source "
                        "files")
    parser.add_argument('-s', dest='script_file',
                        help="PSyclone transformation script")
    parser.add_argument('-x', dest='exit_on_error', action='store_true',
                        help="eXit immediately if PSyclone fails")
    args = parser.parse_args()

    # Keep a list of files for which PSyclone fails
    failed_files = []
    
    for ffile in args.input_files:

        if not os.path.isfile(ffile):
            print("Cannot find file '{0}' - skipping".format(ffile))
            continue

        file_name = os.path.basename(ffile)
        if file_name in EXCLUDED_FILES:
            continue
        print("Processing {0}...".format(file_name))
        out_file = os.path.join(args.out_dir, file_name)
        extra_args = ["-api", 'nemo', "-s", args.script_file,
                      "-oalg", "/dev/null",
                      "-opsy", out_file, ffile]
        try:
            main(extra_args)
        except SystemExit as err:
            print("Running PSyclone on {0} failed\n".format(ffile))
            print("Error was:\n{0}".format(str(err)))
            if args.exit_on_error:
                exit(1)
            failed_files.append(ffile)
    print("All done.")
    if failed_files:
        print("PSyclone failed for the following source files: {0}".
              format(failed_files))
