# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: I. Kavcic, Met Office
#         N. Nobre, STFC Daresbury Lab

'''
   Program that extracts and plots XY slices of output data values in
   Example 3 of the LFRic tutorial. '''

import sys
from collections import OrderedDict
import numpy as np
import matplotlib.pyplot as plt


def plot_xy_slices(states, tstep):
    '''
    Plot extracted model states XY slices

    :param states: extracted model states.
    :type states: dict of `numpy.array` objects.
    :param str tstep: timestep value of data.

    '''
    levels = list(states.keys())
    for i, level in enumerate(levels):
        # Get coordinates and data to plot
        x_val = states[level][:, 0]
        y_val = states[level][:, 1]
        height = states[level][:, 2][0]
        data = states[level][:, 3]

        # Create contour plot from flattened arrays
        fig = plt.figure(i + 1)
        plt.subplot()
        ax_plt = plt.gca()
        tcf = ax_plt.tricontourf(x_val, y_val, data)
        fig.colorbar(tcf)
        ax_plt.set_aspect('equal')
        ax_plt.set_xlabel('x')
        ax_plt.set_ylabel('y')
        ax_plt.set_title("Output field values at level %i (z = %.2f m) and "
                         "timestep %s" % (level, height, tstep))

        # Save plots on each levels as a PNG file
        fig_name = '_'.join(["Level", str(level), "timestep", tstep])
        plt.savefig(fig_name+".png")


if __name__ == "__main__":
    # Parse argument list for file name and plot levels
    # pylint: disable=unbalanced-tuple-unpacking
    try:
        ARGS = sys.argv[:]
        FILEIN, PLOT_LEVELS = ARGS[1:3]
    except ValueError:
        print(f"Usage: {sys.argv[0]} <filenin> <plot_levels> where\n"
              f"- <filename> is a string and \n"
              f"- <levels_plot> is a string list of comma-separated integer "
              f"values (e.g. '0, 2, 5') \n"
              f"                for vertical levels in the range of "
              f"(0, nlayers-1).")
        sys.exit()

    # Create integer list of plot levels
    try:
        LEV_PLT = list(map(int, PLOT_LEVELS.split(",")))
    except ValueError:
        print(f"Error: One or more invalid values for plot levels "
              f"'{PLOT_LEVELS}'.")
        sys.exit()

    # Read in the data file with numpy.loadtxt
    try:
        STATE = np.loadtxt(FILEIN)
    except OSError:
        print(f"Error: Input file '{FILEIN}' not found.")
        sys.exit()
    # Extract timestep information
    TIMESTEP = ("").join(
        [i for i in FILEIN.replace('.', '_').split("_") if i.isdigit()])

    # Create indices for vertical levels from the unique level values
    Z_VAL = STATE[:, 2]
    Z_LEV = np.unique(Z_VAL)
    Z_IND = list(range(len(Z_LEV)))

    # Check whether the input levels are in level index list
    try:
        assert all(lev in Z_IND for lev in LEV_PLT)
    except AssertionError:
        print(
            f"Error: One or more input plot levels '{LEV_PLT}' is outside of "
            f"the\n       z-levels range '{Z_IND}'.")
        sys.exit()

    # Create state dictionary on plot levels
    STATE_LEV = OrderedDict((lev, []) for lev in LEV_PLT)

    # Find indices of the x-y levels to plot
    for lev in LEV_PLT:
        plt_ind = np.where(abs(Z_VAL - Z_LEV[lev]) < 1.0e-6)
        STATE_LEV[lev] = STATE[plt_ind]

    # Plot state
    plot_xy_slices(STATE_LEV, TIMESTEP)
