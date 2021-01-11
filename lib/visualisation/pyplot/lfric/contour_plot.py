# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors J. Henrichs, Bureau of Meteorology


'''A simple python plot class, that receives a field and chi vector from
a PSyData wrapper, and plots the data. This needs to be adjust depending on
the data to be plotted.'''

import sys
import matplotlib.pyplot as plt

GLOBAL_PLOT = None
GLOBAL_CHI = None


# =============================================================================
# pylint: disable=too-few-public-methods
class Plot:
    '''A plot class to interface with PSyData visualisation.
    '''
    def __init__(self):
        self.figure = plt.figure()
        self.axes = plt.subplot()
        self.axes.set_aspect('equal')
        self.axes.set_xlabel('x')
        self.axes.set_ylabel('y')
        self.axes.set_title("Output field values at level 500")
        self.colorbar = None

    # -------------------------------------------------------------------------
    # pylint: disable=invalid-name
    def update(self, x, y, data):
        '''Plots the new data.
        :param x: x-grid values.
        :type x: list of floats
        :param y: y-grid values.
        :type y: list of floats
        :param data: the data values to plot.
        :type data: list of floats

        '''
        tcf = self.axes.tricontourf(x, y, data)
        # Replace an existing colorbar to adjust for new values
        # TODO: how can we 'remap' a colorbar?
        if self.colorbar:
            self.colorbar.remove()
        self.colorbar = self.figure.colorbar(tcf)
        plt.draw()
        plt.pause(0.001)


# =============================================================================

def init_contour_plot():
    '''Called to create the plot instance, and store it in ``GLOBAL_PLOT``.
    '''
    # pylint: disable=global-statement
    global GLOBAL_PLOT

    if GLOBAL_PLOT is not None:
        print("Second initialisation of Plot object - aborting")
        sys.exit()

    GLOBAL_PLOT = Plot()


# =============================================================================
def set_grid(chi1, chi2, chi3):
    '''Stores the grid vector in ``GLOBAL_CHI``.
    '''
    # pylint: disable=global-statement
    global GLOBAL_CHI

    GLOBAL_CHI = [chi1[:], chi2[:], chi3[:]]


# =============================================================================
def update_plot_data(field):
    '''Called to draw a new contour plot. It picks the values
    that have Z 500.
    '''
    # pylint: disable=global-statement
    global GLOBAL_CHI, GLOBAL_PLOT
    # pylint: disable=invalid-name
    x = []
    y = []
    data = []
    for i, value in enumerate(field):
        if abs(GLOBAL_CHI[2][i]-500.0) < 1.0e-6:
            x.append(GLOBAL_CHI[0][i])
            y.append(GLOBAL_CHI[1][i])
            data.append(value)
    GLOBAL_PLOT.update(x, y, data)
