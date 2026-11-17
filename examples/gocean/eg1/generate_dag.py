# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyclone script to generate DAG of the invoke_0 '''

import os
from psyclone.psyir.nodes import FileContainer
from psyclone.psyGen import InvokeSchedule


def trans(psyir: FileContainer):
    '''
    :param psyir: the PSyIR of the PSy-layer.

    '''
    for invoke in psyir.walk(InvokeSchedule):
        if invoke.name == "invoke_0":
            # Generate a DAG for it. If graphviz is not available this call
            # just returns without doing anything.
            dag_name = "invoke_0_dag"
            invoke.dag(file_name=dag_name, file_format="png")
            dag_name += ".png"
            if os.path.isfile(os.path.join(os.getcwd(), dag_name)):
                print(f"Wrote DAG to file: {dag_name}")
            else:
                print("Failed to generate DAG image. Do you have the graphviz "
                      "library and Python\nbindings installed?")
