from utils import (
    insert_explicit_loop_parallelism, normalise_loops, add_profiling,
    enhance_tree_information, PARALLELISATION_ISSUES, PRIVATISATION_ISSUES,
    NEMO_MODULES_TO_IMPORT)

from psyclone.psyir.nodes import Loop, Routine, WhileLoop
from psyclone.psyir.transformations import (LoopTiling2DTrans, ProfileTrans,
                                            TransformationError)

# Whether to chase the imported modules to improve symbol information (it can
# also be a list of module filenames to limit the chasing to only specific
# modules). This has to be used in combination with '-I' command flag in order
# to point to the module location directory. We also strongly recommend using
# the '--enable-cache' flag to reduce the performance overhead.
RESOLVE_IMPORTS = NEMO_MODULES_TO_IMPORT

# List of all files that psyclone will skip processing
FILES_TO_SKIP = []

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})

def trans(psyir):
    '''
    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''
    tile_trans = LoopTiling2DTrans()
    prof_trans = ProfileTrans()
    for subroutine in psyir.walk(Routine):

        if subroutine.name.lower() != "ice_thd_zdf_bl99":
            continue

        enhance_tree_information(subroutine)

        normalise_loops(
                subroutine,
                hoist_local_arrays=False,
                convert_array_notation=True,
                convert_range_loops=True,
                hoist_expressions=False,
                scalarise_loops=False
        )

        for loop in subroutine.walk(Loop):
            if not loop.ancestor(WhileLoop):
                continue
            if loop.loop_type == "lat":
                child = loop.loop_body[0]
                if isinstance(child, Loop) and child.loop_type == "lon":
                    try:
                        tile_trans.apply(loop)
                    except TransformationError:
                        continue

        try:
            prof_trans.apply(subroutine.children)
        except TransformationError:
            continue
