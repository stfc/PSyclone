from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Reference, Loop, ArrayReference

# Parse the Fortran code into PSyIR
fortran_reader = FortranReader()
filename = "lower_loop_test.f90"
psyir = fortran_reader.psyir_from_file(filename)
psyir.view()

# Increase dimension of PSyIR loop temporaries by 1
loop = psyir.children[0].children[2]
assert isinstance(loop, Loop)
iterator = loop.variable

for ref in loop.loop_body.walk(Reference):
    # Skip references which are inside references.
    if ref.ancestor(Reference):
        continue
    if ref is iterator:
        continue
    if isinstance(ref, ArrayReference):
        print ("array {0}\n".format(ref.name))
    elif type(ref) == Reference:
        print ("scalar {0}\n".format(ref.name))
    else:
        raise Exception("Unexpected Reference node found.")
        
