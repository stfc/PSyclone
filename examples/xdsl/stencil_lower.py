from subprocess import run, PIPE

'''
  Example xDSL / MLIR lowering script - the DMP options are hard-wired
'''

# Extract stencils
stencil_gen = run(
  [
    'psy-opt',
    '-p', (
      'apply-stencil-analysis,lower-psy-ir,lower-mpi,'
      'extract-stencil,rewrite-fir-to-standard'
    ),
    'psy_output.mlir',
    '--output-module-files'
  ],
  stdout=PIPE, stderr=PIPE
)

if stencil_gen.returncode != 0:
    print(f"Stencil generation: ERROR\n{stencil_gen.stderr.decode()}")
    exit(1)
else:
    print("Stencil generation: OK")

'''
  Apply distributed-memory parallelism
  NOTE: There are different strategies e.g. '3d-grid',
  where 'slices' defines the topology
'''
passes = (
  'distribute-stencil{strategy=3d-grid slices=16,16,8 restrict-domain=false},'
  'convert-stencil-to-ll-mlir,dmp-to-mpi{mpi_init=false},lower-mpi'
)

dmp_gen = run(
  [
    'psy-opt',
    '-p', passes,
    'generated/module_0.mlir',
    '-o', 'stencil.mlir'
  ],
  stdout=PIPE, stderr=PIPE
)

if dmp_gen.returncode != 0:
    print(f"DMP generation: ERROR\n{dmp_gen.stderr.decode()}")
    exit(2)
else:
    print("DMP generation: OK")

# Optimisation and lowering to LLVM-IR for compilation
mlir_opt = run(
  [
    'mlir-opt',
    """--pass-pipeline=builtin.module(canonicalize, cse,
    loop-invariant-code-motion, canonicalize, cse, loop-invariant-code-motion,
    cse,canonicalize,expand-strided-metadata,fold-memref-alias-ops,
    lower-affine,finalize-memref-to-llvm,loop-invariant-code-motion,
    canonicalize,cse,convert-scf-to-openmp,finalize-memref-to-llvm,
    convert-scf-to-cf,convert-openmp-to-llvm,convert-math-to-llvm,
    convert-func-to-llvm,
    reconcile-unrealized-casts,canonicalize,cse)""",
    'stencil.mlir'
  ],
  stdout=PIPE, stderr=PIPE
)

if mlir_opt.returncode != 0:
    print(f"MLIR optimisation: \n{mlir_opt.stderr.decode()}")
    exit(3)
else:
    print("MLIR optimisation: OK")

mlir_translate = run(
  [
    'mlir-translate',
    '--mlir-to-llvmir',
    '-o', 'stencil.bc'
  ],
  input=mlir_opt.stdout, stdout=PIPE, stderr=PIPE
)

if mlir_translate.returncode != 0:
    print(f"LLVM-IR generation: ERROR\n{mlir_translate.stderr.decode()}")
    exit(4)
else:
    print("LLVM-IR generation: OK")
