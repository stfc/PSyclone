# PSyclone LFRic Examples

These examples assume that you have PSyclone installed. The easiest
way to do this is via pip, e.g. `pip install psyclone`. See the user
manual for more details (`../../psyclone.pdf` or
http://psyclone.readthedocs.io/en/stable/). After doing this `psyclone`
should be on your PATH.

The first two examples are primarily provided to illustrate the use of
the PSyclone code-generation system. No guarantee is made as to their
functional correctness or usefulness (i.e. the calculations that they
perform may often be nonsensical - it is the use of PSyclone that is
being illustrated).

## Example 1: Basic Operation

The first example simply illustrates the use of PSyclone to generate
the necessary sequential PSy-layer code for a single invoke() that
specifies one Built-in kernel and one user-supplied kernel:
```sh
cd eg1/
psyclone -nodm -d ../code ./single_invoke.x90
```

(The `-d ../code` argument tells PSyclone where it should search for any
user-supplied kernels.)

PSyclone will output two lots of Fortran code to `stdout` when run in
this way: the first is the transformed Algorithm layer (the code from
`single_invoke.x90`) and the second is the generated PSy-layer code.

The `transform` target in the Makefile will also run this command. It
also repeats it without the `-nodm` flag so that PSyclone generates
the necessary code for running with distributed memory.

## Example 2: Applying Transformations

The second example provides an introduction to the use of
transformations to:

1. display the PSyclone Internal Representation of the PSy-layer code:
   ```sh
   cd eg2/
   psyclone -nodm -d ../code -s ./print_psyir_trans.py ./multi_invoke_mod.x90
   ```

2. module-inline a user-supplied kernel into the PSy layer:
   ```sh
   psyclone -nodm -d ../code -s ./module_inline_trans.py ./multi_invoke_mod.x90
   ```

3. perform loop fusion:
   ```sh
   psyclone -nodm -d ../code -s ./loop_fuse_trans.py ./multi_invoke_mod.x90
   ```

Please see the individual transformation scripts for more details.

## Example 3: Distributed and Shared Memory

The third example can be used to demonstrate PSyclone:

1. generating distributed memory parallel code
   ```sh
   cd eg3/
   psyclone solver_mod.x90
   # look for %set_dirty and %halo_exchange in the generated code
   ```

2. using a transformation script to perform loop colouring and OpenMP
   parallelisation, either with distributed memory parallel code:
   ```sh
   cd eg3/
   psyclone -s ./colouring_and_omp.py solver_mod.x90
   ```

   or without distributed memory parallel code:
   ```sh
   cd eg3/
   psyclone -s ./colouring_and_omp.py -nodm solver_mod.x90
   ```

This example also demonstrates the use of `Wchi` function space metadata
for coordinate fields and the use of `integer`-valued fields in LFRic.

## Example 4: Multiple Built-in Calls and Named Invokes

The fourth example illustrates the use of (multiple) calls to built-in
operations within an invoke as well as the use of the name="..." argument.
It also includes the use of the enforce_bc_kernel_type kernel to apply
boundary conditions.
```sh
cd eg4/
psyclone solver_mod.x90
```
It also has the `backends` Makefile target to inform the development of
PSy-layer code generation using the PSyIR backends.

## Example 5: Stencils

The fifth example illustrates the use of stencils in kernels and the associated
passing of extent and direction information (where appropriate) from the
algorithm layer.
```sh
cd eg5/
psyclone alg.f90
```

## Example 6: Reductions

The sixth example illustrates the use and implementation of
reductions. It also demonstrates the generation of a schedule's
dependence graph in svg format (see dag.svg). Note, that if graphviz
and its Python bindings are not installed then no dag image will be
generated and the dag method will silently return. The example may be
run sequentially:
```sh
cd eg6/
psyclone -nodm alg.x90
```

code parallelised with MPI:
```sh
cd eg6/
psyclone alg.x90
```

code parallelised with OpenMP (and loop fused)
```sh
cd eg6/
psyclone -nodm -s ./omp_script.py alg.x90
```

or code parallelised with both MPI and OpenMP (and loop fused)
```sh
cd eg6/
psyclone -s ./omp_script.py alg.x90
```

By default the OpenMP implementations make use of the OpenMP reduction
support. OpenMP reductions do not guarantee the same results from one
run to the next for runs with the same number of OpenMP
threads. Therefore a "reprod" option has been added to the OpenMP do
loop transformation which implements a manual reduction that provides
the same results from one run to the next when using the same number
of threads
```sh
cd eg6/
psyclone -s ./omp_reprod_script.py alg.x90
```

## Example 7: Column-Matrix Assembly Operators

The seventh example illustrates the use of PSyclone with kernels that
perform operations with column-wise (Column-Matrix Assembly) operators:
```sh
cd eg7/
psyclone alg.x90
```

## Example 8: Redundant Computation

The eighth example illustrates the use of redundant computation to
remove and/or change the location and depth of halo exchanges:
```sh
cd eg8/
psyclone helmholtz_solver_alg_mod.x90 -s ./redundant_script.py
```

## Example 9: Writing to Discontinuous Fields

The ninth example illustrates the behaviour of discontinuous field writers
and readwriters:
```sh
cd eg9/
psyclone advective_inc_alg_mod.x90
```

This example also demonstrates how to write a PSyclone transformation
script that only colours loops over continuous spaces:
```sh
cd eg9/
psyclone -s ./colouring_and_omp.py -nodm advective_inc_alg_mod.x90
```

## Example 10: Inter-grid Kernels

PSyclone supports so-called 'inter-grid' kernels that map a field (or field
vector) from a coarse mesh onto a fine mesh (prolongation) or from a fine
mesh onto a coarse mesh (restriction). eg10 contains an example algorithm
that takes a field on a fine mesh and restricts it twice before undoing
that by prolonging it twice:
```sh
cd eg10/
psyclone intergrid_3levels.x90
```

This example also demonstrates the use of `ANY_DISCONTINUOUS_SPACE`
function space metadata.

## Example 11: Asynchronous Halo Exchanges

This example shows how asynchronous halo exchange calls can be created
and manipulated:
```sh
cd eg11/
psyclone -s ./async_script.py helmholtz_solver_alg_mod.x90
```

## Example 12: Code Extraction

The twelfth example demonstrates how to apply code extraction to Nodes in
an Invoke Schedule or to a Kernel in an Invoke. For now it only inserts an
`ExtractNode` in appropriate locations. The full support for code
extraction is being developed (please note that distributed memory will not
be supported). This example can extract a list of Nodes:
```sh
cd eg12/
${PSYCLONE} -nodm -s ./extract_nodes.py \
  gw_mixed_schur_preconditioner_alg_mod.x90
```

or the specific Kernel from one Invoke which contains the Kernel
call after applying transformations (here colouring and OpenMP):
```sh
cd eg12/
${PSYCLONE} -nodm -s ./extract_kernel_with_transformations.py \
  gw_mixed_schur_preconditioner_alg_mod.x90
```

This example also contains a Python helper script which displays
useful information for Kernel extraction: names and Schedules of
one or more Invokes which contain call to the specified Kernel:
```sh
cd eg12/
python find_kernel.py
```

For example, looking for `matrix_vector_kernel_code` call in
`gw_mixed_schur_preconditioner_alg_mod.x90` returns:
```python
Kernel call 'matrix_vector_code' was found in

- Invoke 'invoke_0' with the Schedule:
InvokeSchedule[invoke='invoke_0', dm=False]
    ...
    Loop[type='', field_space='any_space_1', it_space='cells', upper_bound='ncells']
        Literal[value:'NOT_INITIALISED']
        Literal[value:'NOT_INITIALISED']
        Literal[value:'1']
        Schedule[]
            CodedKern matrix_vector_kernel_code(m_lumped,ones,mb) [module_inline=False]
    Loop[type='dofs', field_space='any_space_1', it_space='dofs', upper_bound='ndofs']
        Literal[value:'NOT_INITIALISED']
        Literal[value:'NOT_INITIALISED']
        Literal[value:'1']
        Schedule[]
            BuiltIn x_divideby_y(self_mb_lumped_inv,ones,m_lumped)

- Invoke 'invoke_1' with the Schedule:
InvokeSchedule[invoke='invoke_1', dm=False]
    ...
```

## Example 13: Kernel Constants

This example shows how LFRic kernels can be modified so that the
values for the number of degrees of freedom, the number of quadrature
points and the number of layers can be made constant (as they are
passed in by argument by default). To run:
```sh
cd eg13/
psyclone -s ./kernel_constants.py \
../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
-oalg alg.f90 -opsy psy.f90
```

## Example 14: OpenACC

This example shows how OpenACC directives can be added to the LFRic
PSy-layer. It adds OpenACC enter data, parallel and loop directives in the
presence of halo exchanges. It also transforms the (one) user-supplied
kernel with the addition of an `ACC routine` directive.

```sh
cd eg14/
psyclone -s ./acc_parallel_dm.py main.x90
```

The supplied Makefile defines a `compile` target that will build the
transformed code. Currently the compilation will fail because the
generated PSy-layer code does not contain the correct name for the
transformed kernel module (issue #1724).

## Example 15: Optimise matvec Kernel for CPU

This example shows how the LFRic matvec kernel can be optimised by
PSyclone in the same way as it was hand optimised to run efficiently
on a multi-core CPU. This is work in progress. To run:
```sh
cd eg15/
psyclone -s ./matvec_opt.py \
../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
-oalg /dev/null -opsy /dev/null
```

## Example 16: Generate LFRic Code Using LFRic PSyIR

This example shows how LFRic-specific PSyIR can be used to create
LFRic code. To run:
```sh
cd eg16/
python create.py
```

## Example 17: Runnable Examples

This subdirectory contains three stand-alone and runnable examples of
the LFRic code. For more details please refer to the relevant
[eg17/README.md](./eg17) document.


## Example 18: Special Accesses of Continuous Fields - Incrementing After Reading and Writing Before (Potentially) Reading.

This example shows the use of kernel with a ``GH_READINC`` access and
a kernel with a ``GH_WRITE`` access, both for fields on continuous
function spaces. ``GH_READINC``
access indicates that a field is first read within a kernel and then
subsequently incremented. The field must, therefore, be on a
continuous function space. The only difference from a PSyclone code
generation point of view (for vanilla distributed memory code
generation) is that a ``GH_READINC`` access will produce a halo
exchange call before the associated kernel whereas a ``GH_INC`` access
will not. The example demonstrates the generation of such a halo
exchange, see the ``CALL mass_flux_i_proxy%halo_exchange(depth=1)``
line in the generated code. if you manually change the metadata to
``GH_INC`` in this example and ensure that the configuration file has
``COMPUTE_ANNEXED_DOFS`` set to ``true``, you will see that this halo
exchange is not generated (although the generated code would then be
invalid).

``GH_WRITE``, when used for a field on a continuous function space, means
that the kernel guarantees to write the same value to a given shared entity,
independent of which cell is currently being updated. This means that
annexed DoFs on owned cells will be correctly computed without the need to
iterate into the L1 halo and thus the outer loop limit is always
``last_edge_cell``, irrespective of whether distributed memory is turned on.

To run:

```sh
cd eg18/
psyclone advection_alg_mod.x90
# Optionally edit 'impose_min_flux_kernel_mod.f90' line 65 to replace
# 'GH_READINC' with 'GH_INC', change the value of 'COMPUTE_ANNEXED_DOFS' to
# 'true' in the config file and re-run psyclone.
```

## Example 19: Mixed precision

This example shows the use of the LFRic mixed-precision support to
call a kernel with scalars, fields and operators of different
precision.

## Code

Location of LFRic algorithm and kernel code that is used by two or
more examples.

## Scripts

A collection of example PSyclone scripts.
