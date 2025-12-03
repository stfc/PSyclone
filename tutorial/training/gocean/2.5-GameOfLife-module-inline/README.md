# Code Optimisations with PSyclone

The measurement at the end of the previous training module showed
that there can be a significant performance loss with the initial
version of the PSyclone Game of Life. In this module we will
apply some transformations to improve the performance of the
code.

## Inlining
As is obvious from the code being able to inline the kernel calls
is essentials for good performance. If a call needs to be executed
for the computations for each cell, the compiler cannot use
vectorisation, and the call overhead itself is likely higher than
the time required for the actual computations.

The Intel compiler is able (given the right compiler options) to
inline code that is contained in a different program unit. It is
able to inline the kernels contained in the various kernel files
into the psy-layer file, and can then vectorise the loop.

The GNU Fortran compiler on the other hand is not able to inline
code from a different file, but it can inline successfully if the
code is contained in the same file.

PSyclone's `KernelModuleInlineTrans` can move the code from the kernel
files into the psy-layer files, which in turn will enable GNU Fortran
to inline the kernel calls. This step is also a pre-requisite for
full inlining, i.e. replacing a function call with the content
of the function.

In order add all kernels to the psy-layer file, the PSyclone
`KernelModuleInlineTrans` transformation has to
be applied to each kernel that should be module inlined. This
requires a script which PSyclone will call after the psy-layer
has been created, but before the Fortran code is produced.

The following example script shows a script that will module inline
the first kernel. It imports the inline transformation
`KernelModuleInlineTrans` to be used on the kernels that should
be inlined. It then gets the psy-layer representation of the
`invoke_compute` invoke statement. Have a look at `time_step_alg_mod.x90`
to see the name 'compute' used in the one invoke statement.
It then takes the first kernel in this schedule (`schedule[0]`) and
applies the inline tranformation.


    from psyclone.transformations import KernelModuleInlineTrans
    from psyclone.gocean1p0 import GOKern

    def trans(psyir):
        '''
        Take the supplied psyir object, and module inline all kernels
    
        :param psy: the PSy layer to transform.
        :type psy: :py:class:`psyclone.psyGen.PSy`
    
        :returns: the transformed PSy object.
        :rtype: :py:class:`psyclone.psyGen.PSy`
    
        '''
        inline = KernelModuleInlineTrans()
    
        for subroutine in psyir.kernels:
            if subroutine.name == "invoke_compute":
                inline.apply(subroutine)    

If you now invoke PSyclone with the additional parameter
`-s inline.py`, the script will be invoked by PSyclone,
and inlining for the first kernel will be enabled.

Look at the psy-layer output file. While the nested loops
and kernel call inside these loops have not changed, the
difference is that the kernel called is now contained in
the same file. This will help the compilers, especially
the GNU Fortran compiler, with inlining, which can make
a significant performance difference.

Measure the performance.

## Inlining All
Obviously, just inlining one kernel is not enough, all
kernels should be inlined. Modify the inline script to
find all invokes and all kernels in each invoke, and
apply the inline transformation to
them. As shown in the presentation, there are various
way in which this can be achieved - using `walk`, 
or `kernels`.

What is performance with the GNU Fortran compiler after
inlining all kernels?
