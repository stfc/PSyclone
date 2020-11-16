Early days with OpenACC. Just a basic illustration of some of the things that can be done. Working OpenACC will be presented for NEMO api. Manual implementations done on small benchmarks and need to restructure kernel code to get performance.


Can add parallel directives.
Need to colour first to make them independent
!acc parallel loop independent
We don't have both so do two directives ...

Can you see the error in code generation
also need to copy back data for halo exchanges

!$acc routine
kernel will run serially

Now need to modify kernel code to say kernel running on GPU.

Get each kernel psyir.

Add routine directive

Write to new file.

Run very slowly. Know changes that will make it go faster - inlining, ... . Working on support these in PSyclone but not available yet.
