use the same example code as in dist mem tutorial

Add OpenMP parallel directives around loops in psy-layer

Only one of the loops has been parallelised. Do you know why?

Let's look at the error messages ...

Continous loops need to be coloured in order to use OpenMP parallel directives

So, let's colour them first. We want to colour if it is or may be continuous. So we check that it is not discontinuous. Also need to check if over dofs as don't want to do that and transformation will raise an exception if if do try.

This looks better. Notice that the refuses to parallel the loop over colours which is what we want.

Reduction has be parallelised. However is not guaranteed to provide the same results from one run to the next as spec says can sum up in any order.

Can modify transformation to produce reproducible global sums ...

Now we sum to private values and add the thread results in a sequential order, rather than using the reduction. Notice padding added to avoid false sharing. Size of padding can be configured for particular architecture.

*** GET ERROR ON MASTER ***
