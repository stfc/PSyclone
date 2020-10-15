Use the same helmholtz example.

First loop iterates over dofs so does not compute annexed dofs. grad_p is written in the first loop and read in the second loop. The second loop iterates over xxx so will need annexed dofs. Therefore need a halo exchange between the two.

If we compute annexed dofs redundantly then this should not be required. This is implemented as a switch as if this is applied in all situations then we can assume no halo exchange at all when computing annexed dofs.

This is particularly beneficial as a halo exchange updates the whole halo, not just annexed dofs. Also perform halo exchanges when there might be nannxed dofs (i.e. we don't know).

config file.

Copy of config file available in this directory

Use and edit a local copy ...

edit the file to switch on annexed dofs.

rerun the code

> psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py helmholtz_solver_alg_mod.x90 --config psyclone.cfg

## Key points

* Redundant computation of "annexed dofs" can reduce the number of required halo exchanges

