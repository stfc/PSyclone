In this example you will switch on "annexed dof" redundant computation
for kernels that iterate over dofs (currently only builtin kernels).

## 1: The example

We will use the same helmholtz code that we used in the previous
example. As already mentioned this is one of the most computationally
costly routines in the LFRic dynamical core.

## 2: Make a prediction

Generate the code again:

> psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py ../code/helmholtz_solver_alg_mod.x90

When we apply the annexed dof optimisation, it will remove at least
one of the halo exchanges in the generated code.

Study the code and try to predict which one(s) will be removed. You
should be able to work it out from the earlier presentation or by
inspection.

You can look at the PSyIR schedule view and/or the generated PSy-layer
code, whatever you prefer. You might want/need to refer to the kernel
metadata as well.

Reminder: the annexed dof optimisation ensures that annexed dofs are
computed redundantly (by changing the loop bounds of loops that
iterate over dofs).

## 3: The config file

You will see a configuration file in the directory called
"psyclone.cfg". The file format conforms to the Python configparser
format.

This file is an exact copy of the configuration file found in
<psyclone_home>/config which is what PSyclone uses by default. You can
diff the two files to check if you like.

Open this file in your favourite editor. You will see that it has
general (default) section for settings that are common to all APIs and
separate section and sections for each of the APIs.

We are using the API called "dynamo0.3". Note, this is a historical
name which will be changed to "lfric".

Find the dynamo0.3 section and the COMPUTE_ANNEXED_DOFS option within that section.

This option is set to false by default so change this value to true
and then save the modified file.

## 5: Rerun PSyclone

PSyclone allows you to specify a particular config file on the command
line, so lets use this feature to rerun using the one we have just
modified. Let's also save the generated psy-layer code to a different
file.

> psyclone -oalg /dev/null -opsy psy_annexed.f90 -s ./schedule.py helmholtz_solver_alg_mod.x90 --config psyclone.cfg

## 5: Differences

You should see a difference in the schedule view that is output to the
screen and in the generated psy-layer code.

You might also like to see the difference between the two generated psy-layer codes.

> diff psy.f90 psy_annexed.f90

Are the changes what you predicted? If so, well done. If not, don't
worry, determining where to place halo exchanges in parallel code and
where to do redundant computation to reduce the number of halo
exchanges is a difficult and error prone task, especially if the
science code is mixed in with the parallel code and is changing over
time. Isn't it great to have a computer work it out for you instead!

To illustrate that this complexity is an issue in practice, take a
look at the following comment that is found before two halo exchange
calls (called lbc_lnk) in the current NEMO source code:

!! TO BE SUPPRESSED ?  these lbc_lnk are useless since zwdlmtu and zwdlmtv are defined everywhere !

## Key points

* Redundant computation of "annexed dofs" can reduce the number of
  required halo exchanges in LFRic

* Switching this option on or off is done with a single flag in the
  configuration file.

* The science code (algorithm and kernel code) does not change,
  therefore science developers do not need to be concerned with
  parallelism issues.

* The resultant code is guaranteed to be correct (if the rules being
  followed are correct!). This helps the HPC expert.

* Halo exchanges are only called where they are required - there are
  no "just in case" halo exchanges.

## Congratulations

You have finished this section of the tutorial.
