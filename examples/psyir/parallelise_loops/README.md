# Parallelise loops using OpenMP

This directory contains an example of how write PSyclone scripts to parallelise
loops by using an OpenMP transformation with various options to adapt to the
desired target.

The example can be executed using the Makefile, or directly with the command:
``psyclone -s add_parallelism.py example.f90``.
