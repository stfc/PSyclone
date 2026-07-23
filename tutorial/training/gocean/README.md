# PSyclone's GOcean Domain

The hands-on session under this directory contain a set of
training exercise to introduce the ``gocean`` API and
the required ``dl_esm_inf`` infrastructure library.

## 2.2-GameOfLife
This section focusses on the infrastructure library. It does not
use PSyclone itself, but is useful if you want to use the
``gocean`` API for a new application and need to learn the details
about the setup of the infrastructure library.
You will be coding the Game of Life example using the infrastructure
library directly.

This session can be skipped if you are not interested in the details
of the infrastructure library. None of the following hands-on
sessions requires the details contained in this exercise.

## 2.4-GameOfLife-psyclone
This section implements the Game of Life using PSyclone and the
``gocean`` API (and it re-implements the code created in the
previous section).

## 2.5-GameOfLife-module-inline
This section introduces transformation scripts, using kernel module
inlining.

## 2.6-GameOfLife-fuse
This section introduces the loop fusion transformation that
PSyclone provides.

## 2.8-GameOfLife-openmp
This section shows the parallelisation of a code using OpenMP
threading with PSyclone.

## 2.10-GameOfLife-mpi
This section shows how a ``gocean`` API code can be run on a
distributed memory machine without additional code changes.

## 2.12-GameOfLife-extraction
This section shows the kernel extraction and driver creation
using the PSyData API.

## 2.12-GameOfLife-profiling
In this section you will learn how to utilise profiling with
PSyclone.

## 2.12-GameOfLife-value-range-check
PSyclone offers a tool to verify that variables have values
in a specified range. This section shows how this feature
is added, and how to specify the valid range for variables.

## 2.14-GameOfLife-openacc
This section shows how to add OpenACC parallelisation to a
``gocean`` API based code.

## 2.16-GameOfLife-omp-offload
This section shows how to use OpenMP offloading to run your
code on GPUs.

## 2.18-GameOfLife-IntelligentScripts
This open session asks to write a more generic loop-fusion
script that avoids hard-coding the program structure in the
script.
