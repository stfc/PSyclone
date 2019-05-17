#!/bin/bash
for filename in /home/rupert/proj/lfric_trunk/gungho/source/algorithm/*.x90; do
    psyclone $filename -d /home/rupert/proj/lfric_trunk/gungho/source/kernel -api dynamo0.3 -oalg alg.f90 -opsy psy.f90 -s ./kernel_constants.py
done

