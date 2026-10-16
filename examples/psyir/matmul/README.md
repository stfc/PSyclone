# PSyclone PSyIR matrix multiplication example.

Demonstrates acceleration of a simple routine for matrix multiplication using
`LoopTilingTrans` and `OMPLoopTrans`. To run this example:

```sh
> make
./matmul
Passed   3.213s
./matmul_tiled
Passed   3.291s
./matmul_omp
Passed   1.134s
./matmul_omp_tiled
Passed   0.306s
```

(Sample output from a 20-core Intel i9-12900H.)
