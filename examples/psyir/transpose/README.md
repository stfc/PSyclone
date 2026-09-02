# PSyclone PSyIR matrix multiplication example.

Demonstrates acceleration of a simple routine for matrix transposition using
`LoopTilingTrans` and `OMPLoopTrans`. To run this example:

```sh
> make
./trans
Passed 0.7202s
./trans_tiled
Passed 0.2420s
./trans_omp
Passed 0.2996s
./trans_omp_tiled
Passed 0.0445s
```

(Sample output from a 20-core Intel i9-12900H.)
