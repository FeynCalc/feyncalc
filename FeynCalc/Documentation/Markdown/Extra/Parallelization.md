## Parallelization

### See also

[Overview](FeynCalc.md).

Some FeynCalc routines can be parallelized meaning that the code will try to distribute chunks of the calculation to multiple Mathematica kernels. 

To this aim the number of subkernels should roughly correspond to the number of CPU cores

### Enabling parallelization

To enable the parallelization you need to actively launch some parallel kernels and then set the variable `$ParallelizeFeynCalc` to `True`. In this case a copy of FeynCalc will be loaded on each of the parallel kernels and used to parallelize some selected operations. For example,

```mathematica
LaunchKernels[8]
$ParallelizeFeynCalc = True
```

### Functions that support automatic execution on parallel kernels

 - `FCLoopFromGLI`
 - `FCFeynmanPrepare`
 - `FCLoopToPakForm`
 - `FCLoopFindIntegralMappings`

