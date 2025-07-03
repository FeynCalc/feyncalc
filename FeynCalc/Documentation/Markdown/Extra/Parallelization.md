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

Notice that as of now only a small subset of FeynCalc routines supports parallelization. Every such function has an option `FCParallelize` that is set to `False` by default. To enable the parallel mode you explicitly need to call the given function with the option `FCParallelize->True`.

### Synchronizing definitions between multiple kernels

All definitions made via `DataType[x,y]=True`, `ScalarProduct[a,b] = c`, `SPD[a,b] = c` as well as `Commutator` or `AntiCommutator` *before* activating the parallel mode must be repeated. This is necessary to ensure that they are synchronized between the master kernel and the subkernels. To this
aim it is recommended to remove all definitions via

```
FCClearScalarProducts[]
FCClearDataTypes[]
UnDeclareAllCommutators[]
UnDeclareAllAntiCommutators[]
```

and then introduce them again.

Alternatively, you can *first* activate the parallel mode without making any definitions and *then* define everything as you like. `DataType`, `ScalarProduct`, `Commutator` and `AntiCommutator` will automatically distribute the definitions among all subkernels if they detect the parallel mode.


### Functions that support automatic execution on parallel kernels

 - `FCLoopFromGLI`
 - `FCFeynmanPrepare`
 - `FCLoopToPakForm`
 - `FCLoopFindIntegralMappings`
 - `FCLoopFindTopologyMappings`
 - `FCLoopFindSubtopologies`
 - `FCLoopFindTopologies`
 - `FCLoopCreatePartialFractioningRules`
 - `FCLoopApplyTopologyMappings`
 - `FCLoopGetKinematicInvariants`
 - `FCLoopTensorReduce`
 - `Tdec`
 - `FCLoopIsolate`
 - `MomentumCombine`
 - `Collect2` (only when isolation is disabled)
 - `FCClearScalarProducts` (parallelization is enabled by default)
 - `ScalarProduct` (parallelization is enabled by default)
 - `DataType` (parallelization is enabled by default)
 - `FCClearDataTypes` (parallelization is enabled by default)


