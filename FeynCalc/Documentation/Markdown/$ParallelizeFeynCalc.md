## $ParallelizeFeynCalc

`$ParallelizeFeynCalc` is a global switch that enables FeynCalc to evaluate some subroutines on using parallel kernels. It should be explicitly activated by setting `$ParallelizeFeynCalc` to `True`. However, before that one should evaluate `LaunchKernels[n]` with `n` being the number of parallel kernels to launch. The default value is `False`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
$ParallelizeFeynCalc
```

$$\text{False}$$