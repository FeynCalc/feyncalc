## FCGetMetricSignature

`FCGetMetricSignature[]` returns the signature of the Minkowski metric used when working with Cartesian objects, such as `CartesianPair`, `CartesianIndex`, `CartesianMomentum` etc.

` {1,-1}` corresponds to $(1,-1,-1,-1)$ and `{-1,1}` means $(-1, 1, 1, 1)$.

### See also

[Overview](Extra/FeynCalc.md), [FCSetMetricSignature](FCSetMetricSignature.md).

### Examples

```mathematica
FCGetMetricSignature[]
```

$$\{1,-1\}$$
