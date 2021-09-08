## FCSetMetricSignature

`FCSetMetricSignature` sets the signature of the Minkowski metric used when working with Cartesian objects, like `CartesianPair`, `CartesianIndex`, `CartesianMomentum` etc.

The default choice is $(1,-1,-1,-1)$ which corresponds to `FCSetMetricSignature[{1,-1}]`.

### See also

[Overview](Extra/FeynCalc.md), [FCGetMetricSignature](FCGetMetricSignature.md).

### Examples

```mathematica
FCSetMetricSignature[{-1, 1}]
SPD[p, q] // LorentzToCartesian
```

$$p\cdot q-p^0 q^0$$

```mathematica
FCSetMetricSignature[{1, -1}]
SPD[p, q] // LorentzToCartesian
```

$$p^0 q^0-p\cdot q$$
