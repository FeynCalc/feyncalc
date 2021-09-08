## FCLoopMixedIntegralQ

`FCLoopMixedIntegralQ[exp]` returns `True` if the integral contains both Lorentz and Cartesian indices and momenta.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FCI[FVD[p, mu] CFAD[q, q - p]]
FCLoopMixedIntegralQ[%]
```

$$\frac{p^{\text{mu}}}{(q^2-i \eta ).((q-p)^2-i \eta )}$$

$$\text{True}$$

```mathematica
FCI[FVD[p, mu] FAD[q, q - p]]
FCLoopMixedIntegralQ[%]
```

$$\frac{p^{\text{mu}}}{q^2.(q-p)^2}$$

$$\text{False}$$
