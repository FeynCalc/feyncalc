## FCLoopSamePropagatorHeadsQ

`FCLoopSamePropagatorHeadsQ[exp]` returns `True` if the `FeynAmpDenominator` of `exp` contains only propagator denominators of the same type (e.g. only `StandardPropagatorDenominator` or only `CartesianPropagatorDenominator`).

### See also

[Overview](Extra/FeynCalc.md), [FeynAmpDenominator](FeynAmpDenominator.md).

### Examples

```mathematica
FCI@SFAD[q, q - p]
FCLoopSamePropagatorHeadsQ[%]
```

$$\frac{1}{(q^2+i \eta ).((q-p)^2+i \eta )}$$

$$\text{True}$$

```mathematica
FeynAmpDenominatorCombine[CFAD[q, q - p] SFAD[l, l + k]]
FCLoopSamePropagatorHeadsQ[%]
```

$$\frac{1}{(q^2-i \eta ).((q-p)^2-i \eta ).(l^2+i \eta ).((k+l)^2+i \eta )}$$

$$\text{False}$$
