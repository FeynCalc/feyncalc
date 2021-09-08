## FCLoopRemoveNegativePropagatorPowers

`FCLoopRemoveNegativePropagatorPowers[exp]` rewrites propagators raised to integer powers as products.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
SFAD[{q, m, -1}]
FCLoopRemoveNegativePropagatorPowers[%]
% // StandardForm
```

$$(q^2-m+i \eta )$$

$$q^2-m$$

```
(*-m + Pair[Momentum[q, D], Momentum[q, D]]*)
```

```mathematica
SFAD[{q, m}, q + p, {q, m, -2}]
FCLoopRemoveNegativePropagatorPowers[%]
% // StandardForm

```

$$\frac{1}{(q^2-m+i \eta ).((p+q)^2+i \eta ).\frac{1}{(q^2-m+i \eta )^2}}$$

$$\frac{q^2-m}{((p+q)^2+i \eta )}$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D] + Momentum[q, D], 0, 0, {1, 1}]] (-m + Pair[Momentum[q, D], Momentum[q, D]])*)
```
