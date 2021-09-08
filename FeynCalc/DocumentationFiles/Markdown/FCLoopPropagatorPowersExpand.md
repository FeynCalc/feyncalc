## FCLoopPropagatorPowersExpand

`FCLoopPropagatorPowersExpand[exp]` rewrites propagators raised to integer powers as products.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
SFAD[{q, m, 2}]
FCLoopPropagatorPowersExpand[%]
% // StandardForm
```

$$\frac{1}{(q^2-m+i \eta )^2}$$

$$\frac{1}{(q^2-m+i \eta )^2}$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q, D], 0, -m, {1, 1}], StandardPropagatorDenominator[Momentum[q, D], 0, -m, {1, 1}]]*)
```

```mathematica
SFAD[{q, m, 2}, q + p]
FCLoopPropagatorPowersExpand[%]
% // StandardForm
```

$$\frac{1}{(q^2-m+i \eta )^2.((p+q)^2+i \eta )}$$

$$\frac{1}{(q^2-m+i \eta )^2.((p+q)^2+i \eta )}$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q, D], 0, -m, {1, 1}], StandardPropagatorDenominator[Momentum[q, D], 0, -m, {1, 1}], StandardPropagatorDenominator[Momentum[p, D] + Momentum[q, D], 0, 0, {1, 1}]]*)
```
