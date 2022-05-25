## FCLoopPropagatorPowersCombine

`FCLoopPropagatorPowersCombine[exp]` combines the same propagators in a `FeynAmpDenominator` to one propagator raised to an integer power.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopPropagatorPowersExpand](FCLoopPropagatorPowersExpand.md).

### Examples

```mathematica
SFAD[{{q, 0}, {m, 1}, 1}, {{q, 0}, {m, 1}, 1}] 
 
ex = FCLoopPropagatorPowersCombine[%]
```

$$\frac{1}{(q^2-m+i \eta )^2}$$

$$\frac{1}{(q^2-m+i \eta )^2}$$

```mathematica
ex // StandardForm

(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q, D], 0, -m, {2, 1}]]*)
```

```mathematica
SFAD[{{q, 0}, {m, 1}, -1}, {{q, 0}, {m, 1}, 1}] 
 
ex = FCLoopPropagatorPowersCombine[%]
```

$$\frac{1}{\frac{1}{(q^2-m+i \eta )}.(q^2-m+i \eta )}$$

$$1$$

```mathematica
ex // StandardForm

(*1*)
```

The function automatically employs `FeynAmpDenominatorCombine`.

```mathematica
int = SFAD[{{-k1, 0}, {mc^2, 1}, 1}]  SFAD[{{-k1 - k2 + k3 + p1, 0}, {0, 1}, 1}] SFAD[{{-k1 - k2 + k3 + p1, 0}, {0, 1}, 2}]
```

$$\frac{1}{(\text{k1}^2-\text{mc}^2+i \eta ) ((-\text{k1}-\text{k2}+\text{k3}+\text{p1})^2+i \eta )^3}$$

```mathematica
int // FCI // StandardForm

(*FeynAmpDenominator[StandardPropagatorDenominator[-Momentum[k1, D], 0, -mc^2, {1, 1}]] FeynAmpDenominator[StandardPropagatorDenominator[-Momentum[k1, D] - Momentum[k2, D] + Momentum[k3, D] + Momentum[p1, D], 0, 0, {1, 1}]] FeynAmpDenominator[StandardPropagatorDenominator[-Momentum[k1, D] - Momentum[k2, D] + Momentum[k3, D] + Momentum[p1, D], 0, 0, {2, 1}]]*)
```

```mathematica
res = FCLoopPropagatorPowersCombine[int]
```

$$\frac{1}{(\text{k1}^2-\text{mc}^2+i \eta ).((-\text{k1}-\text{k2}+\text{k3}+\text{p1})^2+i \eta )^3}$$

```mathematica
res // FCI // StandardForm

(*FeynAmpDenominator[StandardPropagatorDenominator[-Momentum[k1, D], 0, -mc^2, {1, 1}], StandardPropagatorDenominator[-Momentum[k1, D] - Momentum[k2, D] + Momentum[k3, D] + Momentum[p1, D], 0, 0, {3, 1}]]*)
```