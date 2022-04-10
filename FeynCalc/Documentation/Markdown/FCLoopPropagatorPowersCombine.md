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