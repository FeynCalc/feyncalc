## FCLoopNonIntegerPropagatorPowersFreeQ

`FCLoopNonIntegerPropagatorPowersFreeQ[int]` checks if the integral contains propagators raised to noninteger (i.e. fractional or symbolic) powers.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopRemoveNegativePropagatorPowers](FCLoopRemoveNegativePropagatorPowers.md).

### Examples

```mathematica
SFAD[{q + p, m^2, 2}]
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]]
```

$$\frac{1}{((p+q)^2-m^2+i \eta )^2}$$

$$\text{True}$$

```mathematica
SFAD[{q + p, m^2, n}]
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]]
```

$$((p+q)^2-m^2+i \eta )^{-n}$$

$$\text{False}$$

```mathematica
CFAD[{l, m^2, 1/2}]
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]]
```

$$\frac{1}{\sqrt{(l^2+m^2-i \eta )}}$$

$$\text{False}$$
