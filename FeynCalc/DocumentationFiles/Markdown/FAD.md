## FAD

`FAD` is the FeynCalc external form of `FeynAmpDenominator` and denotes an inverse propagator.

`FAD[q, q-p, ...]` is $\frac{1}{q^2 (q-p)^2 \ldots}$.

`FAD[{q1,m}, {q1-p,m}, q2, ...]` is \frac{1}{[q1^2 - m^2][(q1-p)^2 - m^2] q2^2 }. Translation into FeynCalc internal form is performed by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [FCE](FCE.md), [FCI](FCI.md), [FeynAmpDenominator](FeynAmpDenominator.md), [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify.md), [PropagatorDenominator](PropagatorDenominator.md).

### Examples

```mathematica
FAD[q, p - q]
```

$$\frac{1}{q^2.(p-q)^2}$$

```mathematica
FAD[p, {p - q, m}]
```

$$\frac{1}{p^2.\left((p-q)^2-m^2\right)}$$

```mathematica
FAD[{p, 0, 2}, {p - q, m, 3}]
```

$$\frac{1}{\left(p^2\right)^2.\left((p-q)^2-m^2\right)^3}$$

```mathematica
FAD[q, p - q] // FCI // StandardForm

(*FeynAmpDenominator[PropagatorDenominator[Momentum[q, D], 0], PropagatorDenominator[Momentum[p, D] - Momentum[q, D], 0]]*)
```

```mathematica
FAD[p] FAD[p - q] // FeynAmpDenominatorCombine[#, FCE -> True] & // StandardForm

(*FAD[p, p - q]*)
```
