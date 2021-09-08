## FeynAmpDenominatorExplicit

`FeynAmpDenominatorExplicit[exp]` changes each occurence of `PropagatorDenominator[a,b]` in exp into `1/(SPD[a,a]-b^2)` and replaces `FeynAmpDenominator` by `Identity`.

### See also

[Overview](Extra/FeynCalc.md), [FeynAmpDenominator](FeynAmpDenominator.md), [PropagatorDenominator](PropagatorDenominator.md).

### Examples

```mathematica
FAD[{q, m}, {q - p, 0}]
FeynAmpDenominatorExplicit[%]
% // FCE // StandardForm
```

$$\frac{1}{\left(q^2-m^2\right).(q-p)^2}$$

$$\frac{1}{\left(q^2-m^2\right) \left(-2 (p\cdot q)+p^2+q^2\right)}$$

$$\frac{1}{\left(-m^2+\text{SPD}[q,q]\right) (\text{SPD}[p,p]-2 \;\text{SPD}[p,q]+\text{SPD}[q,q])}$$
