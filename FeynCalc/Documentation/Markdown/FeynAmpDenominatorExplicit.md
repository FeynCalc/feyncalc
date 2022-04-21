## FeynAmpDenominatorExplicit

`FeynAmpDenominatorExplicit[exp]` changes each occurrence of `PropagatorDenominator[a,b]` in exp into `1/(SPD[a,a]-b^2)` and replaces `FeynAmpDenominator` by `Identity`.

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

Notice that you should never apply `FeynAmpDenominatorExplicit` to loop integrals. Denominators in a proper loop integral should be written as `FeynAmpDenominator`s. Otherwise, the given integral is assumed to have no denominators and consequently set to zero as being scaleless.

```mathematica
TID[FVD[q, mu] FAD[{q, m}, {q - p, 0}], q]
```

$$\frac{\left(m^2+p^2\right) p^{\text{mu}}}{2 p^2 q^2.\left((q-p)^2-m^2\right)}-\frac{p^{\text{mu}}}{2 p^2 \left(q^2-m^2\right)}$$

```mathematica
TID[FeynAmpDenominatorExplicit[FVD[q, mu] FAD[{q, m}, {q - p, 0}]], q]
```

$$0$$