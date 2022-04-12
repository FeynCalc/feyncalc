## OneLoop

`OneLoop[q, amplitude]` calculates the 1-loop Feynman diagram amplitude. The argument `q` denotes the integration variable, i.e., the loop momentum. `OneLoop[name, q, amplitude]` has as first argument a name of the amplitude. If the second argument has head `FeynAmp` then `OneLoop[q, FeynAmp[name, k, expr]]` and `OneLoop[FeynAmp[name, k, expr]]` tranform to `OneLoop[name, k, expr]`. `OneLoop` is deprecated, please use `TID` instead!

### See also

[Overview](Extra/FeynCalc.md), [ToPaVe](ToPaVe.md), [ToPaVe2](ToPaVe2.md), [A0](A0.md), [A00](A00.md), [B0](B0.md), [B1](B1.md), [B00](B00.md), [B11](B11.md), [C0](C0.md), [D0](D0.md).

### Examples

```mathematica
-I/Pi^2 FAD[{q, m}] 
 
OneLoop[q, %]
```

$$-\frac{i}{\pi ^2 \left(q^2-m^2\right)}$$

$$\text{A}_0\left(m^2\right)$$

```mathematica
I ((el^2)/(16 Pi^4 (1 - D))) FAD[{q, mf}, {q - k, mf}] DiracTrace[(mf + GSD[q - k]) . GAD[\[Mu]] . (mf + GSD[q]) . GAD[\[Mu]]] 
 
OneLoop[q, %]
```

$$\frac{i \;\text{el}^2 \;\text{tr}\left((\gamma \cdot (q-k)+\text{mf}).\gamma ^{\mu }.(\text{mf}+\gamma \cdot q).\gamma ^{\mu }\right)}{16 \pi ^4 (1-D) \left(q^2-\text{mf}^2\right).\left((q-k)^2-\text{mf}^2\right)}$$

$$\frac{\text{el}^2 \left(-\frac{8 \;\text{mf}^2 \;\text{B}_0\left(\overline{k}^2,\text{mf}^2,\text{mf}^2\right)}{1-D}+\frac{2 (2-D) \overline{k}^2 \;\text{B}_0\left(\overline{k}^2,\text{mf}^2,\text{mf}^2\right)}{1-D}+\frac{4 D \;\text{A}_0\left(\text{mf}^2\right)}{1-D}-\frac{8 \;\text{A}_0\left(\text{mf}^2\right)}{1-D}\right)}{16 \pi ^2}$$