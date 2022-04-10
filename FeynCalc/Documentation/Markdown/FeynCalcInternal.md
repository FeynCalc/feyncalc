## FeynCalcInternal

`FeynCalcInternal[exp]` translates `exp` into the internal FeynCalc (abstract data-type) representation.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCI](FCI.md), [FCE](FCE.md).

### Examples

```mathematica
ex = {GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```mathematica
ex // StandardForm

(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```

```mathematica
ex // FeynCalcInternal
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```mathematica
ex // StandardForm

(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```

```mathematica
FeynCalcExternal[ex] // StandardForm

(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```

```mathematica
ex = FCI[{SD[a, b], SUND[a, b, c], SUNF[a, b, c], FAD[q], LC[\[Mu], \[Nu], \[Rho], \[Sigma]]}]
```

$$\left\{\delta ^{ab},d^{abc},f^{abc},\frac{1}{q^2},\bar{\epsilon }^{\mu \nu \rho \sigma }\right\}$$

```mathematica
ex // StandardForm

(*{SUNDelta[SUNIndex[a], SUNIndex[b]], SUND[SUNIndex[a], SUNIndex[b], SUNIndex[c]], SUNF[SUNIndex[a], SUNIndex[b], SUNIndex[c]], FeynAmpDenominator[PropagatorDenominator[Momentum[q, D], 0]], Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]}*)
```