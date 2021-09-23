## FeynCalcInternal

`FeynCalcInternal[exp]` translates `exp` into the internal FeynCalc (abstract data-type) representation.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCI](FCI.md), [FCE](FCE.md).

### Examples

```mathematica
ex = {GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}
% // StandardForm
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```
(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```

```mathematica
ex // FeynCalcInternal
% // StandardForm
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```
(*{DiracGamma[LorentzIndex[\[Mu]]], DiracGamma[LorentzIndex[\[Rho], D], D], DiracGamma[Momentum[p]], Pair[Momentum[p], Momentum[q]], Pair[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]], Pair[LorentzIndex[\[Mu]], Momentum[p]]}*)
```

```mathematica
FeynCalcExternal[ex] // StandardForm

(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```

```mathematica
FCI[{SD[a, b], SUND[a, b, c], SUNF[a, b, c], FAD[q], LC[\[Mu], \[Nu], \[Rho], \[Sigma]]}]
% // StandardForm 
  
 

```

$$\left\{\delta ^{ab},d^{abc},f^{abc},\frac{1}{q^2},\bar{\epsilon }^{\mu \nu \rho \sigma }\right\}$$

```
(*{SUNDelta[SUNIndex[a], SUNIndex[b]], SUND[SUNIndex[a], SUNIndex[b], SUNIndex[c]], SUNF[SUNIndex[a], SUNIndex[b], SUNIndex[c]], FeynAmpDenominator[PropagatorDenominator[Momentum[q, D], 0]], Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]}*)
```
