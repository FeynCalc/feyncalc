##  FeynCalcInternal 

FeynCalcInternal[exp] translates exp into the internal FeynCalc (abstract data-type) representation..

###  See also 

FeynCalcExternal, FCI, FCE.

###  Examples 

```mathematica
{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]} 
 
% // StandardForm 
 
% // FeynCalcInternal 
 
% // StandardForm 
 
FeynCalcExternal[%] // StandardForm 
 
FCI[{SD[a, b], SUND[a, b, c], SUNF[a, b, c], FAD[q], LC[\[Mu], \[Nu], \[Rho], \[Sigma]]}] 
 
% // StandardForm
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```
(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```
(*{DiracGamma[LorentzIndex[\[Mu]]], DiracGamma[LorentzIndex[\[Rho], D], D], DiracGamma[Momentum[p]], Pair[Momentum[p], Momentum[q]], Pair[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]], Pair[LorentzIndex[\[Mu]], Momentum[p]]}*)

(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```

$$\left\{\delta ^{ab},d^{abc},f^{abc},\frac{1}{q^2},\bar{\epsilon }^{\mu \nu \rho \sigma }\right\}$$

```
(*{SUNDelta[SUNIndex[a], SUNIndex[b]], SUND[SUNIndex[a], SUNIndex[b], SUNIndex[c]], SUNF[SUNIndex[a], SUNIndex[b], SUNIndex[c]], FeynAmpDenominator[PropagatorDenominator[Momentum[q, D], 0]], Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]}*)
```