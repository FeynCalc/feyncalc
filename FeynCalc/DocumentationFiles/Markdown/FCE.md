##  FCE 

FCE[exp] translates exp from the internal FeynCalc representation to a short form.FCE is equivalent to FeynCalcExternal..

###  See also 

FeynCalcExternal, FCI, FeynCalcInternal.

###  Examples 

```mathematica
FCE[{DiracGamma[5], DiracGamma[Momentum[p]]}] 
 
% // StandardForm 
 
{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]} 
 
% // StandardForm 
 
% // FCI 
 
% // StandardForm 
 
FCE[%] // StandardForm
```

$$\left\{\bar{\gamma }^5,\bar{\gamma }\cdot \overline{p}\right\}$$

```
(*{GA[5], GS[p]}*)
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