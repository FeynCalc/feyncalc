## FCI

`FCI[exp]` translates exp into the internal FeynCalc (datatype-)representation.

`FCI` is equivalent to `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FeynCalcInternal](FeynCalcInternal.md), [FCE](FCE.md).

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
ex // FCI
% // StandardForm
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```
(*{DiracGamma[LorentzIndex[\[Mu]]], DiracGamma[LorentzIndex[\[Rho], D], D], DiracGamma[Momentum[p]], Pair[Momentum[p], Momentum[q]], Pair[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]], Pair[LorentzIndex[\[Mu]], Momentum[p]]}*)
```

```mathematica
ex // FCE
% // StandardForm 
  
 

```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```
(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```
