## FCE

`FCE[exp]` translates `exp` from the internal FeynCalc representation to a short form.

`FCE` is equivalent to `FeynCalcExternal`.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCI](FCI.md), [FeynCalcInternal](FeynCalcInternal.md).

### Examples

```mathematica
ex = FCE[{DiracGamma[5], DiracGamma[Momentum[p]]}]
```

$$\left\{\bar{\gamma }^5,\bar{\gamma }\cdot \overline{p}\right\}$$

```mathematica
ex // StandardForm

(*{GA[5], GS[p]}*)
```

```mathematica
ex = {GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```mathematica
ex // StandardForm

(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```

```mathematica
ex // FCI // StandardForm

(*{DiracGamma[LorentzIndex[\[Mu]]], DiracGamma[LorentzIndex[\[Rho], D], D], DiracGamma[Momentum[p]], Pair[Momentum[p], Momentum[q]], Pair[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]], Pair[LorentzIndex[\[Mu]], Momentum[p]]}*)
```

```mathematica
FCE[ex] // StandardForm

(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```