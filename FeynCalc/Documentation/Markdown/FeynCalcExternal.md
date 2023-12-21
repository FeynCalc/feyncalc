## FeynCalcExternal

`FeynCalcExternal[exp]` translates exp from the internal FeynCalc representation to a shorthand form.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcInternal](FeynCalcInternal.md).

### Examples

```mathematica
FeynCalcExternal[DiracGamma[5]]
```

$$\bar{\gamma }^5$$

```mathematica
FeynCalcExternal[DiracGamma[5]] // StandardForm

(*GA[5]*)
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
ex // FeynCalcInternal
```

$$\left\{\bar{\gamma }^{\mu },\gamma ^{\rho },\bar{\gamma }\cdot \overline{p},\overline{p}\cdot \overline{q},\bar{g}^{\alpha \beta },\overline{p}^{\mu }\right\}$$

```mathematica
ex // FeynCalcInternal // StandardForm

(*{DiracGamma[LorentzIndex[\[Mu]]], DiracGamma[LorentzIndex[\[Rho], D], D], DiracGamma[Momentum[p]], Pair[Momentum[p], Momentum[q]], Pair[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]], Pair[LorentzIndex[\[Mu]], Momentum[p]]}*)
```

```mathematica
ex // FeynCalcInternal // FeynCalcExternal // StandardForm

(*{GA[\[Mu]], GAD[\[Rho]], GS[p], SP[p, q], MT[\[Alpha], \[Beta]], FV[p, \[Mu]]}*)
```