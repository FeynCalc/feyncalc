## MTE

`MTE[mu, nu]` is the metric tensor in $D-4$ dimensions.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCE](FCE.md), [FCI](FCI.md), [MT](MT.md), [MTD](MTD.md).

### Examples

```mathematica
MTE[\[Alpha], \[Beta]]
```

$$\hat{g}^{\alpha \beta }$$

```mathematica
Contract[MTE[\[Alpha], \[Beta]] MTE[\[Alpha], \[Beta]]]
```

$$D-4$$

```mathematica
Contract[MTE[\[Alpha], \[Beta]] MT[\[Alpha], \[Beta]]]
```

$$0$$

```mathematica
Contract[MTE[\[Alpha], \[Beta]] MTD[\[Alpha], \[Beta]]]
```

$$D-4$$

```mathematica
MTE[\[Alpha], \[Beta]] // StandardForm

(*MTE[\[Alpha], \[Beta]]*)
```

```mathematica
MTE[\[Alpha], \[Beta]]
```

$$\hat{g}^{\alpha \beta }$$

```mathematica
FCI[MTE[\[Alpha], \[Beta]]] // StandardForm

(*Pair[LorentzIndex[\[Alpha], -4 + D], LorentzIndex[\[Beta], -4 + D]]*)
```

```mathematica
FCE[FCI[MTE[\[Mu], \[Nu]]]] // StandardForm

(*MTE[\[Mu], \[Nu]]*)
```

```mathematica
MTE[\[Mu], \[Nu]]
```

$$\hat{g}^{\mu \nu }$$
