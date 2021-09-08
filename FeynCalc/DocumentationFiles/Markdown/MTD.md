## MTD

`MTD[mu, nu]` is the metric tensor in $D$ dimensions.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCE](FCE.md), [FCI](FCI.md), [MT](MT.md), [MTE](MTE.md).

### Examples

```mathematica
MTD[\[Alpha], \[Beta]]
```

$$g^{\alpha \beta }$$

```mathematica
Contract[MTD[\[Alpha], \[Beta]] MTD[\[Alpha], \[Beta]]]
```

$$D$$

```mathematica
MTD[\[Alpha], \[Beta]] // StandardForm

(*MTD[\[Alpha], \[Beta]]*)
```

```mathematica
FCI[MTD[\[Alpha], \[Beta]]] // StandardForm

(*Pair[LorentzIndex[\[Alpha], D], LorentzIndex[\[Beta], D]]*)
```

```mathematica
FCE[FCI[MTD[\[Mu], \[Nu]]]] // StandardForm

(*MTD[\[Mu], \[Nu]]*)
```
