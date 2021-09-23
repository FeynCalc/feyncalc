## MT

`MT[mu, nu]` is the metric tensor in $4$ dimensions.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCE](FCE.md), [FCI](FCI.md), [MTD](MTD.md), [MTE](MTE.md).

### Examples

```mathematica
MT[\[Alpha], \[Beta]]
```

$$\bar{g}^{\alpha \beta }$$

```mathematica
Contract[MT[\[Alpha], \[Beta]] MT[\[Alpha], \[Beta]]]
```

$$4$$

```mathematica
MT[a, b] // StandardForm

(*MT[a, b]*)
```

```mathematica
FCI[MT[a, b]] // StandardForm

(*Pair[LorentzIndex[a], LorentzIndex[b]]*)
```

```mathematica
FCE[FCI[MT[a, b]]] // StandardForm

(*MT[a, b]*)
```
