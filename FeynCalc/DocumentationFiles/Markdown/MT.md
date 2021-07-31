`MT[mu, nu]` is the metric tensor in $4$ dimensions.

### See also

[FeynCalcExternal](FeynCalcExternal), [FCE](FCE), [FCI](FCI), [MTD](MTD), [MTE](MTE).

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