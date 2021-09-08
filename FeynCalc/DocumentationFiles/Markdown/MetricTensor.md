## MetricTensor

`MetricTensor[mu, nu]` is the metric tensor. The default dimension is $4$.

The shortcut `MetricTensor` is deprecated, please use `MT` instead!

### See also

[Overview](Extra/FeynCalc.md), [FCI](FCI.md), [MT](MT.md), [MTD](MTD.md).

### Examples

```mathematica
MetricTensor[\[Alpha], \[Beta]]
Contract[% %]
```

$$\bar{g}^{\alpha \beta }$$

$$4$$

```mathematica
MetricTensor[\[Alpha], \[Beta], Dimension -> D]
Contract[% %]
```

$$g^{\alpha \beta }$$

$$D$$

```mathematica
StandardForm[MetricTensor[a, b]]

(*Pair[LorentzIndex[a], LorentzIndex[b]]*)
```

`MetricTensor` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use `MT`.

```mathematica
MT[\[Mu], \[Nu]]
```

$$\bar{g}^{\mu \nu }$$

```mathematica
MTD[\[Mu], \[Nu]]
```

$$g^{\mu \nu }$$

```mathematica
FCI[MT[\[Mu], \[Nu]]] === MetricTensor[\[Mu], \[Nu]]
FCI[MTD[\[Mu], \[Nu]]] === MetricTensor[\[Mu], \[Nu], Dimension -> D] 
  
 

```

$$\text{True}$$

$$\text{True}$$
