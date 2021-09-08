## FV

`FV[p, mu]` is the $4$-dimensional vector $p^{\mu }$.

### See also

[Overview](Extra/FeynCalc.md), [FCE](FCE.md), [FCI](FCI.md), [FVD](FVD.md), [Pair](Pair.md).

### Examples

```mathematica
FV[p, \[Mu]]
```

$$\overline{p}^{\mu }$$

```mathematica
FV[p - q, \[Mu]]
```

$$\left(\overline{p}-\overline{q}\right)^{\mu }$$

```mathematica
FV[p, \[Mu]] // StandardForm

(*FV[p, \[Mu]]*)
```

```mathematica
FCI[FV[p, \[Mu]]] // StandardForm

(*Pair[LorentzIndex[\[Mu]], Momentum[p]]*)
```

`ExpandScalarProduct` is used to expand momenta in `FV`

```mathematica
ExpandScalarProduct[FV[p - q, \[Mu]]]
```

$$\overline{p}^{\mu }-\overline{q}^{\mu }$$
