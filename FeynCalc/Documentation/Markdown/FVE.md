## FVE

`FVE[p, mu]` is the $D-4$-dimensional vector $p$ with Lorentz index $\mu$.

### See also

[Overview](Extra/FeynCalc.md), [FCE](FCE.md), [FCI](FCI.md), [FV](FV.md), [FVD](FVD.md), [Pair](Pair.md).

### Examples

```mathematica
FVE[p, \[Mu]]
```

$$\hat{p}^{\mu }$$

```mathematica
FVE[p - q, \[Mu]]
```

$$\left(\hat{p}-\hat{q}\right)^{\mu }$$

```mathematica
FVE[p, \[Mu]] // StandardForm

(*FVE[p, \[Mu]]*)
```

```mathematica
FCI[FVE[p, \[Mu]]] // StandardForm

(*Pair[LorentzIndex[\[Mu], -4 + D], Momentum[p, -4 + D]]*)
```

There is no special function to expand momenta in `FVE`.

```mathematica
ExpandScalarProduct[FVE[p - q, \[Mu]]]
StandardForm[%]
```

$$\hat{p}^{\mu }-\hat{q}^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu], -4 + D], Momentum[p, -4 + D]] - Pair[LorentzIndex[\[Mu], -4 + D], Momentum[q, -4 + D]]*)
```

```mathematica
Contract[FVE[p, \[Mu]] FV[q, \[Mu]]]
```

$$0$$
