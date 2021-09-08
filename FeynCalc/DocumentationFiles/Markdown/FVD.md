## FVD

`FVD[p, mu]` is the $D$-dimensional vector $p$ with Lorentz index `mu`.

### See also

[Overview](Extra/FeynCalc.md), [FCE](FCE.md), [FCI](FCI.md), [FV](FV.md), [Pair](Pair.md).

### Examples

```mathematica
FVD[p, \[Mu]]
```

$$p^{\mu }$$

```mathematica
FVD[p - q, \[Mu]]
```

$$(p-q)^{\mu }$$

```mathematica
FVD[p, \[Mu]] // StandardForm

(*FVD[p, \[Mu]]*)
```

```mathematica
FCI[FVD[p, \[Mu]]] // StandardForm

(*Pair[LorentzIndex[\[Mu], D], Momentum[p, D]]*)
```

There is no special function to expand momenta in `FVD`.

```mathematica
ExpandScalarProduct[FVD[p - q, \[Mu]]]
StandardForm[%]
```

$$p^{\mu }-q^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu], D], Momentum[p, D]] - Pair[LorentzIndex[\[Mu], D], Momentum[q, D]]*)
```
