## FourVector

`FourVector[p, mu]` is the $4$-dimensional vector `p` with Lorentz index `mu`.

A vector with space-time Dimension $D$ is obtained by supplying the option `Dimension -> D`.

The shortcut `FourVector` is deprecated, please use `FV` instead!

### See also

[Overview](Extra/FeynCalc.md), [FV](FV.md), [FCI](FCI.md).

### Examples

```mathematica
FourVector[p, \[Mu]]
```

$$\overline{p}^{\mu }$$

```mathematica
FourVector[p - q, \[Mu]]
```

$$\left(\overline{p}-\overline{q}\right)^{\mu }$$

```mathematica
StandardForm[FourVector[p, \[Mu]]]

(*Pair[LorentzIndex[\[Mu]], Momentum[p]]*)
```

```mathematica
StandardForm[FourVector[p, \[Mu], Dimension -> D]]

(*Pair[LorentzIndex[\[Mu], D], Momentum[p, D]]*)
```

`FourVector` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use `FV`.

```mathematica
FV[p, \[Mu]]
```

$$\overline{p}^{\mu }$$

```mathematica
FVD[p, \[Mu]]
```

$$p^{\mu }$$

```mathematica
FCI[FV[p, \[Mu]]] === FourVector[p, \[Mu]]
```

$$\text{True}$$

```mathematica
FCI[FVD[p, \[Mu]]] === FourVector[p, \[Mu], Dimension -> D]
```

$$\text{True}$$
