## DiracMatrix

`DiracMatrix[mu]` denotes a Dirac gamma matrix with Lorentz index $\mu$.

`DiracMatrix[mu , nu , ...]` is a product of $\gamma$ matrices with Lorentz indices `mu , nu , ...`

`DiracMatrix[5]` is $\gamma ^5$.

`DiracMatrix[6]` is $(1 + \gamma^5)/2$.

`DiracMatrix[7]` is $(1 - \gamma^5)/2$.

The shortcut `DiracMatrix` is deprecated, please use `GA` instead!

### See also

[Overview](Extra/FeynCalc.md), [GA](GA.md), [FCI](FCI.md).

### Examples

```mathematica
DiracMatrix[\[Mu]]
```

$$\bar{\gamma }^{\mu }$$

This is how to enter the non-commutative product of two. The Mathematica Dot "." is used as non-commutative multiplication operator.

```mathematica
DiracMatrix[\[Mu]] . DiracMatrix[\[Nu]]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

```mathematica
DiracMatrix[\[Alpha]] // StandardForm

(*DiracGamma[LorentzIndex[\[Alpha]]]*)
```

`DiracMatrix` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use `GA`.

```mathematica
GA[\[Mu]]
```

$$\bar{\gamma }^{\mu }$$

```mathematica
GAD[\[Mu]]
```

$$\gamma ^{\mu }$$

```mathematica
FCI[GA[\[Mu]]] === DiracMatrix[\[Mu]]
```

$$\text{True}$$

```mathematica
FCI[GAD[\[Mu]]] === DiracMatrix[\[Mu], Dimension -> D]
```

$$\text{True}$$
