## CV

`CV[p, i]` is a 3-dimensional Cartesian vector and is transformed into `CartesianPair[CartesianMomentum[p], CartesianIndex[i]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [FV](FV.md), [Pair](Pair.md), [CartesianPair](CartesianPair.md).

### Examples

```mathematica
CV[p, i]
```

$$\overline{p}^i$$

```mathematica
CV[p - q, i]
```

$$\left(\overline{p}-\overline{q}\right)^i$$

```mathematica
FCI[CV[p, i]] // StandardForm

(*CartesianPair[CartesianIndex[i], CartesianMomentum[p]]*)
```

`ExpandScalarProduct` is used to expand momenta in `CV`

```mathematica
ExpandScalarProduct[CV[p - q, i]]
```

$$\overline{p}^i-\overline{q}^i$$
