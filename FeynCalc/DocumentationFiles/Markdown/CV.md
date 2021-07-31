`CV[p, i]` is a 3-dimensional Cartesian vector and is transformed into `CartesianPair[CartesianMomentum[p], CartesianIndex[i]]` by `FeynCalcInternal`.

### See also

[FV](FV), [Pair](Pair), [CartesianPair](CartesianPair).

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