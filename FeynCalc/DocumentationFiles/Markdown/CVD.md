`CVD[p, i]` is a $D-1$-dimensional Cartesian vector and is transformed into `CartesianPair[CartesianMomentum[p,D], CartesianIndex[i,D]]` by `FeynCalcInternal`.

### See also

[FVD](FVD), [Pair](Pair), [CartesianPair](CartesianPair).

### Examples

```mathematica
CVD[p, i]
```

$$p^i$$

```mathematica
CVD[p - q, i]
```

$$(p-q)^i$$

```mathematica
FCI[CVD[p, i]] // StandardForm

(*CartesianPair[CartesianIndex[i, -1 + D], CartesianMomentum[p, -1 + D]]*)
```

`ExpandScalarProduct` is used to expand momenta in `CVD`

```mathematica
ExpandScalarProduct[CVD[p - q, i]]
```

$$p^i-q^i$$