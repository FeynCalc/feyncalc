## CVE

`CVE[p, i]` is a $D-4$-dimensional Cartesian vector and is transformed into `CartesianPair[CartesianMomentum[p,D-4], CartesianIndex[i,D-4]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [FVE](FVE.md), [Pair](Pair.md), [CartesianPair](CartesianPair.md).

### Examples

```mathematica
CVE[p, i]
```

$$\hat{p}^i$$

```mathematica
CVE[p - q, i]
```

$$\left(\hat{p}-\hat{q}\right)^i$$

```mathematica
FCI[CVE[p, i]] // StandardForm

(*CartesianPair[CartesianIndex[i, -4 + D], CartesianMomentum[p, -4 + D]]*)
```

`ExpandScalarProduct` is used to expand momenta in `CVE`

```mathematica
ExpandScalarProduct[CVE[p - q, i]]
```

$$\hat{p}^i-\hat{q}^i$$
