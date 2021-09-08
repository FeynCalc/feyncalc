## CartesianPair

`CartesianPair[a, b]` is a special pairing used in the internal representation. `a` and `b` may have heads `CartesianIndex` or `CartesianMomentum`. If both `a` and `b` have head `CartesianIndex`, the Kronecker delta is understood. If `a` and `b` have head `CartesianMomentum`, a Cartesian scalar product is meant. If one of `a` and `b` has head `CartesianIndex` and the other `CartesianMomentum`, a Cartesian vector $p^i$ is understood.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [TemporalPair](TemporalPair.md).

### Examples

This represents a three-dimensional Kronecker delta

```mathematica
CartesianPair[CartesianIndex[i], CartesianIndex[j]]
```

$$\bar{\delta }^{ij}$$

This is a $D-1$-dimensional Kronecker delta

```mathematica
CartesianPair[CartesianIndex[i, D - 1], CartesianIndex[j, D - 1]]
```

$$\delta ^{ij}$$

If the Cartesian indices live in different dimensions, this gets resolved according to the t'Hoft-Veltman-Breitenlohner-Maison prescription

```mathematica
CartesianPair[CartesianIndex[i, D - 1], CartesianIndex[j]]
```

$$\bar{\delta }^{ij}$$

```mathematica
CartesianPair[CartesianIndex[i, D - 1], CartesianIndex[j, D - 4]]
```

$$\hat{\delta }^{ij}$$

```mathematica
CartesianPair[CartesianIndex[i], CartesianIndex[j, D - 4]]
```

$$0$$

A $3$-dimensional Cartesian vector

```mathematica
CartesianPair[CartesianIndex[i], CartesianMomentum[p]]
```

$$\overline{p}^i$$

A $D-1$-dimensional Cartesian vector

```mathematica
CartesianPair[CartesianIndex[i, D - 1], CartesianMomentum[p, D - 1]]
```

$$p^i$$

$3$-dimensional scalar products of Cartesian vectors

```mathematica
CartesianPair[CartesianMomentum[q], CartesianMomentum[p]]
```

$$\overline{p}\cdot \overline{q}$$

```mathematica
CartesianPair[CartesianMomentum[p], CartesianMomentum[p]]
```

$$\overline{p}^2$$

```mathematica
CartesianPair[CartesianMomentum[p - q], CartesianMomentum[p]]
```

$$\overline{p}\cdot (\overline{p}-\overline{q})$$

```mathematica
CartesianPair[CartesianMomentum[p], CartesianMomentum[p]]^2
```

$$\overline{p}^4$$

```mathematica
CartesianPair[CartesianMomentum[p], CartesianMomentum[p]]^3
```

$$\overline{p}^6$$

```mathematica
ExpandScalarProduct[CartesianPair[CartesianMomentum[p - q], CartesianMomentum[p]]]
```

$$\overline{p}^2-\overline{p}\cdot \overline{q}$$

```mathematica
CartesianPair[CartesianMomentum[-q], CartesianMomentum[p]] + CartesianPair[CartesianMomentum[q], CartesianMomentum[p]]
```

$$0$$
