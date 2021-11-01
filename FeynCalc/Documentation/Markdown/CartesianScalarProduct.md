## CartesianScalarProduct

`CartesianScalarProduct[p, q]`  is the input for the scalar product of two Cartesian vectors `p` and `q`.

`CartesianScalarProduct[p]` is equivalent to `CartesianScalarProduct[p, p]`.

Expansion of sums of momenta in `CartesianScalarProduct` is done with `ExpandScalarProduct`.

Scalar products may be set, e.g. via `ScalarProduct[a, b] = m^2;` but `a` and `b` may not contain sums.

`CartesianScalarProduct[a] ` corresponds to `CartesianScalarProduct[a,a] `

Note that `ScalarProduct[a, b] = m^2` actually sets Cartesian scalar products in different dimensions specified by the value of the `SetDimensions` option.

It is highly recommended to set `ScalarProduct`s before any calculation. This improves the performance of FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [CSP](CSP.md), [CSPD](CSPD.md), [CSPE](CSPE.md).

### Examples

```mathematica
CartesianScalarProduct[p, q]
```

$$\overline{p}\cdot \overline{q}$$

```mathematica
CartesianScalarProduct[p + q, -q]
```

$$-\left(\overline{q}\cdot (\overline{p}+\overline{q})\right)$$

```mathematica
CartesianScalarProduct[p, p]
```

$$\overline{p}^2$$

```mathematica
CartesianScalarProduct[q]
```

$$\overline{q}^2$$

```mathematica
CartesianScalarProduct[p, q] // StandardForm

(*CartesianPair[CartesianMomentum[p], CartesianMomentum[q]]*)
```

```mathematica
CartesianScalarProduct[p, q, Dimension -> D - 1] // StandardForm

(*CartesianPair[CartesianMomentum[p, -1 + D], CartesianMomentum[q, -1 + D]]*)
```

```mathematica
CartesianScalarProduct[Subscript[p, 1], Subscript[p, 2]] = s/2
```

$$\frac{s}{2}$$

```mathematica
ExpandScalarProduct[ CartesianScalarProduct[Subscript[p, 1] - q, Subscript[p, 2] - k]]
```

$$-\overline{k}\cdot \overline{p}_1+\overline{k}\cdot \overline{q}-\overline{q}\cdot \overline{p}_2+\frac{s}{2}$$

```mathematica
Calc[ CartesianScalarProduct[Subscript[p, 1] - q, Subscript[p, 2] - k]]
```

$$-\overline{k}\cdot \overline{p}_1+\overline{k}\cdot \overline{q}-\overline{q}\cdot \overline{p}_2+\frac{s}{2}$$

```mathematica
CartesianScalarProduct[q1] = qq;
```

```mathematica
CSP[q1]
```

$$\text{qq}$$

```mathematica
FCClearScalarProducts[]
```