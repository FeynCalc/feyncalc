## CartesianMomentum

`CartesianMomentum[p]` is the head of a 3-momentum `p`. The internal representation of a $3$-dimensional `p` is `CartesianMomentum[p]`. For other than three dimensions: `CartesianMomentum[p, Dimension]`. `CartesianMomentum[p, 3]` simplifies to `CartesianMomentum[p]`.

### See also

[Overview](Extra/FeynCalc.md), [Momentum](Momentum.md), [TemporalMomentum](TemporalMomentum.md).

### Examples

This is a 3-dimensional momentum

```mathematica
CartesianMomentum[p]
```

$$\overline{p}$$

As an optional second argument the dimension must be specified if it is different from 3

```mathematica
CartesianMomentum[p, D - 1]
```

$$p$$

The dimension index is suppressed in the output.

```mathematica
CartesianMomentum[p, d - 1]
```

$$p$$

```mathematica
a1 = CartesianMomentum[-q]
a1 // StandardForm
```

$$-\overline{q}$$

```
(*-CartesianMomentum[q]*)
```

```mathematica
a2 = CartesianMomentum[p - q] + CartesianMomentum[2 q]
a2 // StandardForm
```

$$\left(\overline{p}-\overline{q}\right)+2 \overline{q}$$

```
(*CartesianMomentum[p - q] + 2 CartesianMomentum[q]*)
```

```mathematica
a2 // MomentumExpand // StandardForm

(*CartesianMomentum[p] + CartesianMomentum[q]*)
```

```mathematica
a2 // MomentumCombine // StandardForm

(*CartesianMomentum[p + q]*)
```

Notice that when changing the dimension, one must specify its value as if the the 3-vector were the spatial component of the corresponding 4-vector

```mathematica
ChangeDimension[CartesianMomentum[p], d - 1] // StandardForm

(*CartesianMomentum[p, -2 + d]*)
```

```mathematica
ChangeDimension[CartesianMomentum[p], d] // StandardForm

(*CartesianMomentum[p, -1 + d]*)
```

```mathematica
Clear[a1, a2]
```
