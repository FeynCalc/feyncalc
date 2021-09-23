## Pair

`Pair[x, y]` is the head of a special pairing used in the internal representation: `x` and `y` may have heads `LorentzIndex` or `Momentum`.

If both `x` and `y` have head `LorentzIndex`, the metric tensor (e.g. $g^{\mu \nu}$) is understood.

If `x` and `y` have head `Momentum`, a scalar product (e.g. $p \cdot q$) is meant.

If one of `x` and `y` has head `LorentzIndex` and the other `Momentum`, a Lorentz vector (e.g. $p^{\mu }$) is implied.

### See also

[Overview](Extra/FeynCalc.md), [FV](FV.md), [FVD](FVD.md), [MT](MT.md), [MTD](MTD.md), [ScalarProduct](ScalarProduct.md), [SP](SP.md), [SPD](SPD.md).

### Examples

This represents a $4$-dimensional metric tensor

```mathematica
Pair[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]]
```

$$\bar{g}^{\alpha \beta }$$

This is a D-dimensional metric tensor

```mathematica
Pair[LorentzIndex[\[Alpha], D], LorentzIndex[\[Beta], D]]
```

$$g^{\alpha \beta }$$

If the Lorentz indices live in different dimensions, this gets resolved according to the t'Hooft-Veltman-Breitenlohner-Maison prescription

```mathematica
Pair[LorentzIndex[\[Alpha], n - 4], LorentzIndex[\[Beta]]]
```

$$0$$

A $4$-dimensional Lorentz vector

```mathematica
Pair[LorentzIndex[\[Alpha]], Momentum[p]]
```

$$\overline{p}^{\alpha }$$

A $D$-dimensional Lorentz vector

```mathematica
Pair[LorentzIndex[\[Alpha], D], Momentum[p, D]]
```

$$p^{\alpha }$$

$4$-dimensional scalar products of Lorentz vectors

```mathematica
Pair[Momentum[q], Momentum[p]]
```

$$\overline{p}\cdot \overline{q}$$

```mathematica
Pair[Momentum[p], Momentum[p]]
```

$$\overline{p}^2$$

```mathematica
Pair[Momentum[p - q], Momentum[p]]
```

$$\overline{p}\cdot (\overline{p}-\overline{q})$$

```mathematica
Pair[Momentum[p], Momentum[p]]^2
```

$$\overline{p}^4$$

```mathematica
Pair[Momentum[p], Momentum[p]]^3
```

$$\overline{p}^6$$

```mathematica
ExpandScalarProduct[Pair[Momentum[p - q], Momentum[p]]]
```

$$\overline{p}^2-\overline{p}\cdot \overline{q}$$

```mathematica
Pair[Momentum[-q], Momentum[p]] + Pair[Momentum[q], Momentum[p]]
```

$$0$$
