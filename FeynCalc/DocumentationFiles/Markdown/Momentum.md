## Momentum

`Momentum[p]` is the head of a four momentum `p`.

The internal representation of a $4$-dimensional $p$ is `Momentum[p]`.

For other than $4$ dimensions: `Momentum[p, dim]`.

`Momentum[p, 4]` simplifies to `Momentum[p]`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [Eps](Eps.md), [LorentzIndex](LorentzIndex.md), [MomentumExpand](MomentumExpand.md).

### Examples

This is a $4$-dimensional momentum.

```mathematica
Momentum[p]
```

$$\overline{p}$$

As an optional second argument the dimension must be specified if it is different from $4$.

```mathematica
Momentum[p, D]
```

$$p$$

The dimension index is suppressed in the output.

```mathematica
Momentum[p, d]
```

$$p$$

```mathematica
Momentum[-q]
% // StandardForm
```

$$-\overline{q}$$

```
(*-Momentum[q]*)
```

```mathematica
ex = Momentum[p - q] + Momentum[2 q]
% // StandardForm
```

$$\left(\overline{p}-\overline{q}\right)+2 \overline{q}$$

```
(*Momentum[p - q] + 2 Momentum[q]*)
```

```mathematica
ex // MomentumExpand // StandardForm

(*Momentum[p] + Momentum[q]*)
```

```mathematica
ex // MomentumCombine // StandardForm

(*Momentum[p + q]*)
```

```mathematica
ChangeDimension[Momentum[p], d] // StandardForm

(*Momentum[p, d]*)
```
