## MomentumExpand

`MomentumExpand[expr]` expands `Momentum[a+b+ ...]` in `expr` into `Momentum[a] + Momentum[b] + ...`.

### See also

[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [MomentumCombine](MomentumCombine.md).

### Examples

```mathematica
MomentumExpand[Momentum[p + q]] // StandardForm

(*Momentum[p] + Momentum[q]*)
```

```mathematica
ScalarProduct[p + q, r]
```

$$(\overline{p}+\overline{q})\cdot \overline{r}$$

```mathematica
ScalarProduct[p + q, r] // StandardForm

(*Pair[Momentum[p + q], Momentum[r]]*)
```

```mathematica
ex = MomentumExpand[ScalarProduct[p + q, r]]
```

$$(\overline{p}+\overline{q})\cdot \overline{r}$$

```mathematica
ex // StandardForm

(*Pair[Momentum[p] + Momentum[q], Momentum[r]]*)
```

```mathematica
ex = MomentumExpand[ScalarProduct[p + q, r - p]]
```

$$(\overline{p}+\overline{q})\cdot (\overline{r}-\overline{p})$$

```mathematica
ex // StandardForm

(*Pair[Momentum[p] + Momentum[q], -Momentum[p] + Momentum[r]]*)
```