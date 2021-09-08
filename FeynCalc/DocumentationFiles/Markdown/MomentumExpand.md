## MomentumExpand

`MomentumExpand[expr]` expands `Momentum[a+b+ ...]` in `expr` into `Momentum[a] + Momentum[b] + ...`.

### See also

[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [MomentumCombine](MomentumCombine.md).

### Examples

```mathematica
MomentumExpand[Momentum[p + q]] // StandardForm
ScalarProduct[p + q, r]
% // StandardForm

(*Momentum[p] + Momentum[q]*)
```

$$(\overline{p}+\overline{q})\cdot \overline{r}$$

```
(*Pair[Momentum[p + q], Momentum[r]]*)
```

```mathematica
MomentumExpand[ScalarProduct[p + q, r]]
% // StandardForm
```

$$(\overline{p}+\overline{q})\cdot \overline{r}$$

```
(*Pair[Momentum[p] + Momentum[q], Momentum[r]]*)
```

```mathematica
MomentumExpand[ScalarProduct[p + q, r - p]]
% // StandardForm 
  
 

```

$$(\overline{p}+\overline{q})\cdot (\overline{r}-\overline{p})$$

```
(*Pair[Momentum[p] + Momentum[q], -Momentum[p] + Momentum[r]]*)
```
