## MomentumCombine

`MomentumCombine[expr]` is the inverse operation to `MomentumExpand` and `ExpandScalarProduct`. `MomentumCombine` combines also `Pair`s.

### See also

[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [Momentum](Momentum.md), [MomentumExpand](MomentumExpand.md).

### Examples

```mathematica
Momentum[p] - 2 Momentum[q] // MomentumCombine // StandardForm

(*Momentum[p - 2 q]*)
```

```mathematica
FV[p, \[Mu]] + 2 FV[q, \[Mu]] 
 
ex = MomentumCombine[%]
```

$$\overline{p}^{\mu }+2 \overline{q}^{\mu }$$

$$\left(\overline{p}+2 \overline{q}\right)^{\mu }$$

```mathematica
ex // StandardForm

(*Pair[LorentzIndex[\[Mu]], Momentum[p + 2 q]]*)
```

```mathematica
ex // ExpandScalarProduct
```

$$\overline{p}^{\mu }+2 \overline{q}^{\mu }$$

```mathematica
3 Pair[LorentzIndex[\[Mu]], Momentum[p]] + 2 Pair[LorentzIndex[\[Mu]], Momentum[q]] 
 
ex = MomentumCombine[%]
```

$$3 \overline{p}^{\mu }+2 \overline{q}^{\mu }$$

$$\left(3 \overline{p}+2 \overline{q}\right)^{\mu }$$

```mathematica
ex // StandardForm

(*Pair[LorentzIndex[\[Mu]], Momentum[3 p + 2 q]]*)
```