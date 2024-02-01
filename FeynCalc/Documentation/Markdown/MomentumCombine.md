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

In some cases one might need a better control over the types of expressions getting combined. For example, the following
expression will not be combined by default, since the coefficients of scalar products are not numbers

```mathematica
DataType[a1, FCVariable] = True;
DataType[a2, FCVariable] = True;
```

```mathematica
ex = SPD[a1 p, n] + SPD[a2 p, nb]
```

$$\text{a1} (n\cdot p)+\text{a2} (\text{nb}\cdot p)$$

```mathematica
MomentumCombine[ex]
```

$$\text{a1} (n\cdot p)+\text{a2} (\text{nb}\cdot p)$$

Setting the option `NumberQ` to `False` we can still achieve the desired form

```mathematica
MomentumCombine[ex, NumberQ -> False]
```

$$(\text{a1} n+\text{a2} \;\text{nb})\cdot p$$

However, in the following case combing $p^2$ with the other two scalar products is not useful

```mathematica
ex = SPD[p] + SPD[a1 p, n] + SPD[a2 p, nb]
```

$$\text{a1} (n\cdot p)+\text{a2} (\text{nb}\cdot p)+p^2$$

```mathematica
MomentumCombine[ex, NumberQ -> False]
```

$$p\cdot (\text{a1} n+\text{a2} \;\text{nb}+p)$$

To prevent this from happening there is a somewhat hidden option `"Quadratic"` that can be set to `False`

```mathematica
MomentumCombine[ex, NumberQ -> False, "Quadratic" -> False]
```

$$(\text{a1} n+\text{a2} \;\text{nb})\cdot p+p^2$$

```mathematica
ex = SPD[p] + SPD[a1 p, n] + SPD[a2 p, nb] + SPD[p, l] + SPD[p, k]
```

$$\text{a1} (n\cdot p)+\text{a2} (\text{nb}\cdot p)+k\cdot p+l\cdot p+p^2$$

In this case we we would like to prevent the scalar products involving `l` and `k` from being combined with
the rest. To that end we need to use the option `Except`

```mathematica
MomentumCombine[ex, NumberQ -> False, "Quadratic" -> False, Except -> {k, l}]
```

$$(\text{a1} n+\text{a2} \;\text{nb})\cdot p+k\cdot p+l\cdot p+p^2$$