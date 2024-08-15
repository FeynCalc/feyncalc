## MomentumCombine

`MomentumCombine[expr]` is the inverse operation to `MomentumExpand` and `ExpandScalarProduct`. `MomentumCombine` combines also `Pair`s.
Notice, that `MomentumCombine` cannot complete squares. It can, however, bring expressions containing scalar products to a suitable form that allows for a square completion using other means.

This function offers multiple options.

The option `NumberQ` (default is `True`) specifies whether one should only merge quantities with numerical prefactors or not. Setting it to `False` allows for symbolic prefactors.

Setting the option `"Quadratic"` to `False` (default is `True`) effectively means that momenta squared will not be combined with anything else.

With the option `"ExcludeScalarProducts"` we can ensure that scalar products containing any of the momenta listed are not merged with anything else. So `a.x + a.y` can be merged either if `a` contains no such momenta, or if both `x` and `y` are free of them.

The option `Except` forbids merging the listed momenta with anything else. It is much more restrictive than `"ExcludeScalarProducts"` that allows for merging terms linear in the listed momenta.

The option `Select` allows for gathering all terms linear in the given momenta before applying any other combining rules.

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

Suppose that we have an expression that can be written as a square. To achieve the desired combination of momenta we need to

```mathematica
(DataType[#, FCVariable] = True) & /@ {gkin, meta, u0b};
```

```mathematica
ex = SPD[k1, k1] - 2 SPD[k1, k2] + 2 gkin meta SPD[k1, n] - 2 gkin meta u0b SPD[k1, n] - meta u0b SPD[k1, nb] + 
   SPD[k2, k2] - 2 gkin meta SPD[k2, n] + 2 gkin meta u0b SPD[k2, n] +meta u0b SPD[k2, nb]
```

$$-2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)+2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k2}\cdot n)-2 \;\text{gkin} \;\text{meta} (\text{k2}\cdot n)-2 (\text{k1}\cdot \;\text{k2})-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})+\text{k1}^2+\text{meta} \;\text{u0b} (\text{k2}\cdot \;\text{nb})+\text{k2}^2$$

The naive application of `MomentumCombine` doesn't return anything useful

```mathematica
MomentumCombine[ex]
```

$$\text{meta} \;\text{u0b} ((\text{k2}-\text{k1})\cdot \;\text{nb})+\text{k1}\cdot (\text{k1}-2 \;\text{k2})-2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)+2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k2}\cdot n)-2 \;\text{gkin} \;\text{meta} (\text{k2}\cdot n)+\text{k2}^2$$

Here we actually want to gather terms linear in `k1` and `k2`first before trying to combine them together. To that aim we can use the option `Select`.
Employing the options `"Quadratic"` and `"ExcludeScalarProducts"` we can prevent `k1` and `k2` from getting combined with anything containing
those momenta. Furthermore, we enable symbolical prefactor by setting `NumberQ` to false

```mathematica
MomentumCombine[ex, Select -> {k1, k2}, "Quadratic" -> False, "ExcludeScalarProducts" -> {k1, k2}, NumberQ -> False]
```

$$\text{k1}\cdot (-2 \;\text{gkin} \;\text{meta} n \;\text{u0b}+2 \;\text{gkin} \;\text{meta} n-\text{meta} \;\text{nb} \;\text{u0b})+\text{k2}\cdot (2 \;\text{gkin} \;\text{meta} n \;\text{u0b}-2 \;\text{gkin} \;\text{meta} n+\text{meta} \;\text{nb} \;\text{u0b})-2 (\text{k1}\cdot \;\text{k2})+\text{k1}^2+\text{k2}^2$$

This result looks very good, but `k1` and `k2` were not combined because they are contracted to long linear combinations of 4-momenta that were not properly factorized. The option `Factoring` solves this issue

```mathematica
res = MomentumCombine[ex, Select -> {k1, k2}, "Quadratic" -> False, "ExcludeScalarProducts" -> {k1, k2}, NumberQ -> False, Factoring -> Factor2]
```

$$\text{meta} ((\text{k1}-\text{k2})\cdot (-2 \;\text{gkin} n \;\text{u0b}+2 \;\text{gkin} n-\text{nb} \;\text{u0b}))-2 (\text{k1}\cdot \;\text{k2})+\text{k1}^2+\text{k2}^2$$