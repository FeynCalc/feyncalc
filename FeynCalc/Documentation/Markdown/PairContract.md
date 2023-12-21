## PairContract

`PairContract` is like `Pair`, but with (local) contraction properties. The function fully supports the BMHV algebra and will expand momenta inside scalar products when it leads to simpler expressions.

`PairContract` is an auxiliary function used in higher level FeynCalc functions that require fast contractions between multiple expressions, where `Contract` would be too slow.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [PairContract2](PairContract2.md), [PairContract3](PairContract3.md).

### Examples

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p]] Pair[LorentzIndex[\[Mu]], Momentum[q]] 
 
% /. Pair -> PairContract
```

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p]] Pair[LorentzIndex[\[Nu]], Momentum[q]] Pair[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]]] 
 
% /. Pair -> PairContract
```

$$\overline{p}^{\mu } \overline{q}^{\nu } \bar{g}^{\mu \nu }$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p + q]] Pair[LorentzIndex[\[Mu]], Momentum[r + s]] 
 
% /. Pair -> PairContract
```

$$\left(\overline{p}+\overline{q}\right)^{\mu } \left(\overline{r}+\overline{s}\right)^{\mu }$$

$$(\overline{p}+\overline{q})\cdot (\overline{r}+\overline{s})$$

```mathematica
FCClearScalarProducts[];
SP[p1, p2] = s2;
SP[p1, p3] = s3;
FCI[SP[p1, p2 + p3]] /. Pair -> PairContract
```

$$\text{s2}+\text{s3}$$