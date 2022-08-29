## PairContract3

`PairContract3` is like `Pair`, but with local contraction properties among `PairContract3`s. The function fully supports the BMHV algebra and, unlike `PairContract` or `PairContract2` will always expand momenta inside scalar products.

`PairContract3` is an auxiliary function used in higher level FeynCalc functions that require fast contractions between multiple expressions, where `Contract` would be too slow.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [PairContract](PairContract.md), [PairContract2](PairContract2.md).

### Examples

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p]] Pair[LorentzIndex[\[Mu]], Momentum[q]] 
 
% /. Pair -> PairContract3
```

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p]] Pair[LorentzIndex[\[Nu]], Momentum[q]] Pair[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]]] 
 
% /. Pair -> PairContract3
```

$$\overline{p}^{\mu } \overline{q}^{\nu } \bar{g}^{\mu \nu }$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p + q]] Pair[LorentzIndex[\[Mu]], Momentum[r + s]] 
 
% /. Pair -> PairContract3 
  
 

```

$$\left(\overline{p}+\overline{q}\right)^{\mu } \left(\overline{r}+\overline{s}\right)^{\mu }$$

$$\overline{p}\cdot \overline{r}+\overline{p}\cdot \overline{s}+\overline{q}\cdot \overline{r}+\overline{q}\cdot \overline{s}$$