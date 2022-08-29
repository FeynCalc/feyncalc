## PairContract2

`PairContract2` is like `Pair`, but with local contraction properties. It works best with products of `Pair`s that are expected to evaluate to a product of scalar products.

- Suitable contractions between products of `PairContract2` symbols are evaluated immediately.
- Momenta are never expanded and every `PairContract2` symbol containing `Momentum` in both slots is immediately converted to a `Pair`.
- BMHV algebra is not supported, every tensor must be purely `4` or `D`-dimensional

`PairContract2` is an auxiliary function used in higher level FeynCalc functions that require fast contractions between multiple expressions, where `Contract` would be too slow.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [PairContract](PairContract.md), [PairContract3](PairContract3.md).

### Examples

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p]] Pair[LorentzIndex[\[Mu]], Momentum[q]] 
 
% /. Pair -> PairContract2
```

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p + q]] Pair[LorentzIndex[\[Mu]], Momentum[r + s]] 
 
% /. Pair -> PairContract2
```

$$\left(\overline{p}+\overline{q}\right)^{\mu } \left(\overline{r}+\overline{s}\right)^{\mu }$$

$$(\overline{p}+\overline{q})\cdot (\overline{r}+\overline{s})$$