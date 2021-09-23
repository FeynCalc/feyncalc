## PairContract

`PairContract` is like `Pair`, but with (local) contraction properties.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [Contract](Contract.md).

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
