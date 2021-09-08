## TemporalPair

`TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]]` is a special pairing used in the internal representation to denote $p^0$, the temporal component of a 4-momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [TemporalPair](TemporalPair.md), [TC](TC.md), [ExplicitLorentzIndex](ExplicitLorentzIndex.md).

### Examples

```mathematica
TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]]
```

$$p^0$$

```mathematica
TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p + q]]
% // ExpandScalarProduct
```

$$(p+q)^0$$

$$p^0+q^0$$
