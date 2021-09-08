## TC

`TC[p]` is the temporal component of a $4$-vector and is transformed into `TemporalPair[TemporalMomentum[p], ExplicitLorentzIndex[0]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [TemporalPair](TemporalPair.md), [TemporalMomentum](TemporalMomentum.md), [FCClearScalarProducts](FCClearScalarProducts.md).

### Examples

```mathematica
TC[p]
```

$$p^0$$

```mathematica
TC[p - q]
```

$$(p-q)^0$$

```mathematica
FCI[TC[p]] // StandardForm

(*TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]]*)
```

`ExpandScalarProduct` is used to expand momenta in `TC`

```mathematica
ExpandScalarProduct[TC[p - q]]
% // StandardForm
```

$$p^0-q^0$$

```
(*TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]] - TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[q]]*)
```
