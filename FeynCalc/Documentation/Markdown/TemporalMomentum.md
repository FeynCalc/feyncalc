## TemporalMomentum

`TemporalMomentum[p]`  is the head of the temporal component of a $4$-momentum $p^0$. The internal representation of the temporal component $p^0$ is `TemporalMomentum[p]`.

`TemporalMomentum` may appear only inside `TemporalPair`s.

### See also

[Overview](Extra/FeynCalc.md), [TemporalPair](TemporalPair.md), [ExplicitLorentzIndex](ExplicitLorentzIndex.md).

### Examples

```mathematica
TemporalMomentum[p]
```

$$p$$

```mathematica
TemporalMomentum[-q]
% // StandardForm
```

$$-q$$

```
(*-TemporalMomentum[q]*)
```

```mathematica
TemporalMomentum[p + q]
% // MomentumExpand // StandardForm
```

$$p+q$$

```
(*TemporalMomentum[p] + TemporalMomentum[q]*)
```

```mathematica
% // MomentumCombine // StandardForm

(*TemporalMomentum[p + q]*)
```
