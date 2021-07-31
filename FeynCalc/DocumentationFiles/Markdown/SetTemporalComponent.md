`SetTemporalComponent[p, val]` sets the value of the temporal component of a $4$-vector $p$, `TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]]` to `val`.

### See also

[TC](TC), [TemporalPair](TemporalPair), [TemporalMomentum](TemporalMomentum).

### Examples

```mathematica
FCClearScalarProducts[]
ClearAll[t]
SetTemporalComponent[p, t]
TC[p]
```

$$t$$

```mathematica
TC[p + q] // ExpandScalarProduct
```

$$q^0+t$$

```mathematica
SP[p, q] // LorentzToCartesian
```

$$q^0 t-\overline{p}\cdot \overline{q}$$