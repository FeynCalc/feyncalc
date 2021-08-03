## SOD

`SOD[q]` is a $D$-dimensional scalar product of `OPEDelta` with `q`. It is transformed into `Pair[Momentum[q,D], Momentum[OPEDelta,D]` by `FeynCalcInternal`.

### See also

[OPEDelta](OPEDelta), [Pair](Pair), [ScalarProduct](ScalarProduct), [SOD](SOD).

### Examples

```mathematica
SOD[p]
```

$$\Delta \cdot p$$

```mathematica
SOD[p - q]
```

$$\Delta \cdot (p-q)$$

```mathematica
SOD[p] // FCI // StandardForm

(*Pair[Momentum[OPEDelta, D], Momentum[p, D]]*)
```