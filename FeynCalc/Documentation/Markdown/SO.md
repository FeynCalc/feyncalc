## SO

`SO[q]` is a four-dimensional scalar product of `OPEDelta` with `q`. It is transformed into `Pair[Momentum[q], Momentum[OPEDelta]` by `FCI`.

### See also

[Overview](Extra/FeynCalc.md), [FCI](FCI.md), [OPEDelta](OPEDelta.md), [Pair](Pair.md), [ScalarProduct](ScalarProduct.md), [SOD](SOD.md).

### Examples

```mathematica
SO[p]
```

$$\Delta \cdot p$$

```mathematica
SO[p - q]
```

$$\Delta \cdot (p-q)$$

```mathematica
SO[p] // FCI // StandardForm

(*Pair[Momentum[OPEDelta], Momentum[p]]*)
```
