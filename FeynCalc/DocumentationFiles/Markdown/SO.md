## SO

`SO[q]` is a four-dimensional scalar product of `OPEDelta` with `q`. It is transformed into `Pair[Momentum[q], Momentum[OPEDelta]` by `FCI`.

### See also

[FCI](FCI), [OPEDelta](OPEDelta), [Pair](Pair), [ScalarProduct](ScalarProduct), [SOD](SOD).

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