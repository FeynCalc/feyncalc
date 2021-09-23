## Power2

`Power2[x, y]` represents `x^y`.  Sometimes `Power2` is more useful than the Mathematica `Power`. `Power2[-a,b]` simplifies to `(-1)^b Power2[a,b]` (if no `Epsilon` is in `b` ...).

### See also

[Overview](Extra/FeynCalc.md), [PowerFactor](PowerFactor.md).

### Examples

```mathematica
Power[-a, b]
```

$$(-a)^b$$

```mathematica
Power2[-a, b]
```

$$(-1)^b a^b$$
