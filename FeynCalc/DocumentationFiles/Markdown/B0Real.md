## B0Real

`B0Real` is an option of `B0` (default `False`). If set to `True`, `B0` is assumed to be real and the relation `B0[a,0,a] = 2 + B0[0,a,a]` is applied.

### See also

[Overview](Extra/FeynCalc.md), [B0](B0.md).

### Examples

By default the arguments are not assumed real.

```mathematica
B0[s, 0, s]
```

$$\text{B}_0(s,0,s)$$

With B0RealTrue, transformation is done.

```mathematica
B0[s, 0, s, B0Real -> True, B0Unique -> True]
```

$$\text{B}_0(0,s,s)+2$$
