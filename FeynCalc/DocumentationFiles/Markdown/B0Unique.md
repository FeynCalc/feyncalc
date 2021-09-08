## B0Unique

`B0Unique` is an option of `B0`. If set to `True`, `B0[0,0,m2]` is replaced with `(B0[0,m2,m2]+1)` and `B0[m2,0,m2]` simplifies to `(B0[0,m2,m2]+2)`.

### See also

[Overview](Extra/FeynCalc.md), [B0](B0.md).

### Examples

By default no transformation is done.

```mathematica
B0[0, 0, s]
```

$$\text{B}_0(0,0,s)$$

With `B0Real->True` following transformation is applied

```mathematica
B0[0, 0, s, B0Unique -> True]
```

$$\text{B}_0(0,s,s)+1$$
