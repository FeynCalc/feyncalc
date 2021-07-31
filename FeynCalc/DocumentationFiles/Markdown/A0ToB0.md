## A0ToB0 

`A0ToB0` is an option for `A0`. If set to `True`, `A0[m^2]` is expressed by `(1 + B0[0, m^2, m^2]) m^2`.

### See also

[A0](A0), [B0](B0), [C0](C0), [D0](D0), [PaVe](PaVe).

### Examples

```mathematica
A0[m^2]
```

$$\text{A}_0\left(m^2\right)$$

```mathematica
A0[m^2, A0ToB0 -> True]
```

$$-\frac{2 m^2 \text{B}_0\left(0,m^2,m^2\right)}{2-D}$$