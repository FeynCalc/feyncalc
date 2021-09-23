## PaVe

`PaVe[i, j, ..., {p10, p12, ...}, {m1^2, mw^2, ...}]` denotes the invariant (and scalar) Passarino-Veltman integrals, i.e. the coefficient functions of the tensor integral decomposition. Joining `plist` and `mlist` gives the same conventions as for `A0`, `B0`, `C0`, `D0`. Automatic simplifications are performed for the coefficient functions of two-point integrals and for the scalar integrals.

### See also

[Overview](Extra/FeynCalc.md), [PaVeReduce](PaVeReduce.md).

### Examples

Some of the PaVe's reduce to special cases with `PaVeAutoReduce`to `True`

```mathematica
PaVe[0, 0, {pp}, {m^2, M^2}, PaVeAutoReduce -> True]
```

$$\frac{\left(m^2-2 m M+M^2-\text{pp}\right) \left(m^2+2 m M+M^2-\text{pp}\right) \;\text{B}_0\left(\text{pp},m^2,M^2\right)}{4 (1-D) \;\text{pp}}-\frac{\text{A}_0\left(m^2\right) \left(m^2-M^2+\text{pp}\right)}{4 (1-D) \;\text{pp}}+\frac{\text{A}_0\left(M^2\right) \left(m^2-M^2-\text{pp}\right)}{4 (1-D) \;\text{pp}}$$