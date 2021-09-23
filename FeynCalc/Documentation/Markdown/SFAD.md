## SFAD

`SFAD[{{q1 +..., p1 . q2 +...,} {m^2, s}, n}, ...]` denotes a Cartesian propagator given by \frac{1}{[(q_1+\ldots)^2 + p_1 \cdot q_2 ... + m^2 + s i \eta]^n}, where $q_1^2$ and $p_1 \cdot q_2$ are Cartesian scalar products in $D-1$ dimensions.

For brevity one can also use shorter forms such as `SFAD[{q1+ ...,  m^2}, ...]`, `SFAD[{q1+ ...,  m^2 , n}, ...]`, `SFAD[{q1+ ...,  {m^2, -1}}, ...]`, `SFAD[q1,...]`  etc.

If `s` is not explicitly specified, its value is determined by the option `EtaSign`, which has the default value `+1`.

If `n` is not explicitly specified, then the default value `1` is assumed. Translation into FeynCalcI internal form is performed by `FeynCalcInternal`, where a `SFAD` is encoded using the special head `CartesianPropagatorDenominator`.

### See also

[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [GFAD](GFAD.md), [CFAD](CFAD.md).

### Examples

```mathematica
SFAD[{{p, 0}, m^2}]
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

```mathematica
SFAD[{{p, 0}, {m^2, -1}}]
```

$$\frac{1}{(p^2-m^2-i \eta )}$$

```mathematica
SFAD[{{p, 0}, {-m^2, -1}}]
```

$$\frac{1}{(p^2+m^2-i \eta )}$$

```mathematica
SFAD[{{0, p . q}, m^2}]
```

$$\frac{1}{(p\cdot q-m^2+i \eta )}$$
