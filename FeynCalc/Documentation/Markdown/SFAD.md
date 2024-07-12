## SFAD

`SFAD[{{q1 +..., p1 . q2 +...,} {m^2, s}, n}, ...]` denotes a standard Lorentzian  propagator given by $\frac{1}{[(q_1+\ldots)^2 + p_1 \cdot q_2 ... + m^2 + s i \eta]^n}$, where $q_1^2$ and $p_1 \cdot q_2$ are Lorentzian scalar products in $D$ dimensions.

For brevity one can also use shorter forms such as `SFAD[{q1+ ...,  m^2}, ...]`, `SFAD[{q1+ ...,  m^2 , n}, ...]`, `SFAD[{q1+ ...,  {m^2, -1}}, ...]`, `SFAD[q1,...]` etc.

If `s` is not explicitly specified, its value is determined by the option `EtaSign`, which has the default value `+1` and corresponds to $+ i \eta$

If `n` is not explicitly specified, then the default value `1` is assumed. Translation into the FeynCalc internal form is performed by `FeynCalcInternal`, where an `SFAD` is encoded using the special head `StandardPropagatorDenominator`.

`SFAD` can represent more versatile propagators as compared to the old `FAD`. In particular, `FAD` does not allow one to enter eikonal propagators, track the sign of the $i \eta$ or change the sign and the form of the mass term.

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

```mathematica
SFAD[{{0, n . q}}]
```

$$\frac{1}{(n\cdot q+i \eta )}$$

```mathematica
SFAD[{{p, p . q}, m^2}]
```

$$\frac{1}{(p^2+p\cdot q-m^2+i \eta )}$$

The so called Smirnov-notation for propagators can be achieved by multiplying the quadratic part by `I` and switching the sign of the mass term.

```mathematica
SFAD[{{I*p, 0}, -m^2}]
```

$$\frac{1}{(-p^2+m^2+i \eta )}$$

If one wants to have additional variables multiplying loop or external momenta, those need to be declared to be of the `FCVariable` type

```mathematica
DataType[la, FCVariable] = True
```

$$\text{True}$$

```mathematica
SFAD[{{0, la p . q}, m^2}]
```

$$\frac{1}{(\text{la} (p\cdot q)-m^2+i \eta )}$$

```mathematica
% // FCI // StandardForm

(*FeynAmpDenominator[StandardPropagatorDenominator[0, la Pair[Momentum[p, D], Momentum[q, D]], -m^2, {1, 1}]]*)
```