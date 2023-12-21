## ToPaVe2

`ToPaVe2[expr]` converts all the direct Passarino-Veltman functions (`A0`, `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0`) to `PaVe`-functions.

### See also

[Overview](Extra/FeynCalc.md), [ToPaVe](ToPaVe.md).

### Examples

```mathematica
ToPaVe2[A0[m^2]]
```

$$\text{A}_0\left(m^2\right)$$

```mathematica
ToPaVe2[A0[m^2]] // FCI // StandardForm

(*PaVe[0, {}, {m^2}]*)
```

```mathematica
ToPaVe2[B11[pp, m^2, M^2, BReduce -> False]]
```

$$\text{B}_{11}\left(\text{pp},m^2,M^2\right)$$

```mathematica
ToPaVe2[B11[pp, m^2, M^2, BReduce -> False]] // FCI // StandardForm

(*PaVe[1, 1, {pp}, {m^2, M^2}]*)
```