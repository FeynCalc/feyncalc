## ToPaVe2

`ToPaVe2[expr]` converts all the direct Passarino-Veltman functions (`A0`, `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0`) to `PaVe`-functions.

### See also

[Overview](Extra/FeynCalc.md), [ToPaVe](ToPaVe.md).

### Examples

```mathematica
A0[m^2]
ToPaVe2[%]
% // FCI // StandardForm
```

$$\text{A}_0\left(m^2\right)$$

$$\text{A}_0\left(m^2\right)$$

```
(*PaVe[0, {}, {m^2}]*)
```

```mathematica
B11[pp, m^2, M^2, BReduce -> False]
ToPaVe2[%]
% // FCI // StandardForm
```

$$\text{B}_{11}\left(\text{pp},m^2,M^2\right)$$

$$\text{B}_{11}\left(\text{pp},m^2,M^2\right)$$

```
(*PaVe[1, 1, {pp}, {m^2, M^2}]*)
```
