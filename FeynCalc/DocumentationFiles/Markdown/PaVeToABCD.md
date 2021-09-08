## PaVeToABCD

`PaVeToABCD[expr]` converts suitable PaVe functions to direct Passarino-Veltman functions (`A0`,  `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0`). `PaVeToABCD` is nearly the inverse of `ToPaVe2`.

### See also

[Overview](Extra/FeynCalc.md), [ToPaVe](ToPaVe.md), [ToPaVe2](ToPaVe2.md), [A0](A0.md), [A00](A00.md), [B0](B0.md), [B1](B1.md), [B00](B00.md), [B11](B11.md), [C0](C0.md), [D0](D0.md).

### Examples

```mathematica
PaVe[0, {pp}, {m1^2, m2^2}]
PaVeToABCD[%]
% // FCI // StandardForm
```

$$\text{B}_0\left(\text{pp},\text{m1}^2,\text{m2}^2\right)$$

$$\text{B}_0\left(\text{pp},\text{m1}^2,\text{m2}^2\right)$$

```
(*B0[pp, m1^2, m2^2]*)
```

```mathematica
PaVe[0, {SPD[p1], 0, SPD[p2]}, {m1^2, m2^2, m3^2}]
PaVeToABCD[%]
% // FCI // StandardForm

```

$$\text{C}_0\left(0,\text{p1}^2,\text{p2}^2,\text{m3}^2,\text{m2}^2,\text{m1}^2\right)$$

$$\text{C}_0\left(0,\text{p1}^2,\text{p2}^2,\text{m3}^2,\text{m2}^2,\text{m1}^2\right)$$

```
(*C0[0, Pair[Momentum[p1, D], Momentum[p1, D]], Pair[Momentum[p2, D], Momentum[p2, D]], m3^2, m2^2, m1^2]*)
```

```mathematica
PaVe[0, 0, {SPD[p1], 0, SPD[p2]}, {m1^2, m2^2, m3^2}]
PaVeToABCD[%]
% // FCI // StandardForm 
  
 

```

$$\text{C}_{00}\left(0,\text{p1}^2,\text{p2}^2,\text{m3}^2,\text{m2}^2,\text{m1}^2\right)$$

$$\text{C}_{00}\left(0,\text{p1}^2,\text{p2}^2,\text{m3}^2,\text{m2}^2,\text{m1}^2\right)$$

```
(*PaVe[0, 0, {0, Pair[Momentum[p1, D], Momentum[p1, D]], Pair[Momentum[p2, D], Momentum[p2, D]]}, {m3^2, m2^2, m1^2}]*)
```
