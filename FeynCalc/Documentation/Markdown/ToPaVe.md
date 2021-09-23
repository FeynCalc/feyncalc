## ToPaVe

`ToPaVe[exp, q]`  converts all scalar 1-loop integrals in `exp` that depend on the momentum `q` to scalar Passarino Veltman functions `A0`, `B0`, `C0`, `D0` etc.

### See also

[Overview](Extra/FeynCalc.md), [PaVeToABCD](PaVeToABCD.md), [ToPaVe2](ToPaVe2.md), [A0](A0.md), [A00](A00.md), [B0](B0.md), [B1](B1.md), [B00](B00.md), [B11](B11.md), [C0](C0.md), [D0](D0.md).

### Examples

```mathematica
FAD[{q, m1}]
ToPaVe[%, q]
```

$$\frac{1}{q^2-\text{m1}^2}$$

$$i \pi ^2 \;\text{A}_0\left(\text{m1}^2\right)$$

```mathematica
FAD[{q, m1}, {q + p1, m2}]
ToPaVe[%, q]
% // StandardForm
```

$$\frac{1}{\left(q^2-\text{m1}^2\right).\left((\text{p1}+q)^2-\text{m2}^2\right)}$$

$$i \pi ^2 \;\text{B}_0\left(\text{p1}^2,\text{m1}^2,\text{m2}^2\right)$$

```
(*I \[Pi]^2 B0[Pair[Momentum[p1, D], Momentum[p1, D]], m1^2, m2^2]*)
```

```mathematica
FAD[{q, m1}, {q + p1, m2}, {q + p2, m3}, {q + p3, m4}, {q + p4, m5}]
ToPaVe[%, q]
```

$$\frac{1}{\left(q^2-\text{m1}^2\right).\left((\text{p1}+q)^2-\text{m2}^2\right).\left((\text{p2}+q)^2-\text{m3}^2\right).\left((\text{p3}+q)^2-\text{m4}^2\right).\left((\text{p4}+q)^2-\text{m5}^2\right)}$$

$$i \pi ^2 \;\text{E}_0\left(\text{p1}^2,\text{p2}^2,-2 (\text{p2}\cdot \;\text{p3})+\text{p2}^2+\text{p3}^2,-2 (\text{p3}\cdot \;\text{p4})+\text{p3}^2+\text{p4}^2,-2 (\text{p1}\cdot \;\text{p4})+\text{p1}^2+\text{p4}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,\text{p3}^2,-2 (\text{p2}\cdot \;\text{p4})+\text{p2}^2+\text{p4}^2,-2 (\text{p1}\cdot \;\text{p3})+\text{p1}^2+\text{p3}^2,\text{p4}^2,\text{m2}^2,\text{m1}^2,\text{m3}^2,\text{m4}^2,\text{m5}^2\right)$$

By default, `ToPaVe` has the option `PaVeToABCD ` set to `True`. This means that some of the `PaVe` functions are automatically converted to direct Passarino-Veltman functions (`A0`,  `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0`). This also has consequences for `TID`

```mathematica
TID[FVD[q, mu] FAD[{q, m1}, {q + p}], q, ToPaVe -> True]
% // StandardForm
```

$$\frac{i \pi ^2 p^{\text{mu}} \;\text{A}_0\left(\text{m1}^2\right)}{2 p^2}-\frac{i \pi ^2 \left(\text{m1}^2+p^2\right) p^{\text{mu}} \;\text{B}_0\left(p^2,0,\text{m1}^2\right)}{2 p^2}$$

$$\frac{i \pi ^2 \;\text{A0}\left[\text{m1}^2\right] \;\text{Pair}[\text{LorentzIndex}[\text{mu},D],\text{Momentum}[p,D]]}{2 \;\text{Pair}[\text{Momentum}[p,D],\text{Momentum}[p,D]]}-\left.\left(i \pi ^2 \;\text{B0}\left[\text{Pair}[\text{Momentum}[p,D],\text{Momentum}[p,D]],0,\text{m1}^2\right] \;\text{Pair}[\text{LorentzIndex}[\text{mu},D],\text{Momentum}[p,D]] \left(\text{m1}^2+\text{Pair}[\text{Momentum}[p,D],\text{Momentum}[p,D]]\right)\right)\right/(2 \;\text{Pair}[\text{Momentum}[p,D],\text{Momentum}[p,D]])$$

If you want to avoid direct functions in the output of `TID` and other functions that employ `ToPaVe`, you need to set the option `PaVeToABCD` to `False` globally.

```mathematica
SetOptions[ToPaVe, PaVeToABCD -> False];
```

```mathematica
TID[FVD[q, mu] FAD[{q, m1}, {q + p}], q, ToPaVe -> True]
% // StandardForm 
  
 

```

$$\frac{i \pi ^2 p^{\text{mu}} \;\text{A}_0\left(\text{m1}^2\right)}{2 p^2}-\frac{i \pi ^2 \left(\text{m1}^2+p^2\right) p^{\text{mu}} \;\text{B}_0\left(p^2,0,\text{m1}^2\right)}{2 p^2}$$

$$\frac{i \pi ^2 \;\text{Pair}[\text{LorentzIndex}[\text{mu},D],\text{Momentum}[p,D]] \;\text{PaVe}\left[0,\{\},\left\{\text{m1}^2\right\}\right]}{2 \;\text{Pair}[\text{Momentum}[p,D],\text{Momentum}[p,D]]}-\left.\left(i \pi ^2 \;\text{Pair}[\text{LorentzIndex}[\text{mu},D],\text{Momentum}[p,D]] \left(\text{m1}^2+\text{Pair}[\text{Momentum}[p,D],\text{Momentum}[p,D]]\right) \;\text{PaVe}\left[0,\{\text{Pair}[\text{Momentum}[p,D],\text{Momentum}[p,D]]\},\left\{0,\text{m1}^2\right\}\right]\right)\right/(2 \;\text{Pair}[\text{Momentum}[p,D],\text{Momentum}[p,D]])$$
