## FeynAmpDenominatorSimplify

`FeynAmpDenominatorSimplify[exp]` tries to simplify each `PropagatorDenominator` in a canonical way. `FeynAmpDenominatorSimplify[exp, q1]` simplifies all `FeynAmpDenominator`s in `exp` in a canonical way, including momentum shifts. Scaleless integrals are discarded.

### See also

[Overview](Extra/FeynCalc.md), [TID](TID.md).

### Examples

```mathematica
FDS
```

$$\text{FeynAmpDenominatorSimplify}$$

The cornerstone of dimensional regularization is that $\int d^n k f(k)/k^4 = 0$

```mathematica
FeynAmpDenominatorSimplify[f[k] FAD[k, k], k]
```

$$0$$

This brings some loop integrals into a standard form.

```mathematica
FeynAmpDenominatorSimplify[FAD[k - Subscript[p, 1], k - Subscript[p, 2]], k]
```

$$\frac{1}{k^2.(k-p_1+p_2){}^2}$$

```mathematica
FeynAmpDenominatorSimplify[FAD[k, k, k - q], k]
```

$$\frac{1}{\left(k^2\right)^2.(k-q)^2}$$

```mathematica
FeynAmpDenominatorSimplify[f[k] FAD[k, k - q, k - q], k]
```

$$\frac{f(q-k)}{\left(k^2\right)^2.(k-q)^2}$$

```mathematica
FeynAmpDenominatorSimplify[FAD[k - Subscript[p, 1], k - Subscript[p, 2]] SPD[k, k], k]
ApartFF[%, {k}]
TID[%, k] // Factor2
```

$$\frac{2 \left(k\cdot p_2\right)+k^2+p_2{}^2}{k^2.(k-p_1+p_2){}^2}$$

$$\frac{2 \left(k\cdot p_2\right)+p_2{}^2}{k^2.(k-p_1+p_2){}^2}$$

$$\frac{p_1\cdot p_2}{k^2.(k-p_1+p_2){}^2}$$

```mathematica
FDS[FAD[k - p1, k - p2] SPD[k, OPEDelta]^2, k]
```

$$\frac{(k\cdot \Delta +\Delta \cdot \;\text{p2})^2}{k^2.(k-\text{p1}+\text{p2})^2}$$
