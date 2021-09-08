## OneLoopSimplify

`OneLoopSimplify[amp, q]` simplifies the one-loop amplitude amp. The second argument denotes the integration momentum.

If the first argument has head `FeynAmp` then `OneLoopSimplify[FeynAmp[name, k, expr], k]` transforms to `OneLoopSimplify[expr, k]`

### See also

[Overview](Extra/FeynCalc.md), [TID](TID.md), [TIDL](TIDL.md).

### Examples

```mathematica
SPD[k, r] FAD[{k, m} , k - p] // FCI
OneLoopSimplify[%, k]
OneLoopSimplify[% /. m -> 0, k]
```

$$\frac{k\cdot r}{\left(k^2-m^2\right).(k-p)^2}$$

$$\frac{\left(m^2+p^2\right) (p\cdot r)}{2 p^2 k^2.\left((k-p)^2-m^2\right)}-\frac{p\cdot r}{2 p^2 \left(k^2-m^2\right)}$$

$$\frac{p\cdot r}{2 k^2.(k-p)^2}$$

```mathematica
FAD[k, k, k - Subscript[p, 1], k - Subscript[p, 2]] FVD[k, \[Mu]] // FCI
OneLoopSimplify[ %, k]
FCE[%] /. SPD[Subscript[p, 1]] -> 0 // FCI
```

$$\frac{k^{\mu }}{\left(k^2\right)^2.(k-p_1){}^2.(k-p_2){}^2}$$

$$\frac{p_1{}^2 p_2{}^{\mu }-p_1{}^{\mu } \left(p_1\cdot p_2\right)}{2 \left(k^2\right)^2.(k-p_1){}^2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}+\frac{-p_1{}^2 p_2{}^2 p_1{}^{\mu }-p_1{}^2 p_2{}^2 p_2{}^{\mu }+p_1{}^2 p_2{}^{\mu } \left(p_1\cdot p_2\right)+p_2{}^2 p_1{}^{\mu } \left(p_1\cdot p_2\right)}{2 \left(k^2\right)^2.(k-p_1){}^2.(k-p_2){}^2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}-\frac{p_2{}^{\mu } \left(p_1\cdot p_2\right)-p_2{}^2 p_1{}^{\mu }}{2 \left(k^2\right)^2.(k-p_2){}^2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}-\frac{p_1{}^2 p_2{}^{\mu }+p_2{}^2 p_1{}^{\mu }-p_1{}^{\mu } \left(p_1\cdot p_2\right)-p_2{}^{\mu } \left(p_1\cdot p_2\right)}{2 k^2.(k-p_1){}^2.(k-p_2){}^2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}$$

$$\frac{p_2{}^2 p_1{}^{\mu }}{2 \left(p_1\cdot p_2\right) \left(k^2\right)^2.(k-p_1){}^2.(k-p_2){}^2}-\frac{p_1{}^{\mu }}{2 \left(p_1\cdot p_2\right) \left(k^2\right)^2.(k-p_1){}^2}-\frac{p_2{}^{\mu } \left(p_1\cdot p_2\right)-p_2{}^2 p_1{}^{\mu }}{2 \left(k^2\right)^2.(k-p_2){}^2 (p_1\cdot p_2){}^2}-\frac{p_2{}^2 p_1{}^{\mu }-p_1{}^{\mu } \left(p_1\cdot p_2\right)-p_2{}^{\mu } \left(p_1\cdot p_2\right)}{2 k^2.(k-p_1){}^2.(k-p_2){}^2 (p_1\cdot p_2){}^2}$$

```mathematica
OneLoopSimplify[FAD[k - Subscript[p, 1], k - Subscript[p, 2]] SPD[k, l]^2, k]
```

$$-\frac{D (l\cdot p_1){}^2+D (l\cdot p_2){}^2+2 D \left(l\cdot p_2\right) \left(l\cdot p_1\right)-l^2 p_1{}^2-l^2 p_2{}^2-4 \left(l\cdot p_2\right) \left(l\cdot p_1\right)+2 l^2 \left(p_1\cdot p_2\right)}{4 (1-D) k^2.(k-p_1+p_2){}^2}$$
