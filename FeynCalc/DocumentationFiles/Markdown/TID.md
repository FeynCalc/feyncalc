##  TID 

TID[amp, q] performs  tensor decomposition of 1-loop integrals with loop momentum q.

###  See also 

OneLoopSimplify, TIDL.

###  Examples 

```mathematica
FCClearScalarProducts[];
int = FAD[{k, m}, k - Subscript[p, 1], k - Subscript[p, 2]] FVD[k, \[Mu]] // FCI
```

$$\frac{k^{\mu }}{\left(k^2-m^2\right).(k-p_1){}^2.(k-p_2){}^2}$$

By default, all the tensor integrals are reduced to the Passarino-Veltman scalar integrals $A_0$, $B_0$, $C_0$, $D_0$ etc. 

```mathematica
TID[int, k]
```

$$\frac{p_1{}^2 p_2{}^{\mu }-p_1{}^{\mu } \left(p_1\cdot p_2\right)}{2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right) k^2.\left((k+p_1){}^2-m^2\right)}-\frac{p_2{}^2 \left(m^2+p_1{}^2\right) p_1{}^{\mu }+p_1{}^2 \left(m^2+p_2{}^2\right) p_2{}^{\mu }+\left(m^2+p_1{}^2\right) \left(-p_2{}^{\mu }\right) \left(p_1\cdot p_2\right)-\left(m^2+p_2{}^2\right) p_1{}^{\mu } \left(p_1\cdot p_2\right)}{2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right) \left(k^2-m^2\right).(k-p_2){}^2.(k-p_1){}^2}-\frac{p_2{}^{\mu } \left(p_1\cdot p_2\right)-p_2{}^2 p_1{}^{\mu }}{2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right) k^2.\left((k+p_2){}^2-m^2\right)}-\frac{p_1{}^2 p_2{}^{\mu }+p_2{}^2 p_1{}^{\mu }-p_1{}^{\mu } \left(p_1\cdot p_2\right)-p_2{}^{\mu } \left(p_1\cdot p_2\right)}{2 k^2.(k-p_1+p_2){}^2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}$$

Scalar integrals can be converted to the Passarino-Veltman notation via the option ToPaVe

```mathematica
TID[int, k, ToPaVe -> True]
```

$$\frac{i \pi ^2 \left(p_1{}^2 p_2{}^{\mu }-p_1{}^{\mu } \left(p_1\cdot p_2\right)\right) \text{B}_0\left(p_1{}^2,0,m^2\right)}{2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}-\frac{i \pi ^2 \left(p_2{}^{\mu } \left(p_1\cdot p_2\right)-p_2{}^2 p_1{}^{\mu }\right) \text{B}_0\left(p_2{}^2,0,m^2\right)}{2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}-\frac{i \pi ^2 \left(p_1{}^2 p_2{}^{\mu }+p_2{}^2 p_1{}^{\mu }-p_1{}^{\mu } \left(p_1\cdot p_2\right)-p_2{}^{\mu } \left(p_1\cdot p_2\right)\right) \text{B}_0\left(p_1{}^2-2 \left(p_1\cdot p_2\right)+p_2{}^2,0,0\right)}{2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}-\frac{i \pi ^2 \left(p_2{}^2 \left(m^2+p_1{}^2\right) p_1{}^{\mu }+p_1{}^2 \left(m^2+p_2{}^2\right) p_2{}^{\mu }+\left(m^2+p_1{}^2\right) \left(-p_2{}^{\mu }\right) \left(p_1\cdot p_2\right)-\left(m^2+p_2{}^2\right) p_1{}^{\mu } \left(p_1\cdot p_2\right)\right) \text{C}_0\left(p_1{}^2,p_2{}^2,p_1{}^2-2 \left(p_1\cdot p_2\right)+p_2{}^2,0,m^2,0\right)}{2 \left((p_1\cdot p_2){}^2-p_1{}^2 p_2{}^2\right)}$$

We can force the reduction algorithm to use Passarino-Veltman coefficient functions via the option UsePaVeBasis

```mathematica
TID[int, k, UsePaVeBasis -> True]
```

$$-i \pi ^2 p_1{}^{\mu } \text{C}_1\left(p_1{}^2,p_1{}^2+p_2{}^2-2 \left(p_1\cdot p_2\right),p_2{}^2,m^2,0,0\right)-i \pi ^2 p_2{}^{\mu } \text{C}_2\left(p_1{}^2,p_1{}^2+p_2{}^2-2 \left(p_1\cdot p_2\right),p_2{}^2,m^2,0,0\right)$$

The basis of Passarino-Veltman coefficient functions is used automatically if there are zero Gram determinants

```mathematica
FCClearScalarProducts[];
SPD[Subscript[p, 1], Subscript[p, 1]] = 0;
SPD[Subscript[p, 2], Subscript[p, 2]] = 0;
SPD[Subscript[p, 1], Subscript[p, 2]] = 0;
TID[int, k]
```

$$-i \pi ^2 \left(p_1{}^{\mu }+p_2{}^{\mu }\right) \text{C}_1\left(0,0,0,m^2,0,0\right)$$

In FeynCalc, Passarino-Veltman coefficient functions are defined in the same way as in LoopTools, which is a quite common convention. If one wants to use a different definition, it is useful to activate the option GenPaVe

```mathematica
TID[int, k, GenPaVe -> True]
```

$$-i \pi ^2 p_1{}^{\mu } \text{GenPaVe}\left(\{1\},\left(
\begin{array}{cc}
 0 & m \\
 p_1 & 0 \\
 p_2 & 0 \\
\end{array}
\right)\right)-i \pi ^2 p_2{}^{\mu } \text{GenPaVe}\left(\{2\},\left(
\begin{array}{cc}
 0 & m \\
 p_1 & 0 \\
 p_2 & 0 \\
\end{array}
\right)\right)$$

To simplify manifestly IR finite 1-loop results written in terms of Passarino-Veltman functions, we may employ the option PaVeLimitTo4 (must be used together with ToPaVe).
The result is valid up to 0th order in Epsilon, i.e. sufficient for 1-loop calculations.

```mathematica
FCClearScalarProducts[];
int = (D - 1) (D - 2)/(D - 3) FVD[p, mu] FVD[p, nu] FAD[p, p - q] 
 
TID[int, p, ToPaVe -> True] 
 
TID[int, p, ToPaVe -> True, PaVeLimitTo4 -> True]
```

$$\frac{(D-2) (D-1) p^{\text{mu}} p^{\text{nu}}}{(D-3) p^2.(p-q)^2}$$

$$\frac{i \pi ^2 (2-D) \text{B}_0\left(q^2,0,0\right) \left(D q^{\text{mu}} q^{\text{nu}}-q^2 g^{\text{mu}\text{nu}}\right)}{4 (3-D)}$$

$$\frac{1}{2} i \pi ^2 \text{B}_0\left(\overline{q}^2,0,0\right) \left(4 \overline{q}^{\text{mu}} \overline{q}^{\text{nu}}-\overline{q}^2 \bar{g}^{\text{mu}\text{nu}}\right)+\frac{1}{2} i \pi ^2 \left(2 \overline{q}^{\text{mu}} \overline{q}^{\text{nu}}-\overline{q}^2 \bar{g}^{\text{mu}\text{nu}}\right)$$