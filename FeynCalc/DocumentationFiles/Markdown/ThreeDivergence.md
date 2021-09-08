## ThreeDivergence

`ThreeDivergence[exp, CV[p, i]]`  calculates the partial derivative of `exp` w.r.t. $p^i$.

` ThreeDivergence[exp, CV[p, i], CV[p,i], ...]` gives the multiple derivative.

Owing to the fact that in FeynCalc dummy Cartesian index are always understood to be upper indices, applying `ThreeDivergence` to an expression is equivalent to the action of $\nabla^i = \frac{\partial}{\partial p^i}$.

### See also

[Overview](Extra/FeynCalc.md), [FourDivergence](FourDivergence.md).

### Examples

```mathematica
CSP[p, q]
ThreeDivergence[%, CV[q, i]]
```

$$\overline{p}\cdot \overline{q}$$

$$\overline{p}^i$$

```mathematica
CSP[p - k, q]
ThreeDivergence[%, CV[k, i]]
```

$$(\overline{p}-\overline{k})\cdot \overline{q}$$

$$-\overline{q}^i$$

```mathematica
CFAD[{p, m^2}, p - q]
ThreeDivergence[%, CVD[p, i]] 
  
 

```

$$\frac{1}{(p^2+m^2-i \eta ).((p-q)^2-i \eta )}$$

$$\frac{2 q^i-2 p^i}{(p^2+m^2-i \eta ).((p-q)^2-i \eta )^2}-\frac{2 p^i}{(p^2+m^2-i \eta )^2.((p-q)^2-i \eta )}$$