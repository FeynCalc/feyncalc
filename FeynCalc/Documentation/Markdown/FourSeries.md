## FourSeries

`FourSeries[exp, {p,p0,n}]` calculates Taylor series of `exp` w.r.t the $4$-vector $p$ to $n$th order.
If the expression diverges at $p = p_0$, it will be returned unevaluated.

### See also

[Overview](Extra/FeynCalc.md), [FourDivergence](FourDivergence.md), [ThreeDivergence](ThreeDivergence.md).

### Examples

```mathematica
(m^2 + SPD[p, q]) FAD[{k}, {k + p}] 
 
FourSeries[%, {p, 0, 2}]
```

$$\frac{m^2+p\cdot q}{k^2.(k+p)^2}$$

$$\frac{4 m^2 (k\cdot p)^2}{\left(k^2\right)^4}-\frac{m^2 p^2}{\left(k^2\right)^3}-\frac{2 m^2 (k\cdot p)}{\left(k^2\right)^3}+\frac{m^2}{\left(k^2\right)^2}+\frac{p\cdot q}{\left(k^2\right)^2}-\frac{2 (k\cdot p) (p\cdot q)}{\left(k^2\right)^3}$$

```mathematica
(SPD[p, q]) DiracTrace[GAD[mu] . GSD[p + q] . GAD[nu]] 
 
FourSeries[%, {p, 0, 2}]

```mathematica

$$(p\cdot q) \;\text{tr}\left(\gamma ^{\text{mu}}.(\gamma \cdot (p+q)).\gamma ^{\text{nu}}\right)$$

$$(p\cdot q) \;\text{tr}\left(\gamma ^{\text{mu}}.(\gamma \cdot p).\gamma ^{\text{nu}}\right)+(p\cdot q) \;\text{tr}\left(\gamma ^{\text{mu}}.(\gamma \cdot q).\gamma ^{\text{nu}}\right)$$