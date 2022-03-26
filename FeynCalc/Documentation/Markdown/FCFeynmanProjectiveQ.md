## FCFeynmanProjectiveQ

`FCFeynmanProjectiveQ[int, x]` checks if the given Feynman parameter integral (without prefactors) depending on x[1], x[2], ...  is a projective form.

It is similar to `FCFeynmanProjectivize` but unlike the former it simply returns `True` or `False` depending
on whether the integral is projective or not.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanPrepare](FCFeynmanPrepare.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md).

### Examples

```mathematica
int = SFAD[{p3, mg^2}] SFAD[{p3 - p1, mg^2}] SFAD[{{0, -2 p1 . q}}]
```

$$\frac{1}{(\text{p3}^2-\text{mg}^2+i \eta ) ((\text{p3}-\text{p1})^2-\text{mg}^2+i \eta ) (-2 (\text{p1}\cdot q)+i \eta )}$$

```mathematica
fp = FCFeynmanParametrize[int, {p1, p3}, Names -> x, Indexed -> True, FCReplaceD -> {D -> 4 - 2 ep}, Simplify -> True, 
   Assumptions -> {mg > 0, ep > 0}, FinalSubstitutions -> {SPD[q] -> qq, mg^2 -> mg2}]
```

$$\left\{(x(2) x(3))^{3 \text{ep}-3} \left((x(2)+x(3)) \left(\text{mg2} x(2) x(3)+\text{qq} x(1)^2\right)\right)^{1-2 \text{ep}},-\Gamma (2 \text{ep}-1),\{x(1),x(2),x(3)\}\right\}$$

```mathematica
FCFeynmanProjectiveQ[fp[[1]], x]
```

$$\text{True}$$

```mathematica
FCFeynmanProjectiveQ[(x[1] + x[2])^(-2 + 2*ep)/(mb2*(x[1]^2 + x[1]*x[2] + x[2]^2))^ep, x]
```

$$\text{True}$$

Feynman parametrization derived from propagator representation should be projective in most cases.
However, arbitrary Feynman parameter integral do not necessarily have this property.

```mathematica
FCFeynmanProjectiveQ[x[1]^(x - 1) (x[2])^(y - 1), x]
```

$$\text{False}$$