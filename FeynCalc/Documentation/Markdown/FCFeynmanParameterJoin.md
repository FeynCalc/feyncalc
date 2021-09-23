## FCFeynmanParameterJoin

`FCFeynmanParameterJoin[{{{prop1,prop2,x},prop3,y},...}, {p1,p2,...}]` joins all propagators in `int` using Feynman parameters but does not integrate over the loop momenta $p_i$. The function returns `{fpInt,pref,vars}`, where `fpInt` is the piece of the integral that contains a single `GFAD`-type propagator and `pref` is the part containing the `res`. The introduced Feynman parameters are listed in vars. The overall Dirac delta is omitted.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md).

### Examples

```mathematica
testProps = {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p3, m3}], FAD[{p4, m4}]}
```

$$\left\{\frac{1}{\text{p1}^2-\text{m1}^2},\frac{1}{\text{p2}^2-\text{m2}^2},\frac{1}{\text{p3}^2-\text{m3}^2},\frac{1}{\text{p4}^2-\text{m4}^2}\right\}$$

Let us first join two propagators with each other using Feynman parameters `x[i]`

```mathematica
FCFeynmanParameterJoin[{testProps[[1]], testProps[[2]], x}, {p1, p2, p3, p4}]
```

$$\left\{\frac{1}{(\left(\text{p1}^2-\text{m1}^2\right) x(1)+\left(\text{p2}^2-\text{m2}^2\right) x(2)+i \eta )^2},1,\{x(1),x(2)\}\right\}$$

Now we can join the resulting propagator with another propagator by introducing another set of Feynman parameters `y[i]`

```mathematica
FCFeynmanParameterJoin[{{testProps[[1]], testProps[[2]], x}, testProps[[3]], y}, {p1, p2, p3, p4}]
```

$$\left\{\frac{1}{(\left(-x(1) \;\text{m1}^2+\text{p1}^2 x(1)-\text{m2}^2 x(2)+\text{p2}^2 x(2)\right) y(1)+\left(\text{p3}^2-\text{m3}^2\right) y(2)+i \eta )^3},2 y(1),\{x(1),x(2),y(1),y(2)\}\right\}$$

If needed, this procedure can be nested even further

```mathematica
FCFeynmanParameterJoin[{{{testProps[[1]], testProps[[2]], x}, testProps[[3]], y}, testProps[[4]], z}, {p1, p2, p3, p4}]
```

$$\left\{\frac{1}{(\left(-x(1) y(1) \;\text{m1}^2+\text{p1}^2 x(1) y(1)-\text{m2}^2 x(2) y(1)+\text{p2}^2 x(2) y(1)-\text{m3}^2 y(2)+\text{p3}^2 y(2)\right) z(1)+\left(\text{p4}^2-\text{m4}^2\right) z(2)+i \eta )^4},6 y(1) z(1)^2,\{x(1),x(2),y(1),y(2),z(1),z(2)\}\right\}$$

Notice that `FCFeynmanParametrize`knows how to deal with the output produced by `FCFeynmanParameterJoin`

```mathematica
intT = FCFeynmanParameterJoin[{{SFAD[{p1, mg^2}] SFAD[{p3 - p1, mg^2}], 1, x}, SFAD[{{0, -2 p1 . q}}] SFAD[{{0, -2 p3 . q}}], y}, {p1, p3}]
```

$$\left\{\frac{1}{(\left(-x(1) \;\text{mg}^2-x(2) \;\text{mg}^2+\text{p1}^2 x(1)+\text{p1}^2 x(2)-2 (\text{p1}\cdot \;\text{p3}) x(2)+\text{p3}^2 x(2)\right) y(1)-2 (\text{p1}\cdot q) y(2)-2 (\text{p3}\cdot q) y(3)+i \eta )^4},6 y(1),\{x(1),x(2),y(1),y(2),y(3)\}\right\}$$

```mathematica
FCFeynmanParametrize[intT[[1]], intT[[2]], {p1, p3}, Names -> z, Indexed -> True, FCReplaceD -> {D -> 4 - 2 ep}, Simplify -> True, 
  Assumptions -> {mg > 0, ep > 0}, FinalSubstitutions -> {FCI@SPD[q] -> qq, mg^2 -> mg2}, Variables -> intT[[3]]]
```

$$\left\{\frac{\left(x(1) x(2) y(1)^2\right)^{3 \;\text{ep}} \left(\text{mg2} x(1) x(2) (x(1)+x(2)) y(1)^3+\text{qq} y(1) \left(x(1) y(3)^2+x(2) (y(2)+y(3))^2\right)\right)^{-2 \;\text{ep}}}{x(1)^2 x(2)^2 y(1)^3},\Gamma (2 \;\text{ep}),\{x(1),x(2),y(1),y(2),y(3)\}\right\}$$