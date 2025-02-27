## Expanding and undoing expansions

### See also

[Overview](Extra/FeynCalc.md).

### Manipulations

FeynCalc offers further useful functions for the manipulations of Lorentz tensors and Dirac matrices. To expand scalar products

```mathematica
ex1 = SP[p + q + r, s + t]
```

$$(\overline{p}+\overline{q}+\overline{r})\cdot (\overline{s}+\overline{t})$$

or expressions like

```mathematica
ex2 = FV[p + q + r, \[Mu]]
```

$$\left(\overline{p}+\overline{q}+\overline{r}\right)^{\mu }$$

one can use

```mathematica
ExpandScalarProduct[ex1]
```

$$\overline{p}\cdot \overline{s}+\overline{p}\cdot \overline{t}+\overline{q}\cdot \overline{s}+\overline{q}\cdot \overline{t}+\overline{r}\cdot \overline{s}+\overline{r}\cdot \overline{t}$$

```mathematica
ExpandScalarProduct[ex2]
```

$$\overline{p}^{\mu }+\overline{q}^{\mu }+\overline{r}^{\mu }$$

Notice that `ExpandScalarProduct` can also do expansions only for the given momentum, while
leaving the rest of the expression untouched, e.g.

```mathematica
x SP[p1 + p2, q1 + q2] + y SP[p3 + p4, q3 + q4] + z SP[p5 + p6, q5 + q6]
ExpandScalarProduct[%, Momentum -> {p1}]
```

$$x \left((\overline{\text{p1}}+\overline{\text{p2}})\cdot (\overline{\text{q1}}+\overline{\text{q2}})\right)+y \left((\overline{\text{p3}}+\overline{\text{p4}})\cdot (\overline{\text{q3}}+\overline{\text{q4}})\right)+z \left((\overline{\text{p5}}+\overline{\text{p6}})\cdot (\overline{\text{q5}}+\overline{\text{q6}})\right)$$

$$x \left(\overline{\text{p1}}\cdot (\overline{\text{q1}}+\overline{\text{q2}})+\overline{\text{p2}}\cdot (\overline{\text{q1}}+\overline{\text{q2}})\right)+y \left((\overline{\text{p3}}+\overline{\text{p4}})\cdot (\overline{\text{q3}}+\overline{\text{q4}})\right)+z \left((\overline{\text{p5}}+\overline{\text{p6}})\cdot (\overline{\text{q5}}+\overline{\text{q6}})\right)$$

For the expansion of `Eps` tensors, we use

```mathematica
LC[][p1 + p2, q, r, s]
EpsEvaluate[%]
```

$$\bar{\epsilon }^{\overline{\text{p1}}+\overline{\text{p2}}\overline{q}\overline{r}\overline{s}}$$

$$\bar{\epsilon }^{\overline{\text{p1}}\overline{q}\overline{r}\overline{s}}+\bar{\epsilon }^{\overline{\text{p2}}\overline{q}\overline{r}\overline{s}}$$

`EpsEvaluate` also reorders the arguments of `Eps` according to its antisymmetric properties

```mathematica
LC[\[Mu], \[Sigma], \[Rho], \[Nu]]
EpsEvaluate[%]
```

$$\bar{\epsilon }^{\mu \sigma \rho \nu }$$

$$-\bar{\epsilon }^{\mu \nu \rho \sigma }$$

The inverse of `ExpandScalarProduct` is called `MomentumCombine`

```mathematica
3 FV[p, \[Mu]] + 4 FV[q, \[Mu]]
MomentumCombine[%]
```

$$3 \overline{p}^{\mu }+4 \overline{q}^{\mu }$$

$$\left(3 \overline{p}+4 \overline{q}\right)^{\mu }$$

This also works for scalar products, but the results may not be always optimal

```mathematica
SP[p + q + t, r + s]
ExpandScalarProduct[%]
MomentumCombine[%]
```

$$(\overline{r}+\overline{s})\cdot (\overline{p}+\overline{q}+\overline{t})$$

$$\overline{p}\cdot \overline{r}+\overline{p}\cdot \overline{s}+\overline{q}\cdot \overline{r}+\overline{q}\cdot \overline{s}+\overline{r}\cdot \overline{t}+\overline{s}\cdot \overline{t}$$

$$(\overline{r}+\overline{s})\cdot (\overline{p}+\overline{q}+\overline{t})$$

```mathematica
SP[p + q + t, r + s] + SP[r, s]
ExpandScalarProduct[%]
MomentumCombine[%]
```

$$(\overline{r}+\overline{s})\cdot (\overline{p}+\overline{q}+\overline{t})+\overline{r}\cdot \overline{s}$$

$$\overline{p}\cdot \overline{r}+\overline{p}\cdot \overline{s}+\overline{q}\cdot \overline{r}+\overline{q}\cdot \overline{s}+\overline{r}\cdot \overline{s}+\overline{r}\cdot \overline{t}+\overline{s}\cdot \overline{t}$$

$$(\overline{p}+\overline{q})\cdot (\overline{r}+\overline{s})+\overline{r}\cdot (\overline{s}+\overline{t})+\overline{s}\cdot \overline{t}$$

For Dirac matrices the corresponding functions are `DiracGammaExpand` and `DiracGammaCombine`

```mathematica
GA[\[Mu]] . GS[p + q] . GA[\[Nu]] . GS[r + s]
DiracGammaExpand[%]
DiracGammaCombine[%]
```

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \left(\overline{p}+\overline{q}\right)\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \left(\overline{r}+\overline{s}\right)\right)$$

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+\bar{\gamma }\cdot \overline{q}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{r}+\bar{\gamma }\cdot \overline{s}\right)$$

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \left(\overline{p}+\overline{q}\right)\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \left(\overline{r}+\overline{s}\right)\right)$$

Notice the `DiracGammaExpand` does not expand the whole noncommutative product. If you need that, use `DotSimplify`

```mathematica
GA[\[Mu]] . GS[p + q] . GA[\[Nu]] . GS[r + s]
% // DiracGammaExpand // DotSimplify
```

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \left(\overline{p}+\overline{q}\right)\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \left(\overline{r}+\overline{s}\right)\right)$$

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{r}\right)+\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{s}\right)+\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{q}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{r}\right)+\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{q}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{s}\right)$$