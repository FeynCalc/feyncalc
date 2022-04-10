## FCFeynmanRegularizeDivergence

`FCFeynmanRegularizeDivergence[exp, div]` regularizes the divergence `div` in the Feynman parametric integral `exp`. Provided that all divergences have been regularized in this fashion,  upon expanding the integrand around $\varepsilon = 0$ one can safely integrate in the Feynman parameters.

This function uses the method of analytic regularization  introduced by Erik Panzer in [1403.3385](https://arxiv.org/abs/1403.3385), [1401.4361](https://arxiv.org/abs/1401.4361) and [1506.07243](https://arxiv.org/abs/1506.07243).

Its current implementation is very much based on the code of the `dimregPartial` routine from the Maple package [HyperInt](https://bitbucket.org/PanzerErik/hyperint/) by Erik Panzer.

Here `div` must be of the form `{{x[i], x[j], ...}, {x[k], x[l], ...}, sdd}`, where `{x[i],x[j], ...}` need to approach zero, while `{x[k], x[l], ...}` must tend towards infinity to generate the superficial degree of divergence `sdd`.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md), [FCFeynmanFindDivergences](FCFeynmanFindDivergences.md).

### Examples

```mathematica
int = SFAD[l, k + l, {{k, -2 k . q}}]
fpar = FCFeynmanParametrize[int, {k, l}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}]
```

$$\frac{1}{(l^2+i \eta ).((k+l)^2+i \eta ).(k^2-2 (k\cdot q)+i \eta )}$$

$$\left\{(x(1) x(2)+x(3) x(2)+x(1) x(3))^{3 \varepsilon -3} \left(q^2 x(1)^2 (x(2)+x(3))\right)^{1-2 \varepsilon },-\Gamma (2 \varepsilon -1),\{x(1),x(2),x(3)\}\right\}$$

This Feynman parametric integral integrand contains logarithmic divergences for $x_1 \to \infty$ and $x_{2,3} \to 0$

```mathematica
divs = FCFeynmanFindDivergences[fpar[[1]], x]
```

$$\left(
\begin{array}{cc}
 \{\{\},\{x(1)\}\} & \varepsilon  \\
 \{\{x(2),x(3)\},\{\}\} & \varepsilon  \\
\end{array}
\right)$$

Regularizing the first divergence we obtain

```mathematica
intReg = FCFeynmanRegularizeDivergence[fpar[[1]], divs[[1]]]
```

$$-\frac{3 (\varepsilon -1) q^2 x(1)^2 x(2) x(3) (x(2)+x(3)) (x(1) x(2)+x(3) x(2)+x(1) x(3))^{3 \varepsilon -4} \left(q^2 x(1)^2 (x(2)+x(3))\right)^{-2 \varepsilon }}{\varepsilon }$$

It turns out that there are no further divergences left

```mathematica
FCFeynmanFindDivergences[intReg, x]
```

$$\{\}$$

Now one can expand the integrand in `Epsilon` and perform the integration in Feynman parameters order by order in `Epsilon`

```mathematica
Series[intReg, {Epsilon, 0, 0}] // Normal
```

$$\frac{3 q^2 x(1)^2 x(2) x(3) (x(2)+x(3))}{\varepsilon  (x(1) x(2)+x(3) x(2)+x(1) x(3))^4}-\frac{3 q^2 x(1)^2 x(2) x(3) (x(2)+x(3)) \left(2 \log \left(q^2 x(1)^2 (x(2)+x(3))\right)-3 \log (x(1) x(2)+x(3) x(2)+x(1) x(3))+1\right)}{(x(1) x(2)+x(3) x(2)+x(1) x(3))^4}$$