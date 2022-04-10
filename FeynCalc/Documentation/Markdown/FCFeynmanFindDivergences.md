## FCFeynmanFindDivergences

`FCFeynmanFindDivergences[exp, vars]` identifies UV and IR divergences of the given Feynman parametric integral that arise when different parametric variables approach zero or infinity.

This function employs the analytic regularization algorithm introduced by Erik Panzer in [1403.3385](https://arxiv.org/abs/1403.3385), [1401.4361](https://arxiv.org/abs/1401.4361) and [1506.07243](https://arxiv.org/abs/1506.07243). Its current implementation is very much based on the code of the `findDivergences` routine from the Maple package [HyperInt](https://bitbucket.org/PanzerErik/hyperint/) by Erik Panzer.

The function returns a list of lists of the form `{{{x[i], x[j], ...}, {x[k], x[l], ...}, sdd}, ...}`, where
`{x[i],x[j], ...}` need to approach zero, while `{x[k], x[l], ...}` must tend towards infinity to generate the superficial degree of divergence `sdd`.

It is important to apply the function directly to the Feynman parametric integrand obtained e.g. from `FCFeynmanParametrize`. If the integrand has already been modified using variable transformations or the Cheng-Wu theorem, the  algorithm may not work properly.

Furthermore, divergences that arise inside the integration domain cannot be identified using this method.

The identified divergences can be regularized using the function `FCFeynmanRegularizeDivergence`.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md), [FCFeynmanRegularizeDivergence](FCFeynmanRegularizeDivergence.md).

### Examples

#### Feynman representation

```mathematica
int = SFAD[l, k + l, {{k, -2 k . q}}] 
 
fpar = FCFeynmanParametrize[int, {k, l}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}]
```

$$\frac{1}{(l^2+i \eta ).((k+l)^2+i \eta ).(k^2-2 (k\cdot q)+i \eta )}$$

$$\left\{(x(1) x(2)+x(3) x(2)+x(1) x(3))^{3 \varepsilon -3} \left(q^2 x(1)^2 (x(2)+x(3))\right)^{1-2 \varepsilon },-\Gamma (2 \varepsilon -1),\{x(1),x(2),x(3)\}\right\}$$

This Feynman parametric integral integrand contains logarithmic divergences for $x_1 \to \infty$ and $x_{2,3} \to 0$

```mathematica
FCFeynmanFindDivergences[fpar[[1]], x]
```

$$\left(
\begin{array}{cc}
 \{\{\},\{x(1)\}\} & \varepsilon  \\
 \{\{x(2),x(3)\},\{\}\} & \varepsilon  \\
\end{array}
\right)$$