## Convolute

`Convolute[f, g, x]` convolutes $f(x)$ and $g(x)$, i.e., $\int _0^1 dx_1 \int _0^1 dx_2  \delta \left(x - x_1 x_2\right) f (x_1)  g(x_2)$.

`Convolute[f, g]` is equivalent to `Convolute[f, g, x]`.

`Convolute[exp, {x1, x2}]` assumes that `exp` is polynomial in `x1` and `x2`. Convolute uses table-look-up and does not do any integral calculations, only linear algebra.

### See also

[Overview](Extra/FeynCalc.md), [PlusDistribution](PlusDistribution.md), [ConvoluteTable](ConvoluteTable.md).

### Examples

```mathematica
Convolute[1, 1] /. FCGV[z_] :> ToExpression[z]
```

$$-\log (x)$$

```mathematica
Convolute[x, x] /. FCGV[z_] :> ToExpression[z]
```

$$-x^2 \log (x)$$

```mathematica
Convolute[1, x] /. FCGV[z_] :> ToExpression[z]
```

$$-x \log (x)$$

```mathematica
Convolute[1, 1/(1 - x)] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{\log (x)}{x-1}$$

```mathematica
Convolute[1, PlusDistribution[1/(1 - x)]] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{\log (x)}{x-1}$$

```mathematica
Convolute[1/(1 - x), x] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{x \log (x)}{x-1}$$

```mathematica
Convolute[1/(1 - x), 1/(1 - x)] /. FCGV[z_] :> ToExpression[z]
```

$$-\frac{\log (x)}{(x-1)^2}$$

```mathematica
Convolute[1, Log[1 - x]] /. FCGV[z_] :> ToExpression[z]
```

$$-\log (1-x) \log (x)$$

```mathematica
Convolute[1, x Log[1 - x]] /. FCGV[z_] :> ToExpression[z]
```

$$-x \log (1-x) \log (x)$$

```mathematica
Convolute[1/(1 - x), Log[1 - x]] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{\log (1-x) \log (x)}{x-1}$$

```mathematica
Convolute[1/(1 - x), x Log[1 - x]] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{x \log (1-x) \log (x)}{x-1}$$

```mathematica
Convolute[Log[1 - x]/(1 - x), x] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{x \log (1-x) \log (x)}{x-1}$$

```mathematica
Convolute[1, x Log[x]] /. FCGV[z_] :> ToExpression[z]
```

$$-x \log ^2(x)$$

```mathematica
Convolute[Log[1 - x], x] /. FCGV[z_] :> ToExpression[z]
```

$$-x \log (1-x) \log (x)$$

```mathematica
Convolute[1/(1 - x), Log[x]/(1 - x)] /. FCGV[z_] :> ToExpression[z]
```

$$-\frac{\log ^2(x)}{(x-1)^2}$$

```mathematica
Convolute[1, Log[x]] /. FCGV[z_] :> ToExpression[z]
```

$$-\log ^2(x)$$

```mathematica
Convolute[x, x Log[x]] /. FCGV[z_] :> ToExpression[z]
```

$$-x^2 \log ^2(x)$$

```mathematica
Convolute[1/(1 - x), Log[x]] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{\log ^2(x)}{x-1}$$

```mathematica
Convolute[1, Log[x]/(1 - x)] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{\log ^2(x)}{x-1}$$

```mathematica
Convolute[1/(1 - x), x Log[x]] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{x \log ^2(x)}{x-1}$$

```mathematica
Convolute[Log[x]/(1 - x), x] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{x \log ^2(x)}{x-1}$$

```mathematica
Convolute[1, x Log[x]] /. FCGV[z_] :> ToExpression[z]
```

$$-x \log ^2(x)$$

```mathematica
Convolute[Log[x], x] /. FCGV[z_] :> ToExpression[z]
```

$$-x \log ^2(x)$$

```mathematica
Convolute[1/(1 - x), Log[1 - x]/(1 - x)] /. FCGV[z_] :> ToExpression[z]
```

$$-\frac{\log (1-x) \log (x)}{(x-1)^2}$$

```mathematica
Convolute[Log[1 - x]/(1 - x), Log[1 - x]] /. FCGV[z_] :> ToExpression[z]
```

$$\frac{\log ^2(1-x) \log (x)}{x-1}$$
