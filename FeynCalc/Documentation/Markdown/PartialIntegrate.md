## PartialIntegrate

`PartialIntegrate[exp, ap, t]` does a partial integration of the definite integral `Integrate[exp,{t,0,1}]`, with `ap` the factor that is to be integrated and `exp/ap` the factor that is to be differentiated.

### See also

[Overview](Extra/FeynCalc.md), [IntegrateByParts](IntegrateByParts.md), [Integrate2](Integrate2.md).

### Examples

```mathematica
PartialIntegrate[f[x] g[x], g[x], {x, 0, 1}]
```

$$-(f(x) \int g(x) \, dx\text{/.}\, x\to 0)+(f(x) \int g(x) \, dx\text{/.}\, x\to 1)-\int_0^1 f'(x) (\int g(x) \, dx) \, dx$$

```mathematica
f[x_] = Integrate[Log[3 x + 2], x]
g[x_] = D[1/Log[3 x + 2], x]
```

$$\left(x+\frac{2}{3}\right) \log (3 x+2)-x$$

$$-\frac{3}{(3 x+2) \log ^2(3 x+2)}$$

```mathematica
Integrate[PartialIntegrate[f[x] g[x], f[x], x], {x, 0, 1}] // FullSimplify
```

$$-\frac{1}{\log (5)}$$

```mathematica
Integrate[f[x] g[x], {x, 0, 1}] // Simplify
```

$$-\frac{1}{\log (5)}$$

```mathematica
Clear[f, g]
```
