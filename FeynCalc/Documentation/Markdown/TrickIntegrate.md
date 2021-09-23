## TrickIntegrate

`TrickIntegrate[(1 - t)^(a * Epsilon - 1) g[t], t]` does an integration trick for the definite integral of ($(1-t)^{a \;\text{Epsilon}-1}$ g[t])  from `0` to `1`, yielding  `g[1]/a/Epsilon + Hold[Integrate][(1-t)^{a Epsilon-1} (g[t]-g[1]),{t,0,1}]`

`TrickIntegrate[t^(a Epsilon-1) g[t], t]` gives $\frac{g[0]}{a \;\text{Epsilon}}$+ Hold[Integrate][$t^{a \;\text{Epsilon}-1}$ (g[t]-g[0]),{t,0,1}], provided g[1] and g[0] exist.

### See also

[Overview](Extra/FeynCalc.md), [Epsilon](Epsilon.md).

### Examples

```mathematica
TrickIntegrate[(1 - t)^(a Epsilon - 1) g[t], t]
```

$$\text{Hold}[\text{Integrate}]\left[g(t) (1-t)^{a \varepsilon -1},\{t,0,1\}\right]$$

```mathematica
TrickIntegrate[t^(a Epsilon - 1) g[t], t]
```

$$\text{Hold}[\text{Integrate}]\left[g(t) t^{a \varepsilon -1},\{t,0,1\}\right]$$
