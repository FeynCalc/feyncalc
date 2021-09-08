## HypInt

`HypInt[exp, t]` substitutes all `Hypergeometric2F1[a,b,c,z]` in `exp` with `Gamma[c]/(Gamma[b] Gamma[c-b]) Integratedx[t,0,1]  t^(b-1) (1-t)^(c-b-1) (1-t z)^(-a)`.

### See also

[Overview](Extra/FeynCalc.md), [Series2](Series2.md).

### Examples

```mathematica
Hypergeometric2F1[a, b, c, z]
HypInt[%, t]
```

$$\, _2F_1(a,b;c;z)$$

$$\frac{t^{b-1} \Gamma (c) (1-t z)^{-a} (1-t)^{-b+c-1} \int _0^1dt\, }{\Gamma (b) \Gamma (c-b)}$$
