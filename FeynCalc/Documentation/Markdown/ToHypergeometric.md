## ToHypergeometric

`ToHypergeometric[t^b (1 - t)^c (1+tz)^a,t]` returns `u^a Gamma[b+1] Gamma[c+1]/Gamma[b+c+2] Hypergeometric2F1[-a,b+1,b+c+2,-z/u]`. Remember that $\textrm{Re}(b) >0$ and $\textrm{Re} (c-b) > 0$ should hold (need not be set in Mathematica).

### See also

[Overview](Extra/FeynCalc.md), [HypergeometricAC](HypergeometricAC.md), [HypergeometricIR](HypergeometricIR.md), [HypergeometricSE](HypergeometricSE.md).

### Examples

```mathematica
ToHypergeometric[t^b (1 - t)^c (1 + t z)^a, t]
```

$$\frac{\Gamma (b+1) \Gamma (c+1) \, _2F_1(-a,b+1;b+c+2;-z)}{\Gamma (b+c+2)}$$

```mathematica
ToHypergeometric[w t^(b - 1) (1 - t)^(c - b - 1) (1 - t z)^-a, t]
```

$$\frac{w \Gamma (b) \Gamma (c-b) \, _2F_1(a,b;c;z)}{\Gamma (c)}$$

```mathematica
ToHypergeometric[t^b (1 - t)^c (u + t z)^a, t]
```

$$\frac{u^a \Gamma (b+1) \Gamma (c+1) \, _2F_1\left(-a,b+1;b+c+2;-\frac{z}{u}\right)}{\Gamma (b+c+2)}$$

```mathematica
ToHypergeometric[w t^(b - 1) (1 - t)^(c - b - 1) (u - t z)^-a, t]
```

$$\frac{w u^{-a} \Gamma (b) \Gamma (c-b) \, _2F_1\left(a,b;c;\frac{z}{u}\right)}{\Gamma (c)}$$
