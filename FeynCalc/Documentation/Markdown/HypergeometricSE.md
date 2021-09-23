## HypergeometricSE

`HypergeometricSE[exp, nu]` expresses Hypergeometric functions by their series expansion in terms of a sum (the `Sum` is omitted and `nu`, running from $0$ to $\infty$, is the summation index).

### See also

[Overview](Extra/FeynCalc.md), [HypergeometricIR](HypergeometricIR.md).

### Examples

```mathematica
HypergeometricSE[Hypergeometric2F1[a, b, c, z], \[Nu]]
```

$$\frac{\Gamma (c) z^{\nu } \Gamma (a+\nu ) \Gamma (b+\nu )}{\Gamma (a) \Gamma (b) \Gamma (\nu +1) \Gamma (c+\nu )}$$

```mathematica
HypergeometricSE[HypergeometricPFQ[{a, b, c}, {d, e}, z], \[Nu]]
```

$$\frac{\Gamma (d) \Gamma (e) z^{\nu } \Gamma (a+\nu ) \Gamma (b+\nu ) \Gamma (c+\nu )}{\Gamma (a) \Gamma (b) \Gamma (c) \Gamma (\nu +1) \Gamma (d+\nu ) \Gamma (e+\nu )}$$
