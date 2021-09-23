## HypExplicit

`HypExplicit[exp, nu]` expresses Hypergeometric functions in exp by their definition in terms of a sum (the `Sum` is omitted and `nu` is the summation index).

### See also

[Overview](Extra/FeynCalc.md), [HypergeometricIR](HypergeometricIR.md).

### Examples

```mathematica
Hypergeometric2F1[a, b, c, z]
HypExplicit[%, \[Nu]]
```

$$\, _2F_1(a,b;c;z)$$

$$\frac{\Gamma (c) z^{\nu } \Gamma (a+\nu ) \Gamma (b+\nu )}{\Gamma (a) \Gamma (b) \Gamma (\nu +1) \Gamma (c+\nu )}$$

```mathematica
HypergeometricPFQ[{a, b, c}, {d, e}, z]
HypExplicit[%, \[Nu]]
```

$$\, _3F_2(a,b,c;d,e;z)$$

$$\frac{\Gamma (d) \Gamma (e) z^{\nu } \Gamma (a+\nu ) \Gamma (b+\nu ) \Gamma (c+\nu )}{\Gamma (a) \Gamma (b) \Gamma (c) \Gamma (\nu +1) \Gamma (d+\nu ) \Gamma (e+\nu )}$$
