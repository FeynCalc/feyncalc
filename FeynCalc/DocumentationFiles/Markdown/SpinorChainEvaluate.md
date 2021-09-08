## SpinorChainEvaluate

`SpinorChainEvaluate[exp]` explicitly evaluates suitable spinor chains, i.e. it replaces a `DOT[Spinor[...],...,Spinor[...]]` with a scalar quantity without a DOT.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
ex = SpinorUBar[p, m] . SpinorU[p, m]
SpinorChainEvaluate[ex]
```

$$\bar{u}(p,m).u(p,m)$$

$$2 m$$

```mathematica
SpinorChainEvaluate[ex, DiracSpinorNormalization -> "Nonrelativistic"]
```

$$\frac{m}{p^0}$$

```mathematica
SpinorChainEvaluate[ex, DiracSpinorNormalization -> "Rest"]
```

$$1$$

```mathematica
ex = SpinorUBarD[p, m] . GA[5] . SpinorUD[p, m]
SpinorChainEvaluate[ex]
```

$$\bar{u}(p,m).\bar{\gamma }^5.u(p,m)$$

$$0$$

```mathematica
FCSetDiracGammaScheme["BMHV"]
SpinorChainEvaluate[ex]
```

$$\text{BMHV}$$

$$(\varphi (p,m)).\bar{\gamma }^5.(\varphi (p,m))$$
