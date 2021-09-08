## HypergeometricIR

`HypergeometricIR[exp, t]` substitutes for all `Hypergeometric2F1[a,b,c,z]` in `exp` by its Euler integral representation. The factor `Integratedx[t, 0, 1]` can be omitted by setting the option `Integratedx -> False`.

### See also

[Overview](Extra/FeynCalc.md), [HypergeometricAC](HypergeometricAC.md), [HypergeometricSE](HypergeometricSE.md), [ToHypergeometric](ToHypergeometric.md).

### Examples

```mathematica
HypergeometricIR[Hypergeometric2F1[a, b, c, z], t]
```

$$\frac{t^{b-1} \Gamma (c) (1-t z)^{-a} (1-t)^{-b+c-1}}{\Gamma (b) \Gamma (c-b)}$$

```mathematica
ToHypergeometric[t^b (1 - t)^c (1 + t z)^a, t]
HypergeometricIR[%, t] 
  
 

```

$$\frac{\Gamma (b+1) \Gamma (c+1) \, _2F_1(-a,b+1;b+c+2;-z)}{\Gamma (b+c+2)}$$

$$t^b (1-t)^c (t z+1)^a$$
