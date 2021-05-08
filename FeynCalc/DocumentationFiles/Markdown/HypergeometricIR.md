##  HypergeometricIR 

HypergeometricIR[exp, t] substitutes for all Hypergeometric2F1[a,b,c,z] in exp by its Euler integral reprentation. The factor Integratedx[t, 0, 1] can be omitted by setting the option Integratedx -> False..

###  See also 

HypergeometricAC, HypergeometricSE, ToHypergeometric.

###  Examples 

```mathematica
HypergeometricIR[Hypergeometric2F1[a, b, c, z], t] 
 
ToHypergeometric[t^b (1 - t)^c (1 + t z)^a, t] 
 
HypergeometricIR[%, t]
```

$$\frac{t^{b-1} \Gamma (c) (1-t z)^{-a} (1-t)^{-b+c-1}}{\Gamma (b) \Gamma (c-b)}$$

$$\frac{\Gamma (b+1) \Gamma (c+1) \, _2F_1(-a,b+1;b+c+2;-z)}{\Gamma (b+c+2)}$$

$$t^b (1-t)^c (t z+1)^a$$