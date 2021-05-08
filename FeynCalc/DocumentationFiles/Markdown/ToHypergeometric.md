##  ToHypergeometric 

ToHypergeometric[t^b (1 - t)^c (1+tz)^a,t] returns $\text{Null}$. Remember that Re b >0 and Re (c-b) > 0 should hold (need not be set in Mathematica)..

###  See also 

HypergeometricAC, HypergeometricIR, HypergeometricSE.

###  Examples 

```mathematica
ToHypergeometric[t^b (1 - t)^c (1 + t z)^a, t] 
 
ToHypergeometric[w t^(b - 1) (1 - t)^(c - b - 1) (1 - t z)^-a, t] 
 
ToHypergeometric[t^b (1 - t)^c (u + t z)^a, t] 
 
ToHypergeometric[w t^(b - 1) (1 - t)^(c - b - 1) (u - t z)^-a, t]
```

$$\frac{\Gamma (b+1) \Gamma (c+1) \, _2F_1(-a,b+1;b+c+2;-z)}{\Gamma (b+c+2)}$$

$$\frac{w \Gamma (b) \Gamma (c-b) \, _2F_1(a,b;c;z)}{\Gamma (c)}$$

$$\frac{u^a \Gamma (b+1) \Gamma (c+1) \, _2F_1\left(-a,b+1;b+c+2;-\frac{z}{u}\right)}{\Gamma (b+c+2)}$$

$$\frac{w u^{-a} \Gamma (b) \Gamma (c-b) \, _2F_1\left(a,b;c;\frac{z}{u}\right)}{\Gamma (c)}$$