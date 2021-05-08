##  GammaExpand 

GammaExpand[exp] rewrites Gamma[n + m] in exp (where n has Head Integer)..

###  See also 

GammaEpsilon.

###  Examples 

```mathematica
GammaExpand[Gamma[2 + Epsilon]] 
 
GammaExpand[Gamma[-3 + Epsilon]] 
 
GammaExpand[Gamma[1 + Epsilon]]
```

$$(\varepsilon +1) \Gamma (\varepsilon +1)$$

$$\frac{\Gamma (\varepsilon +1)}{(\varepsilon -3) (\varepsilon -2) (\varepsilon -1) \varepsilon }$$

$$\Gamma (\varepsilon +1)$$