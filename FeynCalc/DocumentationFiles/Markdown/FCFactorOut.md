##  FCFactorOut 

FCFactorOut[exp, pref] factors out $\text{pref}$ out of exp. This is often need to bring exp into a particular form that Mathematica refuses to give..

###  Examples 

```mathematica
FCFactorOut[(a + 3 b), 3 b] 
 
FCFactorOut[(a + 3 b), 3 b, Head -> hold]
```

$$3 b \left(\frac{a}{3 b}+1\right)$$

$$3 b \text{hold}\left(\frac{a}{3 b}+1\right)$$