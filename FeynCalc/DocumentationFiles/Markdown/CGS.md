##  CGS 

CGS[p] is transformed into DiracGamma[CartesianMomentum[p]] by FeynCalcInternal. CGS[p,q, ...] is equivalent to CGS[p].CGS[q]. ....

###  See also 

GS, DiracGamma.

###  Examples 

```mathematica
CGS[p] 
 
CGS[p] // FCI // StandardForm 
 
CGS[p, q, r, s] 
 
CGS[p, q, r, s] // StandardForm 
 
CGS[q] . (CGS[p] + m) . CGS[q]
```

$$\overline{\gamma }\cdot \overline{p}$$

```
(*DiracGamma[CartesianMomentum[p]]*)
```

$$\left(\overline{\gamma }\cdot \overline{p}\right).\left(\overline{\gamma }\cdot \overline{q}\right).\left(\overline{\gamma }\cdot \overline{r}\right).\left(\overline{\gamma }\cdot \overline{s}\right)$$

```
(*CGS[p] . CGS[q] . CGS[r] . CGS[s]*)
```

$$\left(\overline{\gamma }\cdot \overline{q}\right).\left(\overline{\gamma }\cdot \overline{p}+m\right).\left(\overline{\gamma }\cdot \overline{q}\right)$$