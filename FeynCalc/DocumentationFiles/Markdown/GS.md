##  GS 

GS[p] can be used as input for a 4-dimensional $p\text{ $\nmedspace $}\nmedspace \nmedspace \nmedspace \left/ \left(=\gamma .p = \gamma _{\mu }p^{\mu }\right)\right.$ and is transformed into DiracGamma[Momentum[p]] by FeynCalcInternal (=FCI). GS[p,q, ...] is a short form for GS[p].GS[q]. ... ..

###  See also 

DiracGamma, GA, GAD.

###  Examples 

```mathematica
GS[p] 
 
GS[p] // FCI // StandardForm 
 
GS[p, q, r, s] 
 
GS[p, q, r, s] // StandardForm 
 
GS[q] . (GS[p] + m) . GS[q]
```

$$\bar{\gamma }\cdot \overline{p}$$

```
(*DiracGamma[Momentum[p]]*)
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{r}\right).\left(\bar{\gamma }\cdot \overline{s}\right)$$

```
(*GS[p] . GS[q] . GS[r] . GS[s]*)
```

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}+m\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$