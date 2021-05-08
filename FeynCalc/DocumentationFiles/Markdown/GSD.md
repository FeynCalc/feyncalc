##  GSD 

GSD[p] can be used as input for a D-dimensional $p\text{ $\nmedspace $}\nmedspace \nmedspace \nmedspace \left/ \left(=\gamma .p = \gamma _{\mu }p^{\mu }\right)\right.$ and is transformed into DiracGamma[Momentum[p,D],D] by FeynCalcInternal (=FCI). GSD[p,q, ...] is a short form for GSD[p].GSD[q]. ... ..

###  See also 

DiracGamma, GA, GAD.

###  Examples 

```mathematica
GSD[p] 
 
GSD[p] // FCI // StandardForm 
 
GSD[p, q, r, s] 
 
GSD[p, q, r, s] // StandardForm 
 
GSD[q] . (GSD[p] + m) . GSD[q]
```

$$\gamma \cdot p$$

```
(*DiracGamma[Momentum[p, D], D]*)
```

$$(\gamma \cdot p).(\gamma \cdot q).(\gamma \cdot r).(\gamma \cdot s)$$

```
(*GSD[p] . GSD[q] . GSD[r] . GSD[s]*)
```

$$(\gamma \cdot q).(m+\gamma \cdot p).(\gamma \cdot q)$$