##  SIS 

SIS[p] can be used as input for 3-dimensional $\sigma ^{\mu }p_{\mu }$ with 4-dimensional Lorentz vector p and is transformed into PauliSigma[Momentum[p]] by FeynCalcInternal..

###  Examples 

```mathematica
SIS[p] 
 
SIS[p] // FCI // StandardForm 
 
SIS[p, q, r, s] 
 
SIS[p, q, r, s] // StandardForm 
 
SIS[q] . (SIS[p] + m) . SIS[q]
```

$$\bar{\sigma }\cdot \overline{p}$$

```
(*PauliSigma[Momentum[p]]*)
```

$$\left(\bar{\sigma }\cdot \overline{p}\right).\left(\bar{\sigma }\cdot \overline{q}\right).\left(\bar{\sigma }\cdot \overline{r}\right).\left(\bar{\sigma }\cdot \overline{s}\right)$$

```
(*SIS[p] . SIS[q] . SIS[r] . SIS[s]*)
```

$$\left(\bar{\sigma }\cdot \overline{q}\right).\left(\bar{\sigma }\cdot \overline{p}+m\right).\left(\bar{\sigma }\cdot \overline{q}\right)$$