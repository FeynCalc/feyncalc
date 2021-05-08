##  GHP 

GHP[p, a, b] gives the ghost propagator where "a" and "b" are the color indices. GHP[p] omits the $\delta _{ab}.$.

###  See also 

GhostPropagator, GluonPropagator, GluonGhostVertex.

###  Examples 

```mathematica
GHP[p, a, b] 
 
GHP[p] // Explicit 
 
GHP[p, c1, c2] 
 
StandardForm[FCE[GHP[-k, c3, c4] // Explicit]]
```

$$\Pi _{ab}(p)$$

$$\frac{i}{p^2}$$

$$\Pi _{\text{c1}\text{c2}}(p)$$

```
(*I FAD[k] SD[c3, c4]*)
```