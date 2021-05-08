##  QuarkPropagator 

QuarkPropagator[p] is the massless quark propagator.   QuarkPropagator[{p, m}] gives the quark propagator with mass $\text{m}$.$text{QP}$ can be used as an abbreviation of QuarkPropagator..

###  See also 

GluonPropagator, QuarkGluonVertex.

###  Examples 

```mathematica
QuarkPropagator[p, Explicit -> True] 
 
QuarkPropagator[{p, m}, Explicit -> True] 
 
QP[{p, m}] 
 
Explicit[%]
```

$$\frac{i \gamma \cdot p}{p^2}$$

$$\frac{i (m+\gamma \cdot p)}{p^2-m^2}$$

$$\Pi _q(p)$$

$$\frac{i (m+\gamma \cdot p)}{p^2-m^2}$$