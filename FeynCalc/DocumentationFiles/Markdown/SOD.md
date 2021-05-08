##  SOD 

SOD[q] is a D-dimensional scalar product of OPEDelta with q. It is transformed into Pair[Momentum[q,D], Momentum[OPEDelta,D] by FeynCalcInternal..

###  See also 

OPEDelta, Pair, ScalarProduct, SOD.

###  Examples 

```mathematica
SOD[p] 
 
SOD[p - q] 
 
SOD[p] // FCI // StandardForm
```

$$\Delta \cdot p$$

$$\Delta \cdot (p-q)$$

```
(*Pair[Momentum[OPEDelta, D], Momentum[p, D]]*)
```