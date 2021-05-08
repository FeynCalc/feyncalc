##  SO 

SO[q] is a four-dimensional scalar product of OPEDelta with q. It is transformed into Pair[Momentum[q], Momentum[OPEDelta] by FCI..

###  See also 

FCI, OPEDelta, Pair, ScalarProduct, SOD.

###  Examples 

```mathematica
SO[p] 
 
SO[p - q] 
 
SO[p] // FCI // StandardForm
```

$$\Delta \cdot p$$

$$\Delta \cdot (p-q)$$

```
(*Pair[Momentum[OPEDelta], Momentum[p]]*)
```