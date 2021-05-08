##  SetTemporalComponent 

SetTemporalComponent[p, val] sets the value of the temporal component of a 4-vector p, TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]] to val..

###  Examples 

```mathematica
FCClearScalarProducts[]
ClearAll[t]
SetTemporalComponent[p, t]
TC[p] 
 
TC[p + q] // ExpandScalarProduct
```

$$t$$

$$q^0+t$$