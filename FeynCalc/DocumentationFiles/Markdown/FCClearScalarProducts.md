##  FCClearScalarProducts 

[]   FCClearScalarProducts[] removes all user-performed specific settings for ScalarProduct's..

###  See also 

ScalarProduct, Pair, SP, SPD.

###  Examples 

```mathematica
ScalarProduct[p, p] = m^2 
 
Pair[Momentum[p], Momentum[p]] 
 
FCClearScalarProducts[]
Pair[Momentum[p], Momentum[p]] 
 
SP[p, p]
```

$$m^2$$

$$m^2$$

$$\overline{p}^2$$

$$\overline{p}^2$$