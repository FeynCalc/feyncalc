##  FCLoopNonIntegerPropagatorsFreeQ 

FCLoopNonIntegerPropagatorsFreeQ[exp] checks if the integral contains propagators raised to noninteger (i.e. fractional or symbolic) powers.

###  Examples 

```mathematica
FCI@CFAD[{q + p, m^2}] 
 
FCLoopNonIntegerPropagatorPowersFreeQ[%] 
 
FCI@CFAD[{q + p, m^2, 1/2}] 
 
FCLoopNonIntegerPropagatorPowersFreeQ[%]
```

$$![1e6x5d7lsis3h](img/1e6x5d7lsis3h.png)$$

$$\text{True}$$

$$![09rsr6wa0x1j3](img/09rsr6wa0x1j3.png)$$

$$\text{False}$$