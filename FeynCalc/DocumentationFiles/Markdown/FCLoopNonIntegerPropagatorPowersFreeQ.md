##  FCLoopNonIntegerPropagatorPowersFreeQ 

FCLoopNonIntegerPropagatorPowersFreeQ[int] checks if the integral contains propagators raised to noninteger (i.e. fractional or symbolic) powers..

###  Examples 

```mathematica
SFAD[{q + p, m^2, 2}] 
 
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]] 
 
SFAD[{q + p, m^2, n}] 
 
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]] 
 
CFAD[{l, m^2, 1/2}] 
 
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]]
```

$$![02ybfw73gwd1e](img/02ybfw73gwd1e.png)$$

$$\text{True}$$

$$![1lz13quj275py](img/1lz13quj275py.png)$$

$$\text{False}$$

$$![161b2xp1cgvxf](img/161b2xp1cgvxf.png)$$

$$\text{False}$$