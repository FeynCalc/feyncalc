##  FCLoopNonIntegerPropagatorsFreeQ 

FCLoopNonIntegerPropagatorsFreeQ[exp] checks if the integral contains propagators raised to noninteger (i.e. fractional or symbolic) powers.

###  Examples 

```mathematica
FCI@CFAD[{q + p, m^2}]
FCLoopNonIntegerPropagatorPowersFreeQ[%]
```

$$\frac{1}{((p+q)^2+m^2-i \eta )}$$

$$\text{True}$$

```mathematica
FCI@CFAD[{q + p, m^2, 1/2}]
FCLoopNonIntegerPropagatorPowersFreeQ[%]
```

$$\frac{1}{\sqrt{((p+q)^2+m^2-i \eta )}}$$

$$\text{False}$$