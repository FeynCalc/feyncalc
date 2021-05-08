##  FCLoopIBPReducableQ 

FCLoopIBPReducableQ[int] checks if the integral contains propagators raised to integer powers..

###  Examples 

```mathematica
FAD[q, q - p] 
 
FCLoopIBPReducableQ[FCI[%]] 
 
FAD[{q, 0, 2}, q - p] 
 
FCLoopIBPReducableQ[FCI[%]]
```

$$\frac{1}{q^2.(q-p)^2}$$

$$\text{False}$$

$$\frac{1}{\left(q^2\right)^2.(q-p)^2}$$

$$\text{True}$$