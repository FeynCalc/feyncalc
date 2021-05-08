##  DeltaFunctionPrime 

DeltaFunctionPrime[1 - x] is the derivative of the Dirac delta-function $\delta (x)$..

###  See also 

Convolute, DeltaFunction, DeltaFunctionDoublePrime, Integrate2, SimplifyDeltaFunction.

###  Examples 

```mathematica
DeltaFunctionPrime[1 - x] 
 
Integrate2[DeltaFunctionPrime[1 - x] f[x], {x, 0, 1}] 
 
Integrate2[DeltaFunctionPrime[1 - x] x^2, {x, 0, 1}]
```

$$\delta '(1-x)$$

$$f'(1)$$

$$2$$