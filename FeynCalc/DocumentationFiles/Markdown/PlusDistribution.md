##  PlusDistribution 

PlusDistribution[1/(1 - x)] denotes a distribution (in the sense of the "+" prescription)..

###  See also 

Integrate2.

###  Examples 

```mathematica
PlusDistribution[1/(1 - x)] 
 
PlusDistribution[Log[1 - x]/(1 - x)] 
 
Integrate2[PlusDistribution[1/(1 - x)], {x, 0, 1}] 
 
Integrate2[PlusDistribution[Log[1 - x]/(1 - x)], {x, 0, 1}] 
 
Integrate2[PlusDistribution[Log[1 - x]^2/(1 - x)], {x, 0, 1}] 
 
PlusDistribution[Log[x (1 - x)]/(1 - x)]
```

$$\left(\frac{1}{1-x}\right)_+$$

$$\left(\frac{\log (1-x)}{1-x}\right)_+$$

$$0$$

$$0$$

$$0$$

$$\frac{\log (x)}{1-x}+\left(\frac{\log (1-x)}{1-x}\right)_+$$