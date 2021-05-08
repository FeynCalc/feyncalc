##  Factor1 

Factor1[poly] factorizes common terms  in the summands of poly. It uses basically PolynomialGCD..

###  See also 

Factor2.

###  Examples 

```mathematica
(a - x) (b - x) 
 
{Factor1[%], Factor[%]} 
 
Expand[(a - b) (a + b)] 
 
Factor[%] 
 
Factor1[%%]
```

$$(a-x) (b-x)$$

$$\{(a-x) (b-x),-((a-x) (x-b))\}$$

$$a^2-b^2$$

$$(a-b) (a+b)$$

$$a^2-b^2$$