##  NumericQ1 

NumericQ1[x, {a, b, ..}] is like NumericQ, but assumes that {a,b,..} are numeric quantities..

###  Examples 

```mathematica
NumericQ[3 a + Log[b] + c^2] 
 
NumericQ1[3 a + Log[b] + c^2, {}] 
 
NumericQ1[3 a + Log[b] + c^2, {a, b, c}]
```

$$\text{False}$$

$$\text{False}$$

$$\text{True}$$