##  Variables2 

Variables2[expr] is like Variables, but it also works on rules and equalities as well as lists thereof. Variables2 always applies Union to the output..

###  Examples 

```mathematica
Variables[{a -> x1 + y1, b -> x2 + y2}] 
 
Variables2[{a -> x1 + y1, b -> x2 + y2}] 
 
Variables[a + b == c + d] 
 
Variables2[a + b == c + d]
```

$$\{\}$$

$$\{a,b,\text{x1},\text{x2},\text{y1},\text{y2}\}$$

$$\{\}$$

$$\{a,b,c,d\}$$