##  FCSplit 

FCSplit[exp, {v1, v2, ...}] splits expr into pieces that are free of any occurence of v1, v2, ... and pieces that contain those variables. This works both on sums and products. The output is provided in the form of a two element list. One can recover the original expression by applying Total to that list..

###  Examples 

```mathematica
FCSplit[(a + b)^2, {a}] 
 
FCSplit[(a + b + c)^2, {a, b}]
```

$$\left\{b^2,a^2+2 a b\right\}$$

$$\left\{c^2,a^2+2 a b+2 a c+b^2+2 b c\right\}$$