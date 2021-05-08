##  FCProductSplit 

FCProductSplit[exp, {v1, v2, ...}] splits expr into pieces that are free of any occurence of v1, v2, ... and pieces that contain those variables. This works both on sums and products. The output is provided in the form of a two element list. One can recover the original expression by applying Total to that list..

###  Examples 

```mathematica
FCProductSplit[c^2, {a}] 
 
FCProductSplit[a^2*b, {a}] 
 
FCProductSplit[(a^2 + b)*b*(c + d), {a, c}]
```

$$\left\{c^2,1\right\}$$

$$\left\{b,a^2\right\}$$

$$\left\{b,\left(a^2+b\right) (c+d)\right\}$$