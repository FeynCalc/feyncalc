##  FCSymmetrize 

FCSymmetrize[expr, {a1, a2, ...}] symmetrizes expr with respect to the variables a1,a2, ....

###  See also 

FCAntiSymmetrize.

###  Examples 

```mathematica
FCSymmetrize[f[a, b], {a, b}] 
 
FCSymmetrize[f[x, y, z], {x, y, z}]
```

$$\frac{1}{2} (f(a,b)+f(b,a))$$

$$\frac{1}{6} (f(x,y,z)+f(x,z,y)+f(y,x,z)+f(y,z,x)+f(z,x,y)+f(z,y,x))$$