##  FCAntiSymmetrize 

FCAntiSymmetrize[expr, {a1, a2, ...}] antisymmetrizes expr with respect to the variables a1,a2, ....

###  See also 

FCSymmetrize.

###  Examples 

```mathematica
FCAntiSymmetrize[f[a, b], {a, b}] 
 
FCAntiSymmetrize[f[x, y, z], {x, y, z}]
```

$$\frac{1}{2} (f(a,b)-f(b,a))$$

$$\frac{1}{6} (f(x,y,z)-f(x,z,y)-f(y,x,z)+f(y,z,x)+f(z,x,y)-f(z,y,x))$$