##  DotExpand 

DotExpand[exp] expands dot products in exp..

###  See also 

DOT, DotSimplify, DeclareNonCommutativeUnDeclareNonCommutative.

###  Examples 

```mathematica
DOT[a x + b y + c z, d + e + f] 
 
DotExpand[%] 
 
DeclareNonCommutative /@ {a, b, c, d, e, f};
DotExpand[DOT[a x + b y + c z, d + e + f]] 
 
UnDeclareNonCommutative /@ {a, b, c, d, e, f};
DotExpand[DOT[a x + b y + c z, d + e + f]]
```

$$(a x+b y+c z).(d+e+f)$$

$$a d x+a e x+a f x+b d y+b e y+b f y+c d z+c e z+c f z$$

$$x a.d+x a.e+x a.f+y b.d+y b.e+y b.f+z c.d+z c.e+z c.f$$

$$a d x+a e x+a f x+b d y+b e y+b f y+c d z+c e z+c f z$$