##  UnDeclareAllCommutators 

UnDeclareAllCommutators[] undeclares all user-defined commutators..

###  Examples 

```mathematica
DeclareNonCommutative[a, b, c, d]
Commutator[a, b] = x1;
Commutator[c, d] = x2;
DotSimplify[a . b . c . d] 
 
UnDeclareAllCommutators[]
DotSimplify[a . b . c . d]
```

$$b.a.d.c+\text{x2} b.a+\text{x1} d.c+\text{x1} \text{x2}$$

$$a.b.c.d$$