##  UnDeclareAllAntiCommutators 

UnDeclareAllAntiCommutators[] undeclares all user-defined anticommutators..

###  Examples 

```mathematica
DeclareNonCommutative[a, b, c, d]
AntiCommutator[a, b] = x1;
AntiCommutator[c, d] = x2;
DotSimplify[a . b . c . d] 
 
UnDeclareAllAntiCommutators[]
DotSimplify[a . b . c . d]
```

$$b.a.d.c-\text{x2} b.a-\text{x1} d.c+\text{x1} \text{x2}$$

$$a.b.c.d$$