##  CommutatorExplicit 

CommutatorExplicit[exp] substitutes any Commutator and AntiCommutator in exp by their definitions..

###  See also 

Calc, DotSimplify.

###  Examples 

```mathematica
DeclareNonCommutative[a, b, c, d]
Commutator[a, b] 
 
CommutatorExplicit[%] 
 
AntiCommutator[a - c, b - d] 
 
CommutatorExplicit[%] 
 
CommutatorExplicit[%%] // DotSimplify 
 
UnDeclareNonCommutative[a, b, c, d]
```

$$[a,b]$$

$$a.b-b.a$$

$$\{a-c,\medspace b-d\}$$

$$(a-c).(b-d)+(b-d).(a-c)$$

$$a.b+b.a-a.d-d.a-b.c-c.b+c.d+d.c$$