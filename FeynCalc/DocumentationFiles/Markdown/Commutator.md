##  Commutator 

Commutator[x, y] = c defines the commutator between the (non-commuting) objects $\text{x}$ and $\text{y}$..

###  See also 

AntiCommutator, CommutatorExplicit, DeclareNonCommutative, DotSimplify.

###  Examples 

```mathematica
DeclareNonCommutative[a, b, c, d]
Commutator[a, b] 
 
CommutatorExplicit[%] 
 
DotSimplify[Commutator[a + b, c + d]] 
 
UnDeclareNonCommutative[a, b, c, d]

```

$$[a,b]$$

$$a.b-b.a$$

$$a.c-c.a+a.d-d.a+b.c-c.b+b.d-d.b$$

Verify the Jacobi identity.

```mathematica
\[Chi] = Commutator; DeclareNonCommutative[x, y, z];
\[Chi][x, \[Chi][y, z]] + \[Chi][y, \[Chi][z, x]] + \[Chi][z, \[Chi][x, y]] 
 
DotSimplify[%] 
 
Clear[\[Chi]]
UnDeclareNonCommutative[x, y, z]
```

$$[x,[y,z]]+[y,[z,x]]+[z,[x,y]]$$

$$0$$