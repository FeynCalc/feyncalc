## Commutator

`Commutator[x, y] = c` defines the commutator between the (non-commuting) objects `x` and `y`.

### See also

[Overview](Extra/FeynCalc.md), [AntiCommutator](AntiCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).

### Examples

```mathematica
DeclareNonCommutative[a, b, c, d]
```

```mathematica
Commutator[a, b]
CommutatorExplicit[%]
```

$$[a,b]$$

$$a.b-b.a$$

```mathematica
DotSimplify[Commutator[a + b, c + d]] 
UnDeclareNonCommutative[a, b, c, d]
```

$$a.c-c.a+a.d-d.a+b.c-c.b+b.d-d.b$$

Verify the Jacobi identity.

```mathematica
\[Chi] = Commutator; DeclareNonCommutative[x, y, z];
```

```mathematica
\[Chi][x, \[Chi][y, z]] + \[Chi][y, \[Chi][z, x]] + \[Chi][z, \[Chi][x, y]]
DotSimplify[%]
```

$$[x,[y,z]]+[y,[z,x]]+[z,[x,y]]$$

$$0$$

```mathematica
Clear[\[Chi]]
UnDeclareNonCommutative[x, y, z]
```
