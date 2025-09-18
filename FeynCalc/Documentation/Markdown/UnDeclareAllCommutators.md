## UnDeclareAllCommutators

`UnDeclareAllCommutators[]` undeclares all user-defined commutators.

### See also

[Overview](Extra/FeynCalc.md), [FCCommutator](FCCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).

### Examples

```mathematica
DeclareNonCommutative[a, b, c, d] 
 
FCCommutator[a, b] = x1; 
 
FCCommutator[c, d] = x2; 
 
DotSimplify[a . b . c . d]
```

$$b.a.d.c+\text{x2} b.a+\text{x1} d.c+\text{x1} \;\text{x2}$$

```mathematica
UnDeclareAllCommutators[] 
 
DotSimplify[a . b . c . d]
```

$$a.b.c.d$$