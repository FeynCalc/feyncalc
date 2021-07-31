`UnDeclareAllAntiCommutators[]` undeclares all user-defined anticommutators.

### See also

[AntiCommutator](AntiCommutator), [CommutatorExplicit](CommutatorExplicit), [DeclareNonCommutative](DeclareNonCommutative), [DotSimplify](DotSimplify).

### Examples

```mathematica
DeclareNonCommutative[a, b, c, d]
AntiCommutator[a, b] = x1;
AntiCommutator[c, d] = x2;
DotSimplify[a . b . c . d]
```

$$b.a.d.c-\text{x2} b.a-\text{x1} d.c+\text{x1} \text{x2}$$

```mathematica
UnDeclareAllAntiCommutators[]
DotSimplify[a . b . c . d]
```

$$a.b.c.d$$