## CommutatorExplicit

`CommutatorExplicit[exp]` substitutes any `Commutator` and `AntiCommutator` in `exp` by their definitions.

### See also

[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [DotSimplify](DotSimplify.md).

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
AntiCommutator[a - c, b - d]
CommutatorExplicit[%]
```

$$\{a-c,\medspace b-d\}$$

$$(a-c).(b-d)+(b-d).(a-c)$$

```mathematica
CommutatorExplicit[AntiCommutator[a - c, b - d]] // DotSimplify
```

$$a.b+b.a-a.d-d.a-b.c-c.b+c.d+d.c$$

```mathematica
UnDeclareNonCommutative[a, b, c, d]
```
