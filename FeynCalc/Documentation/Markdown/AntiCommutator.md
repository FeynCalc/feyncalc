## AntiCommutator

`AntiCommutator[x, y] = c` defines the anti-commutator of the non commuting objects `x` and `y`.

### See also

[Overview](Extra/FeynCalc.md), [Commutator](Commutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).

### Examples

This declares `a` and `b` as noncommutative variables.

```mathematica
DeclareNonCommutative[a, b]
AntiCommutator[a, b]
CommutatorExplicit[%]
```

$$\{a,\medspace b\}$$

$$a.b+b.a$$

```mathematica
CommutatorExplicit[AntiCommutator[a + b, a - 2 b ]]
```

$$(a-2 b).(a+b)+(a+b).(a-2 b)$$

```mathematica
DotSimplify[AntiCommutator[a + b, a - 2 b ]]
```

$$-a.b-b.a+2 a.a-4 b.b$$

```mathematica
DeclareNonCommutative[c, d, ct, dt]
```

Defining `{c,d} = z` results in replacements of `c.d` by `z-d.c.`

```mathematica
AntiCommutator[c, d] = z
DotSimplify[ d . c . d ]
```

$$z$$

$$d z-d.d.c$$

```mathematica
AntiCommutator[dt, ct] = zt
```

$$\text{zt}$$

```mathematica
DotSimplify[dt . ct . dt]
```

$$\text{dt} \;\text{zt}-\text{ct}.\text{dt}.\text{dt}$$

```mathematica
UnDeclareNonCommutative[a, b, c, d, ct, dt]
UnDeclareAllAntiCommutators[]
```
