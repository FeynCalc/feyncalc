## DotSimplify

`DotSimplify[exp]` expands and reorders noncommutative terms in exp. Simplifying relations may be specified by the option `DotSimplifyRelations` or by `Commutator` and `AntiCommutator` definitions. Whether exp is expanded noncommutatively depends on the option `Expanding`.

### See also

[Overview](Extra/FeynCalc.md), [AntiCommutator](AntiCommutator.md), [Commutator](Commutator.md), [Calc](Calc.md).

### Examples

```mathematica
UnDeclareAllCommutators[]
UnDeclareAllAntiCommutators[]
```

```mathematica
GA[\[Mu]] . (2 GS[p] - GS[q]) . GA[\[Nu]]
DotSimplify[%]
```

$$\bar{\gamma }^{\mu }.\left(2 \bar{\gamma }\cdot \overline{p}-\bar{\gamma }\cdot \overline{q}\right).\bar{\gamma }^{\nu }$$

$$2 \bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^{\nu }-\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{q}\right).\bar{\gamma }^{\nu }$$

```mathematica
DeclareNonCommutative[a, b, c]
a . (b - z c) . a
DotSimplify[%]
```

$$a.(b-c z).a$$

$$a.b.a-z a.c.a$$

```mathematica
Commutator[a, c] = 1
DotSimplify[a . (b - z c) . a]
```

$$1$$

$$a.b.a-z (c.a.a+a)$$

```mathematica
Commutator[a, c] =.
DotSimplify[a . (b - z c) . a]
```

$$a.b.a-z a.c.a$$

```mathematica
AntiCommutator[b, a] = c
DotSimplify[a . (b - z c) . a]
```

$$c$$

$$-a.a.b-z a.c.a+a.c$$

```mathematica
AntiCommutator[b, a] =.
DotSimplify[a . (b - z c) . a, DotSimplifyRelations -> {a . c -> 1/z}]
```

$$a.b.a-a$$

```mathematica
UnDeclareNonCommutative[a, b, c]
DeclareNonCommutative[x]
DotSimplify[x . x . x]
```

$$x.x.x$$

```mathematica
DotSimplify[x . x . x, DotPower -> True]
UnDeclareNonCommutative[x]
```

$$x^3$$

Check some relations between noncommutative expressions involving two operators $Q$ and $P$

```mathematica
DeclareNonCommutative[Q, P]
```

```mathematica
lhs = (Q . Commutator[Q, P] + Commutator[Q, P] . Q)/2
rhs = Commutator[Q, Q . P + P . Q]/2
DotSimplify[lhs - rhs]
% // ExpandAll
```

$$\frac{1}{2} (Q.[Q,P]+[Q,P].Q)$$

$$\frac{1}{2} [Q,P.Q+Q.P]$$

$$\frac{1}{2} (P.Q.Q-Q.Q.P)+\frac{1}{2} (Q.Q.P-P.Q.Q)$$

$$0$$

```mathematica
Commutator[Q, P] = I;
```

Introduce the dilation operator $D$ from the affine quantization and verify that $[Q,D]=i \hbar$ (cf. arXiv:2108.10713)

```mathematica
DOp = (Q . P + P . Q)/2;
```

```mathematica
Commutator[Q, DOp]
% // DotSimplify // ExpandAll
```

$$\left[Q,\frac{1}{2} (P.Q+Q.P)\right]$$

$$i Q$$

```mathematica
UnDeclareAllCommutators[]
UnDeclareAllAntiCommutators[]
```
