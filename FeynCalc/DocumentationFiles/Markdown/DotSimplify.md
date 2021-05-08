##  DotSimplify 

DotSimplify[exp] expands and reorders noncommutative terms in exp. Simplifying relations may be specified by the option DotSimplifyRelations or by Commutator and AntiCommutator definitions. Whether exp is expanded noncommutatively depends on the option Expanding..

###  See also 

AntiCommutator, Commutator, Calc.

###  Examples 

```mathematica
GA[\[Mu]] . (2 GS[p] - GS[q]) . GA[\[Nu]] 
 
DotSimplify[%] 
 
DeclareNonCommutative[a, b, c]
a . (b - z c) . a 
 
DotSimplify[%] 
 
Commutator[a, c] = 1 
 
DotSimplify[a . (b - z c) . a] 
 
Commutator[a, c] =.
DotSimplify[a . (b - z c) . a] 
 
AntiCommutator[b, a] = c 
 
DotSimplify[a . (b - z c) . a] 
 
AntiCommutator[b, a] =.
DotSimplify[a . (b - z c) . a, DotSimplifyRelations -> {a . c -> 1/z}] 
 
UnDeclareNonCommutative[a, b, c]
DeclareNonCommutative[x]
DotSimplify[x . x . x] 
 
DotSimplify[x . x . x, DotPower -> True] 
 
UnDeclareNonCommutative[x]
```

$$\bar{\gamma }^{\mu }.\left(2 \bar{\gamma }\cdot \overline{p}-\bar{\gamma }\cdot \overline{q}\right).\bar{\gamma }^{\nu }$$

$$2 \bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^{\nu }-\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{q}\right).\bar{\gamma }^{\nu }$$

$$a.(b-c z).a$$

$$a.b.a-z a.c.a$$

$$1$$

$$a.b.a-z (c.a.a+a)$$

$$a.b.a-z a.c.a$$

$$c$$

$$-a.a.b-z a.c.a+a.c$$

$$a.b.a-a$$

$$x.x.x$$

$$x^3$$