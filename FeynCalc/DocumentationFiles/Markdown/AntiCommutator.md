##  AntiCommutator 

AntiCommutator[x, y] = c defines the anti-commutator of the non commuting objects $\text{x}$ and $\text{y}$..

###  See also 

Commutator, CommutatorExplicit, DeclareNonCommutative, DotSimplify.

###  Examples 

This declares a and b as noncommutative variables.

```mathematica
DeclareNonCommutative[a, b]
AntiCommutator[a, b] 
 
CommutatorExplicit[%] 
 
CommutatorExplicit[AntiCommutator[a + b, a - 2 b ]] 
 
DotSimplify[AntiCommutator[a + b, a - 2 b ]] 
 
DeclareNonCommutative[c, d, Overscript[c, ~], Overscript[d, ~]]

```

$$\{a,\medspace b\}$$

$$a.b+b.a$$

$$(a-2 b).(a+b)+(a+b).(a-2 b)$$

$$-a.b-b.a+2 a.a-4 b.b$$

Defining {c,d} = z results in replacements of c.d by z-d.c.

```mathematica
AntiCommutator[c, d] = z 
 
DotSimplify[ d . c . d ] 
 
AntiCommutator[Overscript[d, ~], Overscript[c, ~]] = Overscript[z, ~] 
 
DotSimplify[ Overscript[d, ~] . Overscript[c, ~] . Overscript[d, ~] ] 
 
UnDeclareNonCommutative[a, b, c, d, Overscript[c, ~], Overscript[d, ~]]
Unset[AntiCommutator[c, d]]
Unset[AntiCommutator[Overscript[d, ~], Overscript[c, ~]]]
```

$$z$$

$$c d^2$$