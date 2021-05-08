##  FunctionalD 

FunctionalD[exp, {QuantumField[name, LorentzIndex[mu], ..., SUNIndex[a]][p], ...}] calculates the functional derivative of exp with respect to the QuantumField list (with incoming momenta $\text{p}$, etc.) and does the Fourier transform.   FunctionalD[expr, {QuantumField[name, LorentzIndex[mu], ... SUNIndex[a]], ...}] calculates the functional derivative and does partial integration but omits the $\text{x}$-space delta functions.FunctionalD is a low level function used in FeynRule..

###  See also 

FeynRule, QuantumField.

###  Examples 

Instead of the usual $delta phi (x)/delta phi (y)=delta ^{(D)}(x-y)$ the arguments and the $delta$ function are omitted, i.e., for the program for simplicity: $delta phi /delta phi =1$.

```mathematica
FunctionalD[QuantumField[\[Phi]], QuantumField[\[Phi]]] 
 
FunctionalD[QuantumField[\[Phi]]^2, QuantumField[\[Phi]]]
```

$$1$$

$$2 \phi$$

Instead of the usual  $left.left(delta  partial _{mu }phi (x)right)right/delta phi (y)=partial _{mu }delta ^{(D)}(x-y)$ the arguments are omitted, and the $text{cell}left(text{TextData}left[text{cell}left(partial _{mu },text{Input}right)right],text{InlineFormula}right)$ operator is specified by default to be an integration by parts operator, i.e., the right hand side will be just $text{Null}$ or, more precisely, (by default)$-vec{partial }_{mu }$.

```mathematica
FunctionalD[QuantumField[FCPartialD[\[Mu]], \[Phi]], QuantumField[\[Phi]]] 
 
$S[\phi ] = 1/2\int  d^Dx \left[ \partial _ {\mu }\phi (x) \partial ^{\mu }\phi (x) - m^2\phi (x) \phi (y) \r ight] $
(QuantumField[FCPartialD[\[Mu]], \[Phi]] . QuantumField[FCPartialD[\[Mu]], \[Phi]] - m^2 QuantumField[\[Phi]] . QuantumField[\[Phi]])/2 
 
FunctionalD[%, QuantumField[\[Phi]]] 
 
$S[A] = -\int  d^Dx \frac {1} {4} F_a^{\mu \n u } (x) F_ {\text {$\mu \n u $a}} (x) $

```

$$-\vec{\partial }_{\mu }$$

First approach:

```mathematica
F1 = FieldStrength[\[Mu], \[Nu], a, {A, b, c}, 1, Explicit -> True] 
 
F2 = FieldStrength[\[Mu], \[Nu], a, {A, d, e}, 1, Explicit -> True] 
 
S[A] = -1/4 F1 . F2
```

$$f^{abc} A_{\mu }^b.A_{\nu }^c+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)$$

$$f^{ade} A_{\mu }^d.A_{\nu }^e+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)$$

$$-\frac{1}{4} \left(f^{abc} A_{\mu }^b.A_{\nu }^c+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)\right).\left(f^{ade} A_{\mu }^d.A_{\nu }^e+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)\right)$$

In order to derive the equation of motion, the functional derivative of $text{S}$ with respect to $A_{sigma }^g$ has to be set to zero. Bearing in mind that for FeynCalc we have to be precise as to where which operators (coming from the substitution of the derivative of the delta function) act.

Act with the functional derivative operator on the first field strength:

```mathematica
$\left .0 = (\text {$\delta $S})\left/\left (\delta A_ {\sigma }^g (y) \r ight) \r ight . = -2/4\int d^Dx \left (\delta \left/\left (\delta A_ {\sigma }^g (y) \r ight) \r ight . \r ight . F_ {\text {$\mu \n u $a}} (x) \r ight) F_a^{, \mu \n u } (x) $

```

 See what happens with just $(text{$delta $S}[A])left/left(delta A_{sigma }^gright)right.$.

```mathematica
Ag = QuantumField[A, {\[Sigma]}, {g}] 
 
FunctionalD[F1, Ag]
```

$$A_{\sigma }^g$$

$$A_{\mu }^b \bar{g}^{\nu \sigma } f^{abg}-A_{\nu }^c \bar{g}^{\mu \sigma } f^{acg}+\delta ^{ag} \vec{\partial }_{\mu } \left(-\bar{g}^{\nu \sigma }\right)+\delta ^{ag} \vec{\partial }_{\nu } \bar{g}^{\mu \sigma }$$

Use FCCanonicalizeDummyIndices to minimize the number of dummy indices.

```mathematica
t1 = FCCanonicalizeDummyIndices[%, SUNIndexNames -> {c1}] /. c1 -> c
```

$$A_{\mu }^c \bar{g}^{\nu \sigma } f^{acg}-A_{\nu }^c \bar{g}^{\mu \sigma } f^{acg}+\delta ^{ag} \vec{\partial }_{\mu } \left(-\bar{g}^{\nu \sigma }\right)+\delta ^{ag} \vec{\partial }_{\nu } \bar{g}^{\mu \sigma }$$

Instead of inserting the definition for the second $F_a^{mu nu }$, introduce a QuantumField object with antisymmetry built into the Lorentz indices:

```mathematica
F /: QuantumField[pard___, F, \[Beta]_, \[Alpha]_, s_] := -QuantumField[pard, F, \[Alpha], \[Beta], s] /; ! OrderedQ[{\[Beta], \[Alpha]}]
QuantumField[F, {\[Mu], \[Nu]}, {a}] 
 
% /. {\[Mu] :> \[Nu], \[Nu] :> \[Mu]} 
 
t2 = Contract[ExpandPartialD[-1/2 t1 . QuantumField[F, LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], SUNIndex[a]]]] /. Dot -> Times 
 
t3 = FCCanonicalizeDummyIndices[t2, LorentzIndexNames -> {mu}, SUNIndexNames -> {aa, cc}] /. {mu -> \[Mu], aa -> a, cc -> c} 
 
t4 = FCE[t3] /. SUNF[a, c, g] -> -SUNF[g, c, a]
```

$$F_{\mu \nu }^a$$

$$-F_{\mu \nu }^a$$

$$-\frac{1}{2} A_{\mu }^c f^{acg} F_{\mu \sigma }^a-\frac{1}{2} A_{\nu }^c f^{acg} F_{\nu \sigma }^a+\frac{1}{2} \left(\left.(\partial _{\mu }F_{\mu \sigma }^g\right)\right)+\frac{1}{2} \left(\left.(\partial _{\nu }F_{\nu \sigma }^g\right)\right)$$

$$A_{\mu }^a f^{acg} F_{\mu \sigma }^c+\left.(\partial _{\mu }F_{\mu \sigma }^g\right)$$

$$\left.(\partial _{\mu }F_{\mu \sigma }^g\right)-A_{\mu }^a f^{gca} F_{\mu \sigma }^c$$

Since the variational derivative vanishes $text{t4}$ implies that $0=D_{mu }F_g^{mu sigma }$.

Second approach:

It is of course also possible to do the functional deriviate on the $S[A]$ with both field strength tensors inserted.

```mathematica
S[A] 
 
r1 = FunctionalD[S[A], Ag]
```

$$-\frac{1}{4} \left(f^{abc} A_{\mu }^b.A_{\nu }^c+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)\right).\left(f^{ade} A_{\mu }^d.A_{\nu }^e+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)\right)$$

$$-\frac{1}{4} f^{abc} f^{adg} A_{\mu }^b.A_{\sigma }^c.A_{\mu }^d+\frac{1}{4} f^{abc} f^{aeg} A_{\sigma }^b.A_{\nu }^c.A_{\nu }^e-\frac{1}{4} f^{abg} f^{ade} A_{\mu }^b.A_{\mu }^d.A_{\sigma }^e-\frac{1}{4} f^{abg} A_{\mu }^b.\left(\left.(\partial _{\mu }A_{\sigma }^a\right)\right)+\frac{1}{4} f^{abg} A_{\mu }^b.\left(\left.(\partial _{\sigma }A_{\mu }^a\right)\right)+\frac{1}{4} f^{acg} f^{ade} A_{\nu }^c.A_{\sigma }^d.A_{\nu }^e-\frac{1}{4} f^{acg} A_{\nu }^c.\left(\left.(\partial _{\nu }A_{\sigma }^a\right)\right)+\frac{1}{4} f^{acg} A_{\nu }^c.\left(\left.(\partial _{\sigma }A_{\nu }^a\right)\right)-\frac{1}{4} f^{adg} \left(\left.(\partial _{\mu }A_{\sigma }^a\right)\right).A_{\mu }^d+\frac{1}{4} f^{adg} \left(\left.(\partial _{\sigma }A_{\mu }^a\right)\right).A_{\mu }^d-\frac{1}{4} f^{aeg} \left(\left.(\partial _{\nu }A_{\sigma }^a\right)\right).A_{\nu }^e+\frac{1}{4} f^{aeg} \left(\left.(\partial _{\sigma }A_{\nu }^a\right)\right).A_{\nu }^e+\frac{1}{4} f^{bcg} A_{\mu }^b.\left(\left.(\partial _{\mu }A_{\sigma }^c\right)\right)+\frac{1}{4} f^{bcg} \left(\left.(\partial _{\mu }A_{\mu }^b\right)\right).A_{\sigma }^c-\frac{1}{4} f^{bcg} A_{\sigma }^b.\left(\left.(\partial _{\nu }A_{\nu }^c\right)\right)-\frac{1}{4} f^{bcg} \left(\left.(\partial _{\nu }A_{\sigma }^b\right)\right).A_{\nu }^c+\frac{1}{4} f^{deg} A_{\mu }^d.\left(\left.(\partial _{\mu }A_{\sigma }^e\right)\right)+\frac{1}{4} f^{deg} \left(\left.(\partial _{\mu }A_{\mu }^d\right)\right).A_{\sigma }^e-\frac{1}{4} f^{deg} A_{\sigma }^d.\left(\left.(\partial _{\nu }A_{\nu }^e\right)\right)-\frac{1}{4} f^{deg} \left(\left.(\partial _{\nu }A_{\sigma }^d\right)\right).A_{\nu }^e+\frac{1}{2} \left(\partial _{\mu }\partial _{\mu }A_{\sigma }^g\right)-\frac{1}{2} \left(\partial _{\mu }\partial _{\sigma }A_{\mu }^g\right)+\frac{1}{2} \left(\partial _{\nu }\partial _{\nu }A_{\sigma }^g\right)-\frac{1}{2} \left(\partial _{\nu }\partial _{\sigma }A_{\nu }^g\right)$$

This is just funcional derivation and partial integration and simple contraction of indices. At first no attempt is made to rename dummy indices (since this is difficult in general).

With a general replacement rule only valid for commuting fields the color indices can be canonicalized a bit more. The idea is to use the commutative properties of the vector fields, and canonicalize the color indices by a trick.

```mathematica
Commutator[QuantumField[aaa___FCPartialD, A, bbb__], QuantumField[ccc___FCPartialD, A, ddd__]] = 0;
r2 = r1 // DotSimplify // FCCanonicalizeDummyIndices[#, SUNIndexNames -> {a1, b1, c1, d1}, LorentzIndexNames -> {mu, nu, rho}] & // ReplaceAll[#, {a1 -> a, b1 -> b, c1 -> c, d1 -> d, mu -> \[Mu], nu -> \[Nu], rho -> \[Rho]}] & // Collect2[#, SUNF] &
```

$$\frac{1}{2} f^{adg} f^{bcd} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+\frac{1}{2} f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+f^{abg} \left(2 A_{\mu }^a.\left(\left.(\partial _{\mu }A_{\sigma }^b\right)\right)-A_{\mu }^a.\left(\left.(\partial _{\sigma }A_{\mu }^b\right)\right)-A_{\sigma }^a.\left(\left.(\partial _{\mu }A_{\mu }^b\right)\right)\right)+\left(\partial _{\mu }\partial _{\mu }A_{\sigma }^g\right)-\left(\partial _{\mu }\partial _{\sigma }A_{\mu }^g\right)$$

Inspection reveals that still terms are the same. Gather the terms with two $f$'s:

```mathematica
twof = Select[r2 // Expand, Count[#, SUNF[__]] === 2 &] 
 
twofnew = ((twof[[1]] /. {a -> b, b -> a}) + twof[[2]]) // DotSimplify 
 
r3 = r2 - twof + twofnew
```

$$\frac{1}{2} f^{adg} f^{bcd} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+\frac{1}{2} f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c$$

$$f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c$$

$$f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+f^{abg} \left(2 A_{\mu }^a.\left(\left.(\partial _{\mu }A_{\sigma }^b\right)\right)-A_{\mu }^a.\left(\left.(\partial _{\sigma }A_{\mu }^b\right)\right)-A_{\sigma }^a.\left(\left.(\partial _{\mu }A_{\mu }^b\right)\right)\right)+\left(\partial _{\mu }\partial _{\mu }A_{\sigma }^g\right)-\left(\partial _{\mu }\partial _{\sigma }A_{\mu }^g\right)$$

Check that this is now indeed the same as the $text{t4}$ result from the first attempt.

```mathematica
t4 
 
w0 = RightPartialD[\[Mu]] . FieldStrength[\[Mu], \[Sigma], g, {A, a, b}, 1] + QuantumField[A, LorentzIndex[\[Mu]], SUNIndex[c]] . FieldStrength[\[Mu], \[Sigma], a, {A, b, d}, 1] SUNF[g, c, a] 
 
w1 = Explicit[w0] 
 
w2 = ExpandPartialD[w1] // DotSimplify 
 
dif1 = w2 - r3
```

$$\left.(\partial _{\mu }F_{\mu \sigma }^g\right)-A_{\mu }^a f^{gca} F_{\mu \sigma }^c$$

$$f^{gca} A_{\mu }^c.F_{\mu \sigma }^{a\{A,b,d\}1}+\vec{\partial }_{\mu }.F_{\mu \sigma }^{g\{A,a,b\}1}$$

$$f^{gca} A_{\mu }^c.\left(f^{abd} A_{\mu }^b.A_{\sigma }^d+\left.(\partial _{\mu }A_{\sigma }^a\right)-\left.(\partial _{\sigma }A_{\mu }^a\right)\right)+\vec{\partial }_{\mu }.\left(f^{gab} A_{\mu }^a.A_{\sigma }^b+\left.(\partial _{\mu }A_{\sigma }^g\right)-\left.(\partial _{\sigma }A_{\mu }^g\right)\right)$$

$$-f^{abd} f^{acg} A_{\mu }^b.A_{\mu }^c.A_{\sigma }^d+f^{abg} A_{\mu }^a.\left(\left.(\partial _{\mu }A_{\sigma }^b\right)\right)+f^{abg} A_{\sigma }^b.\left(\left.(\partial _{\mu }A_{\mu }^a\right)\right)-f^{acg} A_{\mu }^c.\left(\left.(\partial _{\mu }A_{\sigma }^a\right)\right)+f^{acg} A_{\mu }^c.\left(\left.(\partial _{\sigma }A_{\mu }^a\right)\right)+\left(\partial _{\mu }\partial _{\mu }A_{\sigma }^g\right)-\left(\partial _{\mu }\partial _{\sigma }A_{\mu }^g\right)$$

$$-f^{abd} f^{acg} A_{\mu }^b.A_{\mu }^c.A_{\sigma }^d-f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+f^{abg} A_{\mu }^a.\left(\left.(\partial _{\mu }A_{\sigma }^b\right)\right)-f^{abg} \left(2 A_{\mu }^a.\left(\left.(\partial _{\mu }A_{\sigma }^b\right)\right)-A_{\mu }^a.\left(\left.(\partial _{\sigma }A_{\mu }^b\right)\right)-A_{\sigma }^a.\left(\left.(\partial _{\mu }A_{\mu }^b\right)\right)\right)+f^{abg} A_{\sigma }^b.\left(\left.(\partial _{\mu }A_{\mu }^a\right)\right)-f^{acg} A_{\mu }^c.\left(\left.(\partial _{\mu }A_{\sigma }^a\right)\right)+f^{acg} A_{\mu }^c.\left(\left.(\partial _{\sigma }A_{\mu }^a\right)\right)$$

As expected:

```mathematica
dif1 // FCCanonicalizeDummyIndices
```

$$0$$

Finally, unset the commutator of the bosonic fields.

```mathematica
UnDeclareCommutator[QuantumField[aaa___FCPartialD, A, bbb__], QuantumField[ccc___FCPartialD, A, ddd__]] = 0;
```