## FunctionalD

FunctionalD[exp, {QuantumField[name, LorentzIndex[mu], ..., SUNIndex[a]][p], ...}] calculates the functional derivative of exp with respect to the QuantumField list (with incoming momenta $\text{p}$, etc.) and does the Fourier transform.   FunctionalD[expr, {QuantumField[name, LorentzIndex[mu], ... SUNIndex[a]], ...}] calculates the functional derivative and does partial integration but omits the $\text{x}$-space delta functions.FunctionalD is a low level function used in FeynRule.

### See also

[Overview](Extra/FeynCalc.md), [FeynRule](FeynRule.md), [QuantumField](QuantumField.md).

### Examples

Instead of the usual $\delta \phi (x)/ \delta \phi (y)= \delta ^{(D)}(x-y)$ the arguments and the $\delta$ function are omitted, i.e. for the program for simplicity: $\delta \phi / \delta \phi =1$.

```mathematica
FunctionalD[QuantumField[\[Phi]], QuantumField[\[Phi]]]
```

$$1$$

```mathematica
FunctionalD[QuantumField[\[Phi]]^2, QuantumField[\[Phi]]]
```

$$2 \phi$$

Instead of the usual  $(\delta  \partial _{\mu} \phi (x) )/ \delta \phi (y)= \partial _{\mu} \delta^{(D)}(x-y)$ the arguments are omitted, and the $\partial_\mu$ operator is specified by default to be an integration by parts operator, i.e. the right hand side will be just `Null` or, more precisely, (by default) $-\partial _{mu }$.

```mathematica
FunctionalD[QuantumField[FCPartialD[\[Mu]], \[Phi]], QuantumField[\[Phi]]]
```

$$-\vec{\partial }_{\mu }$$

```mathematica
(QuantumField[FCPartialD[\[Mu]], \[Phi]] . QuantumField[FCPartialD[\[Mu]], \[Phi]] - m^2 QuantumField[\[Phi]] . QuantumField[\[Phi]])/2
FunctionalD[%, QuantumField[\[Phi]]]
```

$$\frac{1}{2} \left(\left(\left.(\partial _{\mu }\phi \right)\right).\left(\left.(\partial _{\mu }\phi \right)\right)-m^2 \phi .\phi \right)$$

$$m^2 (-\phi )-\left(\partial _{\mu }\partial _{\mu }\phi \right)$$

$S[A] = -\int  d^Dx \frac{1}{4} F_a^{\mu \n u }(x) F_{\text{$\mu \nu $a}}(x)$

First approach:

```mathematica
F1 = FieldStrength[\[Mu], \[Nu], a, {A, b, c}, 1, Explicit -> True]
F2 = FieldStrength[\[Mu], \[Nu], a, {A, d, e}, 1, Explicit -> True]
S[A] = -1/4 F1 . F2
```

$$f^{abc} A_{\mu }^b.A_{\nu }^c+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)$$

$$f^{ade} A_{\mu }^d.A_{\nu }^e+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)$$

$$-\frac{1}{4} \left(f^{abc} A_{\mu }^b.A_{\nu }^c+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)\right).\left(f^{ade} A_{\mu }^d.A_{\nu }^e+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)\right)$$

In order to derive the equation of motion, the functional derivative of $S$ with respect to $A_{\sigma }^g$ has to be set to zero. Bearing in mind that for FeynCalc we have to be precise as to where which operators (coming from the substitution of the derivative of the delta function) act.

Act with the functional derivative operator on the first field strength:

$0 = (\delta S)  / ( \delta A_ {\sigma }^g(y) ) =-2/4 \int d^D x (\delta / (\delta A_ {\sigma }^g (y) ) F_{\mu \nu}(x)) F_a^{\mu \nu}(x)$

See what happens with just $(\delta S[A]) / (\delta A_{\sigma }^g)$

```mathematica
Ag = QuantumField[A, {\[Sigma]}, {g}]
```

$$A_{\sigma }^g$$

```mathematica
FunctionalD[F1, Ag]
```

$$A_{\mu }^b \bar{g}^{\nu \sigma } f^{abg}-A_{\nu }^c \bar{g}^{\mu \sigma } f^{acg}+\delta ^{ag} \vec{\partial }_{\mu } \left(-\bar{g}^{\nu \sigma }\right)+\delta ^{ag} \vec{\partial }_{\nu } \bar{g}^{\mu \sigma }$$

Use `FCCanonicalizeDummyIndices` to minimize the number of dummy indices.

```mathematica
t1 = FCCanonicalizeDummyIndices[%, SUNIndexNames -> {c1}] /. c1 -> c
```

$$A_{\mu }^c \bar{g}^{\nu \sigma } f^{acg}-A_{\nu }^c \bar{g}^{\mu \sigma } f^{acg}+\delta ^{ag} \vec{\partial }_{\mu } \left(-\bar{g}^{\nu \sigma }\right)+\delta ^{ag} \vec{\partial }_{\nu } \bar{g}^{\mu \sigma }$$

Instead of inserting the definition for the second $F_a^{\mu \nu}$, introduce a `QuantumField` object with antisymmetry built into the Lorentz indices:

```mathematica
F /: QuantumField[pard___, F, \[Beta]_, \[Alpha]_, s_] := -QuantumField[pard, F, \[Alpha], \[Beta], s] /; ! OrderedQ[{\[Beta], \[Alpha]}]
```

```mathematica
QuantumField[F, {\[Mu], \[Nu]}, {a}]
% /. {\[Mu] :> \[Nu], \[Nu] :> \[Mu]}
```

$$F_{\mu \nu }^a$$

$$-F_{\mu \nu }^a$$

```mathematica
t2 = Contract[ExpandPartialD[-1/2 t1 . QuantumField[F, LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], SUNIndex[a]]]] /. Dot -> Times
```

$$-\frac{1}{2} A_{\mu }^c f^{acg} F_{\mu \sigma }^a-\frac{1}{2} A_{\nu }^c f^{acg} F_{\nu \sigma }^a+\frac{1}{2} \left(\left.(\partial _{\mu }F_{\mu \sigma }^g\right)\right)+\frac{1}{2} \left(\left.(\partial _{\nu }F_{\nu \sigma }^g\right)\right)$$

```mathematica
t3 = FCCanonicalizeDummyIndices[t2, LorentzIndexNames -> {mu}, SUNIndexNames -> {aa, cc}] /. {mu -> \[Mu], aa -> a, cc -> c}
```

$$A_{\mu }^a f^{acg} F_{\mu \sigma }^c+\left.(\partial _{\mu }F_{\mu \sigma }^g\right)$$

```mathematica
t4 = FCE[t3] /. SUNF[a, c, g] -> -SUNF[g, c, a]
```

$$\left.(\partial _{\mu }F_{\mu \sigma }^g\right)-A_{\mu }^a f^{gca} F_{\mu \sigma }^c$$

Since the variational derivative vanishes `t4` implies that $0= D_{\mu} F_g^{\mu \sigma }$

Second approach:

It is of course also possible to do the functional derivative on the $S[A]$ with both field strength tensors inserted.

```mathematica
S[A]
```

$$-\frac{1}{4} \left(f^{abc} A_{\mu }^b.A_{\nu }^c+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)\right).\left(f^{ade} A_{\mu }^d.A_{\nu }^e+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)\right)$$

```mathematica
r1 = FunctionalD[S[A], Ag]
```

$$-\frac{1}{4} f^{abc} f^{adg} A_{\mu }^b.A_{\sigma }^c.A_{\mu }^d+\frac{1}{4} f^{abc} f^{aeg} A_{\sigma }^b.A_{\nu }^c.A_{\nu }^e-\frac{1}{4} f^{abg} f^{ade} A_{\mu }^b.A_{\mu }^d.A_{\sigma }^e-\frac{1}{4} f^{abg} A_{\mu }^b.\left(\left.(\partial _{\mu }A_{\sigma }^a\right)\right)+\frac{1}{4} f^{abg} A_{\mu }^b.\left(\left.(\partial _{\sigma }A_{\mu }^a\right)\right)+\frac{1}{4} f^{acg} f^{ade} A_{\nu }^c.A_{\sigma }^d.A_{\nu }^e-\frac{1}{4} f^{acg} A_{\nu }^c.\left(\left.(\partial _{\nu }A_{\sigma }^a\right)\right)+\frac{1}{4} f^{acg} A_{\nu }^c.\left(\left.(\partial _{\sigma }A_{\nu }^a\right)\right)-\frac{1}{4} f^{adg} \left(\left.(\partial _{\mu }A_{\sigma }^a\right)\right).A_{\mu }^d+\frac{1}{4} f^{adg} \left(\left.(\partial _{\sigma }A_{\mu }^a\right)\right).A_{\mu }^d-\frac{1}{4} f^{aeg} \left(\left.(\partial _{\nu }A_{\sigma }^a\right)\right).A_{\nu }^e+\frac{1}{4} f^{aeg} \left(\left.(\partial _{\sigma }A_{\nu }^a\right)\right).A_{\nu }^e+\frac{1}{4} f^{bcg} A_{\mu }^b.\left(\left.(\partial _{\mu }A_{\sigma }^c\right)\right)+\frac{1}{4} f^{bcg} \left(\left.(\partial _{\mu }A_{\mu }^b\right)\right).A_{\sigma }^c-\frac{1}{4} f^{bcg} A_{\sigma }^b.\left(\left.(\partial _{\nu }A_{\nu }^c\right)\right)-\frac{1}{4} f^{bcg} \left(\left.(\partial _{\nu }A_{\sigma }^b\right)\right).A_{\nu }^c+\frac{1}{4} f^{deg} A_{\mu }^d.\left(\left.(\partial _{\mu }A_{\sigma }^e\right)\right)+\frac{1}{4} f^{deg} \left(\left.(\partial _{\mu }A_{\mu }^d\right)\right).A_{\sigma }^e-\frac{1}{4} f^{deg} A_{\sigma }^d.\left(\left.(\partial _{\nu }A_{\nu }^e\right)\right)-\frac{1}{4} f^{deg} \left(\left.(\partial _{\nu }A_{\sigma }^d\right)\right).A_{\nu }^e+\frac{1}{2} \left(\partial _{\mu }\partial _{\mu }A_{\sigma }^g\right)-\frac{1}{2} \left(\partial _{\mu }\partial _{\sigma }A_{\mu }^g\right)+\frac{1}{2} \left(\partial _{\nu }\partial _{\nu }A_{\sigma }^g\right)-\frac{1}{2} \left(\partial _{\nu }\partial _{\sigma }A_{\nu }^g\right)$$

This is just functional derivation and partial integration and simple contraction of indices. At first no attempt is made to rename dummy indices (since this is difficult in general).

With a general replacement rule only valid for commuting fields the color indices can be canonicalized a bit more. The idea is to use the commutative properties of the vector fields, and canonicalize the color indices by a trick.

```mathematica
Commutator[QuantumField[aaa___FCPartialD, A, bbb__], QuantumField[ccc___FCPartialD, A, ddd__]] = 0;
r2 = r1 // DotSimplify // FCCanonicalizeDummyIndices[#, SUNIndexNames -> {a1, b1, c1, d1}, LorentzIndexNames -> {mu, nu, rho}] & // ReplaceAll[#, {a1 -> a, b1 -> b, c1 -> c, d1 -> d, mu -> \[Mu], nu -> \[Nu], rho -> \[Rho]}] & // Collect2[#, SUNF] &
```

$$\frac{1}{2} f^{adg} f^{bcd} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+\frac{1}{2} f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+f^{abg} \left(2 A_{\mu }^a.\left(\left.(\partial _{\mu }A_{\sigma }^b\right)\right)-A_{\mu }^a.\left(\left.(\partial _{\sigma }A_{\mu }^b\right)\right)-A_{\sigma }^a.\left(\left.(\partial _{\mu }A_{\mu }^b\right)\right)\right)+\left(\partial _{\mu }\partial _{\mu }A_{\sigma }^g\right)-\left(\partial _{\mu }\partial _{\sigma }A_{\mu }^g\right)$$

Inspection reveals that still terms are the same. Gather the terms with two `f`'s:

```mathematica
twof = Select[r2 // Expand, Count[#, SUNF[__]] === 2 &]
```

$$\frac{1}{2} f^{adg} f^{bcd} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+\frac{1}{2} f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c$$

```mathematica
twofnew = ((twof[[1]] /. {a -> b, b -> a}) + twof[[2]]) // DotSimplify
```

$$f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c$$

```mathematica
r3 = r2 - twof + twofnew
```

$$f^{acd} f^{bdg} A_{\mu }^a.A_{\mu }^b.A_{\sigma }^c+f^{abg} \left(2 A_{\mu }^a.\left(\left.(\partial _{\mu }A_{\sigma }^b\right)\right)-A_{\mu }^a.\left(\left.(\partial _{\sigma }A_{\mu }^b\right)\right)-A_{\sigma }^a.\left(\left.(\partial _{\mu }A_{\mu }^b\right)\right)\right)+\left(\partial _{\mu }\partial _{\mu }A_{\sigma }^g\right)-\left(\partial _{\mu }\partial _{\sigma }A_{\mu }^g\right)$$

Check that this is now indeed the same as the `t4` result from the first attempt.

```mathematica
t4
```

$$\left.(\partial _{\mu }F_{\mu \sigma }^g\right)-A_{\mu }^a f^{gca} F_{\mu \sigma }^c$$

```mathematica
w0 = RightPartialD[\[Mu]] . FieldStrength[\[Mu], \[Sigma], g, {A, a, b}, 1] + QuantumField[A, LorentzIndex[\[Mu]], SUNIndex[c]] . FieldStrength[\[Mu], \[Sigma], a, {A, b, d}, 1] SUNF[g, c, a]
```

$$f^{gca} A_{\mu }^c.F_{\mu \sigma }^{a\{A,b,d\}1}+\vec{\partial }_{\mu }.F_{\mu \sigma }^{g\{A,a,b\}1}$$

```mathematica
w1 = Explicit[w0]
```

$$f^{gca} A_{\mu }^c.\left(f^{abd} A_{\mu }^b.A_{\sigma }^d+\left.(\partial _{\mu }A_{\sigma }^a\right)-\left.(\partial _{\sigma }A_{\mu }^a\right)\right)+\vec{\partial }_{\mu }.\left(f^{gab} A_{\mu }^a.A_{\sigma }^b+\left.(\partial _{\mu }A_{\sigma }^g\right)-\left.(\partial _{\sigma }A_{\mu }^g\right)\right)$$

```mathematica
w2 = ExpandPartialD[w1] // DotSimplify
```

$$-f^{abd} f^{acg} A_{\mu }^b.A_{\mu }^c.A_{\sigma }^d+f^{abg} A_{\mu }^a.\left(\left.(\partial _{\mu }A_{\sigma }^b\right)\right)+f^{abg} A_{\sigma }^b.\left(\left.(\partial _{\mu }A_{\mu }^a\right)\right)-f^{acg} A_{\mu }^c.\left(\left.(\partial _{\mu }A_{\sigma }^a\right)\right)+f^{acg} A_{\mu }^c.\left(\left.(\partial _{\sigma }A_{\mu }^a\right)\right)+\left(\partial _{\mu }\partial _{\mu }A_{\sigma }^g\right)-\left(\partial _{\mu }\partial _{\sigma }A_{\mu }^g\right)$$

```mathematica
dif1 = w2 - r3
```

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
