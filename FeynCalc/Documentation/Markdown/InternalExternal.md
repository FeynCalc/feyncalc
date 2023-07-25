```mathematica
 
```

## Internal vs. External Representations

### See also

[Overview](Extra/FeynCalc.md).

### FeynCalcInternal

The internal representation (`FeynCalcIntenral` or `FCI`) is how FeynCalc internally "sees" the objects.  For example, a $4$-dimensional $4$-vector is represented by

```mathematica
Pair[LorentzIndex[\[Mu]], Momentum[p]]
```

$$\overline{p}^{\mu }$$

Pair is one of the most basic FeynCalc objects. Depending on its arguments, it can represent a $4$-vector, a metric tensor

```mathematica
Pair[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]]]
```

$$\bar{g}^{\mu \nu }$$

or a scalar product of two 4-vectors

```mathematica
Pair[Momentum[p], Momentum[q]]
```

$$\overline{p}\cdot \overline{q}$$

Another essential object is `DiracGamma` that is used to represent Dirac matrices. An uncontracted Dirac matrix is

```mathematica
DiracGamma[LorentzIndex[\[Mu]]]
```

$$\bar{\gamma }^{\mu }$$

and for a Feynman slash we use

```mathematica
DiracGamma[Momentum[p]]
```

$$\bar{\gamma }\cdot \overline{p}$$

The Levi-Civita-Tensor is

```mathematica
Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]
```

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

or, when contracted with $4$-momenta

```mathematica
Eps[Momentum[p1], Momentum[p2], Momentum[q1], Momentum[q2]]
```

$$\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{q1}}\;\overline{\text{q2}}}$$

This notation (momenta in the index slots) is also used in many other tools (e.g. FORM). The advantage is, that we do not need to canonicalize the indices of the Levi-Civita-Tensor, e.g. to ensure that

```mathematica
diff = Eps[LorentzIndex[\[Mu]], Momentum[p2], Momentum[q1], Momentum[q2]] Pair[LorentzIndex[\[Mu]], Momentum[p1]] - 
   Eps[LorentzIndex[\[Nu]], Momentum[p2], Momentum[q1], Momentum[q2]] Pair[LorentzIndex[\[Nu]], Momentum[p1]]
```

$$\overline{\text{p1}}^{\mu } \bar{\epsilon }^{\mu \overline{\text{p2}}\;\overline{\text{q1}}\;\overline{\text{q2}}}-\overline{\text{p1}}^{\nu } \bar{\epsilon }^{\nu \overline{\text{p2}}\;\overline{\text{q1}}\;\overline{\text{q2}}}$$

```mathematica
diff // Contract
```

$$0$$

is zero.

### FeynCalcExternal

The internal representation is useful for the internal programming FeynCalc, but obviously too cumbersome for the user input. This is why FeynCalc also has an external representation (`FeynCalcExternal` or `FCE`), that is concise and convenient.

Let us start with the $4$-vector. In the FCE-notation it is just `FV` ("FourVector")

```mathematica
FV[p, \[Mu]]
```

$$\overline{p}^{\mu }$$

It is not hard to guess that the scalar product is `SP`

```mathematica
SP[p, q]
```

$$\overline{p}\cdot \overline{q}$$

while for the metric tensor we write `MT`

```mathematica
MT[\[Mu], \[Nu]]
```

$$\bar{g}^{\mu \nu }$$

To input a Dirac matrix or a Feynman slash, use `GA` or `GS` respectively

```mathematica
GA[\[Mu]]
```

$$\bar{\gamma }^{\mu }$$

```mathematica
GS[p]
```

$$\bar{\gamma }\cdot \overline{p}$$

The Levi-Civita tensor is `LC`

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]]
```

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

The fully contracted form is entered via

```mathematica
LC[][p1, p2, q1, q2]
```

$$\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{q1}}\;\overline{\text{q2}}}$$

It is also possible to enter a mixed form

```mathematica
LC[\[Mu]][p1, p2, q]
```

$$\bar{\epsilon }^{\mu \overline{\text{p1}}\;\overline{\text{p2}}\overline{q}}$$

```mathematica
LC[\[Mu], \[Nu]][p1, p2]
```

$$\bar{\epsilon }^{\mu \nu \overline{\text{p1}}\;\overline{\text{p2}}}$$

### Switching between the representations

To convert between the two representations we use the functions `FCI` and `FCE`, which are shortcuts for `FeynCalcInternal` and `FeynCalcExternal`. One cannot distinguish between the notations using the typesetting, i.e. when we see a typeset object in the `TraditionalForm`, we cannot really tell if it is in the `FCI` or `FCE` notation.

```mathematica
ex1 = FV[p, \[Mu]]
ex2 = Pair[Momentum[p], LorentzIndex[\[Mu]]]
```

$$\overline{p}^{\mu }$$

$$\overline{p}^{\mu }$$

However, we can always use StandardForm to see the difference

```mathematica
ex1 // StandardForm
ex2 // StandardForm

(*FV[p, \[Mu]]*)

(*Pair[LorentzIndex[\[Mu]], Momentum[p]]*)
```

### Why it matters

All FeynCalc functions that are meant for users will automatically convert the user input in the `FCE` notation into the `FCI` notation. You do not have to do it by yourself.

On the other hand, virtually all FeynCalc functions produce their output in the `FCI` form. So when you have an expression that was obtained from FeynCalc and want to apply some replacement rules to it, we have to use the `FCI` form in the rule

```mathematica
ex = Pair[Momentum[p], Momentum[q]]
```

$$\overline{p}\cdot \overline{q}$$

No surprise that following does not work

```mathematica
ex /. SP[p, q] -> 1
```

$$\overline{p}\cdot \overline{q}$$

But if we wrap the r.h.s of the rule with `FCI`, then everything is fine

```mathematica
ex /. FCI[SP[p, q]] -> 1
```

$$1$$