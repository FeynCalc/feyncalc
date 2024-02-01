## Eps

`Eps[a, b, c, d]` is the head of the totally antisymmetric $\epsilon$ (Levi-Civita) tensor. The `a,b, ...` may have head `LorentzIndex` or `Momentum`.

When some indices of a Levi-Civita-tensor are contracted with 4-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3 p_4}$ (accessible via `LC[][p1,p2,p3,p4]`) correspond to $\varepsilon_{\mu \nu \rho \sigma} p_1^\mu p_2^\nu p_3^\rho p_4^\sigma$.

### See also

[Overview](Extra/FeynCalc.md), [EpsContract](EpsContract.md), [EpsEvaluate](EpsEvaluate.md), [LC](LC.md), [LCD](LCD.md).

### Examples

```mathematica
Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]
```

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

```mathematica
Eps[Momentum[p], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]
```

$$\bar{\epsilon }^{\overline{p}\nu \rho \sigma }$$

```mathematica
Eps[b, a, c, d] // StandardForm

(*Eps[b, a, c, d]*)
```

```mathematica
Eps[ExplicitLorentzIndex[0], ExplicitLorentzIndex[1], ExplicitLorentzIndex[2], 
  ExplicitLorentzIndex[3]]
```

$$\bar{\epsilon }^{0123}$$

```mathematica
Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]] *
   Pair[LorentzIndex[\[Mu]], Momentum[Subscript[p, 1]]] Pair[LorentzIndex[\[Nu]], Momentum[Subscript[p, 2]]]*
   Pair[LorentzIndex[\[Rho]], Momentum[Subscript[p, 3]]] Pair[LorentzIndex[\[Sigma]], Momentum[Subscript[p, 4]]] 
 
Contract[%] 
```

$$\overline{p}_1{}^{\mu } \overline{p}_2{}^{\nu } \overline{p}_3{}^{\rho } \overline{p}_4{}^{\sigma } \bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$\bar{\epsilon }^{\overline{p}_1\overline{p}_2\overline{p}_3\overline{p}_4}$$

```mathematica
Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]] 
 
Contract[% %]
```

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$-24$$

```mathematica
Eps[LorentzIndex[\[Mu], D], LorentzIndex[\[Nu], D], LorentzIndex[\[Rho], D], LorentzIndex[\[Sigma], D]] 
 
Contract[% %] // Factor2
```

$$\overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }$$

$$(1-D) (2-D) (3-D) D$$

```mathematica
ex1 = -(I/24) LCD[\[Mu], \[Nu], \[Rho], \[Alpha]] . GAD[\[Mu], \[Nu], \[Rho], \[Alpha]] // FCI
```

$$-\frac{1}{24} i \overset{\text{}}{\epsilon }^{\mu \nu \rho \alpha }.\gamma ^{\mu }.\gamma ^{\nu }.\gamma ^{\rho }.\gamma ^{\alpha }$$

```mathematica
ex2 = -(I/24) LCD[\[Mu]', \[Nu]', \[Rho]', \[Alpha]'] . GAD[\[Mu]', \[Nu]', \[Rho]', \[Alpha]'] // FCI
```

$$-\frac{1}{24} i \overset{\text{}}{\epsilon }^{\mu '\nu '\rho '\alpha '}.\gamma ^{\mu '}.\gamma ^{\nu '}.\gamma ^{\rho '}.\gamma ^{\alpha '}$$

```mathematica
DiracSimplify[ex1 . ex2] // Factor2 
 
% /. D -> 4
```

$$-\frac{1}{24} (1-D) (2-D) (3-D) D$$

$$1$$