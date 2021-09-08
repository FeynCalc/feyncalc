## Eps

`Eps[a, b, c, d]` is the head of the totally antisymmetric $\epsilon$ (Levi-Civita) tensor. The `a,b, ...` may have head `LorentzIndex` or `Momentum`.

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
Eps[ExplicitLorentzIndex[0], ExplicitLorentzIndex[1], ExplicitLorentzIndex[2], ExplicitLorentzIndex[3]]
```

$$\bar{\epsilon }^{0123}$$

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
ex2 = -(I/24) LCD[\[Mu]^\[Prime], \[Nu]^\[Prime], \[Rho]^\[Prime], \[Alpha]^\[Prime]] . GAD[\[Mu]^\[Prime], \[Nu]^\[Prime], \[Rho]^\[Prime], \[Alpha]^\[Prime]] // FCI
```

$$-\frac{1}{24} i \overset{\text{}}{\epsilon }^{\mu '\nu '\rho '\alpha '}.\gamma ^{\mu '}.\gamma ^{\nu '}.\gamma ^{\rho '}.\gamma ^{\alpha '}$$

```mathematica
DiracSimplify[ex1 . ex2] // Factor2
% /. D -> 4 
  
 

```

$$-\frac{1}{24} (1-D) (2-D) (3-D) D$$

$$1$$
