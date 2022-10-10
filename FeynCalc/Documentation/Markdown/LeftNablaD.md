## LeftNablaD

`LeftNablaD[i]` denotes $\overleftarrow{\nabla}_{i}$ acting to the left.

### See also

[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftRightNablaD](LeftRightNablaD.md), [RightNablaD](RightNablaD.md).

### Examples

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . LeftNablaD[i] 
 
ex = ExpandPartialD[%]
```

$$A_{\mu }.\overleftarrow{\nabla }^i$$

$$-\left(\partial _iA_{\mu }\right)$$

```mathematica
ex // StandardForm

(*-QuantumField[FCPartialD[CartesianIndex[i]], A, LorentzIndex[\[Mu]]]*)
```

```mathematica
StandardForm[LeftNablaD[i]]

(*LeftNablaD[CartesianIndex[i]]*)
```

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]] . LeftNablaD[i] 
 
ex = ExpandPartialD[%]
```

$$A_{\mu }.A_{\nu }.\overleftarrow{\nabla }^i$$

$$-A_{\mu }.\left(\partial _iA_{\nu }\right)-\left(\partial _iA_{\mu }\right).A_{\nu }$$

```mathematica
ex // StandardForm

(*-QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[FCPartialD[CartesianIndex[i]], A, LorentzIndex[\[Nu]]] - QuantumField[FCPartialD[CartesianIndex[i]], A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]]*)
```