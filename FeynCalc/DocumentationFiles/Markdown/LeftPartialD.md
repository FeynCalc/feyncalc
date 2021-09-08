## LeftPartialD

`LeftPartialD[μ]` denotes $\overleftarrow{\partial }_{\mu }$ acting to the left.

### See also

[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [RightPartialD](RightPartialD.md).

### Examples

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . LeftPartialD[\[Nu]]
ExpandPartialD[%]
StandardForm[%]
```

$$A_{\mu }.\overleftarrow{\partial }_{\nu }$$

$$\left.(\partial _{\nu }A_{\mu }\right)$$

```
(*QuantumField[FCPartialD[LorentzIndex[\[Nu]]], A, LorentzIndex[\[Mu]]]*)
```

```mathematica
StandardForm[LeftPartialD[\[Mu]]]

(*LeftPartialD[LorentzIndex[\[Mu]]]*)
```

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]] . LeftPartialD[\[Rho]]
ExpandPartialD[%]
StandardForm[%]
```

$$A_{\mu }.A_{\nu }.\overleftarrow{\partial }_{\rho }$$

$$A_{\mu }.\left(\left.(\partial _{\rho }A_{\nu }\right)\right)+\left(\left.(\partial _{\rho }A_{\mu }\right)\right).A_{\nu }$$

```
(*QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[FCPartialD[LorentzIndex[\[Rho]]], A, LorentzIndex[\[Nu]]] + QuantumField[FCPartialD[LorentzIndex[\[Rho]]], A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]]*)
```
