## LeftRightPartialD2 

`LeftRightPartialD2[μ]` denotes $\overleftrightarrow{\partial }_{\mu }$, acting to the left and right.

`ExplicitPartialD[LeftRightPartialD2[μ]] gives `(RightPartialD[μ] + LeftPartialD[μ])`.

### See also

[ExplicitPartialD](ExplicitPartialD), [ExpandPartialD](ExpandPartialD), [FCPartialD](FCPartialD), [LeftPartialD](LeftPartialD), [RightPartialD](RightPartialD).

### Examples

```mathematica
LeftRightPartialD2[\[Mu]]
ExplicitPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }$$

$$\overleftarrow{\partial }_{\mu }+\vec{\partial }_{\mu }$$

```mathematica
LeftRightPartialD2[\[Mu]] . QuantumField[A, LorentzIndex[\[Nu]]]
ExpandPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }.A_{\nu }$$

$$\left.(\partial _{\mu }A_{\nu }\right)+\overleftarrow{\partial }_{\mu }.A_{\nu }$$

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . LeftRightPartialD2[\[Nu]] . QuantumField[A, LorentzIndex[\[Rho]]]
ExpandPartialD[%]
```

$$A_{\mu }.\overleftrightarrow{\partial }_{\nu }.A_{\rho }$$

$$A_{\mu }.\left(\left.(\partial _{\nu }A_{\rho }\right)\right)+\left(\left.(\partial _{\nu }A_{\mu }\right)\right).A_{\rho }$$