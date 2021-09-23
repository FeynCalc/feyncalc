## LeftRightPartialD

`LeftRightPartialD[mu]` denotes $\overleftrightarrow {\partial }_{\mu }$, acting to the left and right.

`ExplicitPartialD[LeftRightPartialD[μ]]` gives `1/2 (RightPartialD[μ] - LeftPartialD[μ])`.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD2](LeftRightPartialD2.md), [RightPartialD](RightPartialD.md).

### Examples

```mathematica
LeftRightPartialD[\[Mu]]
ExplicitPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }$$

$$\frac{1}{2} \left(\vec{\partial }_{\mu }-\overleftarrow{\partial }_{\mu }\right)$$

```mathematica
LeftRightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Nu]]]
ExpandPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }.A_{\nu }$$

$$\frac{1}{2} \left(\left.(\partial _{\mu }A_{\nu }\right)-\overleftarrow{\partial }_{\mu }.A_{\nu }\right)$$

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . LeftRightPartialD[\[Nu]] . QuantumField[A, LorentzIndex[\[Rho]]]
ExpandPartialD[%]
```

$$A_{\mu }.\overleftrightarrow{\partial }_{\nu }.A_{\rho }$$

$$\frac{1}{2} \left(A_{\mu }.\left(\left.(\partial _{\nu }A_{\rho }\right)\right)-\left(\left.(\partial _{\nu }A_{\mu }\right)\right).A_{\rho }\right)$$
