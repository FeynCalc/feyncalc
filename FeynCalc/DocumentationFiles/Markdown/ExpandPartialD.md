## ExpandPartialD

`ExpandPartialD[exp]` expands noncommutative products of `QuantumField}`'s and partial differentiation operators in `exp` and applies the Leibniz rule.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [PartialDRelations](PartialDRelations.md), [RightPartialD](RightPartialD.md).

### Examples

```mathematica
RightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]]
ExpandPartialD[%]
```

$$\vec{\partial }_{\mu }.A_{\mu }.A_{\nu }$$

$$A_{\mu }.\left(\left.(\partial _{\mu }A_{\nu }\right)\right)+\left(\left.(\partial _{\mu }A_{\mu }\right)\right).A_{\nu }$$

```mathematica
LeftRightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Nu]]]
ExpandPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }.A_{\nu }$$

$$\frac{1}{2} \left(\left.(\partial _{\mu }A_{\nu }\right)-\overleftarrow{\partial }_{\mu }.A_{\nu }\right)$$

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . (LeftRightPartialD[OPEDelta]^2) . QuantumField[A, LorentzIndex[\[Rho]]]
ExpandPartialD[%]
```

$$A_{\mu }.\overleftrightarrow{\partial }_{\Delta }^2.A_{\rho }$$

$$\frac{1}{4} \left(-2 \left(\left.(\partial _{\Delta }A_{\mu }\right)\right).\left(\left.(\partial _{\Delta }A_{\rho }\right)\right)+A_{\mu }.\left(\partial _{\Delta }\partial _{\Delta }A_{\rho }\right)+\left(\partial _{\Delta }\partial _{\Delta }A_{\mu }\right).A_{\rho }\right)$$

```mathematica
8 LeftRightPartialD[OPEDelta]^3
```

$$8 \overleftrightarrow{\partial }_{\Delta }^3$$

```mathematica
ExplicitPartialD[%]
```

$$\left(\vec{\partial }_{\Delta }-\overleftarrow{\partial }_{\Delta }\right){}^3$$

```mathematica
ExpandPartialD[%]
```

$$-\overleftarrow{\partial }_{\Delta }.\overleftarrow{\partial }_{\Delta }.\overleftarrow{\partial }_{\Delta }+3 \overleftarrow{\partial }_{\Delta }.\overleftarrow{\partial }_{\Delta }.\vec{\partial }_{\Delta }-3 \overleftarrow{\partial }_{\Delta }.\vec{\partial }_{\Delta }.\vec{\partial }_{\Delta }+\vec{\partial }_{\Delta }.\vec{\partial }_{\Delta }.\vec{\partial }_{\Delta }$$

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Tau]] RightPartialD[\[Alpha], \[Mu], \[Beta], \[Nu]]
```

$$\bar{\epsilon }^{\mu \nu \rho \tau } \vec{\partial }_{\alpha }.\vec{\partial }_{\mu }.\vec{\partial }_{\beta }.\vec{\partial }_{\nu }$$

```mathematica
ExpandPartialD[%]
```

$$0$$
