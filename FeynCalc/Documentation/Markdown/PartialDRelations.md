## PartialDRelations

`PartialDRelations` is an option for `ExpandPartialD`. It is a list of rules applied by `ExpandPartialD` at the end.

### See also

[Overview](Extra/FeynCalc.md), [FCPartialD](FCPartialD.md), [ExpandPartialD](ExpandPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [RightPartialD](RightPartialD.md).

### Examples

```mathematica
QuantumField[A, {\[Mu]}] . QuantumField[B, {\[Mu]}] . LeftPartialD[\[Nu]]
ExpandPartialD[%, PartialDRelations -> {A -> C}]
```

$$A_{\mu }.B_{\mu }.\overleftarrow{\partial }_{\nu }$$

$$C_{\mu }.\left(\left.(\partial _{\nu }B_{\mu }\right)\right)+\left(\left.(\partial _{\nu }C_{\mu }\right)\right).B_{\mu }$$
