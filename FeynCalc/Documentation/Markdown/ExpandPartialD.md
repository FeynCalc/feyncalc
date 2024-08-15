## ExpandPartialD

`ExpandPartialD[exp]` expands noncommutative products of `QuantumField`s and partial differentiation operators in `exp` and applies the Leibniz rule.

By default the function assumes that there are no expressions outside of `exp` on which the derivatives inside `exp` could act. If this is not the case, please set the options `LeftPartialD` or `RIghtPartialD` to `True`.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [PartialDRelations](PartialDRelations.md), [RightPartialD](RightPartialD.md), [LeftRightNablaD](LeftRightNablaD.md), [LeftRightNablaD2](LeftRightNablaD2.md), [LeftNablaD](LeftNablaD.md), [RightNablaD](RightNablaD.md).

### Examples

```mathematica
RightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%]
```

$$\vec{\partial }_{\mu }.A_{\mu }.A_{\nu }$$

$$A_{\mu }.\left(\partial _{\mu }A_{\nu }\right)+\left(\partial _{\mu }A_{\mu }\right).A_{\nu }$$

```mathematica
RightNablaD[i] . QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%]
```

$$\vec{\nabla }^i.A_{\mu }.A_{\nu }$$

$$-A_{\mu }.\left(\partial _iA_{\nu }\right)-\left(\partial _iA_{\mu }\right).A_{\nu }$$

```mathematica
LeftRightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }.A_{\nu }$$

$$\frac{1}{2} \left(\left(\partial _{\mu }A_{\nu }\right)-\overleftarrow{\partial }_{\mu }.A_{\nu }\right)$$

```mathematica
LeftRightNablaD[i] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%]
```

$$\overleftrightarrow{\nabla }_i.A_{\nu }$$

$$\frac{1}{2} \left(\overleftarrow{\partial }_i.A_{\nu }-\left(\partial _iA_{\nu }\right)\right)$$

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . (LeftRightPartialD[OPEDelta]^2) . QuantumField[A, 
    LorentzIndex[\[Rho]]] 
 
ExpandPartialD[%]
```

$$A_{\mu }.\overleftrightarrow{\partial }_{\Delta }^2.A_{\rho }$$

$$\frac{1}{4} \left(A_{\mu }.\left(\partial _{\Delta }\partial _{\Delta }A_{\rho }\right)-2 \left(\partial _{\Delta }A_{\mu }\right).\left(\partial _{\Delta }A_{\rho }\right)+\left(\partial _{\Delta }\partial _{\Delta }A_{\mu }\right).A_{\rho }\right)$$

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
 
ExpandPartialD[%]
```

$$\bar{\epsilon }^{\mu \nu \rho \tau } \vec{\partial }_{\alpha }.\vec{\partial }_{\mu }.\vec{\partial }_{\beta }.\vec{\partial }_{\nu }$$

$$0$$

```mathematica
CLC[i, j, k] RightNablaD[i, j, k] 
 
ExpandPartialD[%]
```

$$\bar{\epsilon }^{ijk} \vec{\nabla }^i.\vec{\nabla }^j.\vec{\nabla }^k$$

$$0$$

```mathematica
RightPartialD[CartesianIndex[i]] . QuantumField[S, x] 
 
% // ExpandPartialD
```

$$\vec{\partial }_i.S^x$$

$$\left(\partial _iS^x\right)$$

```mathematica
RightPartialD[{CartesianIndex[i], x}] . QuantumField[S, x] 
 
% // ExpandPartialD
```

$$\vec{\partial }_{\{i,x\}}.S^x$$

$$\left(\partial _{\{i,x\}}S^x\right)$$

By default the derivative won't act on anything outside of the input expression. But it can be made to by setting the option `RightPartialD` to `True`

```mathematica
ExpandPartialD[RightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]]]
```

$$A_{\mu }.\left(\partial _{\mu }A_{\nu }\right)+\left(\partial _{\mu }A_{\mu }\right).A_{\nu }$$

```mathematica
ExpandPartialD[RightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]], RightPartialD -> True]
```

$$A_{\mu }.\left(\partial _{\mu }A_{\nu }\right)+\left(\partial _{\mu }A_{\mu }\right).A_{\nu }+A_{\mu }.A_{\nu }.\vec{\partial }_{\mu }$$

The same applies also to `LeftPartialD`

```mathematica
ExpandPartialD[QuantumField[A, LorentzIndex[\[Nu]]] . LeftNablaD[i]]
```

$$-\left(\partial _iA_{\nu }\right)$$

```mathematica
ExpandPartialD[QuantumField[A, LorentzIndex[\[Nu]]] . LeftNablaD[i], LeftPartialD -> True]
```

$$-\left(\partial _iA_{\nu }\right)-\overleftarrow{\partial }_i.A_{\nu }$$