## LeftRightNablaD2

`LeftRightNablaD2[mu]` denotes $\overleftrightarrow{\nabla }_{i}$, acting to the left and right.

`ExplicitPartialD[LeftRightNablaD2[mu]]` gives `(RightNablaD[i] + LeftNablaD[i])`.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftNablaD](LeftNablaD.md), [RightNablaD](RightNablaD.md).

### Examples

```mathematica
LeftRightNablaD2[i] 
 
ExplicitPartialD[%]
```

$$\overleftrightarrow{\nabla }_i$$

$$-\overleftarrow{\partial }_i-\vec{\partial }_i$$

```mathematica
LeftRightNablaD2[i] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%]
```

$$\overleftrightarrow{\nabla }_i.A_{\nu }$$

$$-\left(\partial _iA_{\nu }\right)-\overleftarrow{\partial }_i.A_{\nu }$$

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . LeftRightNablaD2[i] . QuantumField[A, LorentzIndex[\[Rho]]] 
 
ExpandPartialD[%] 
  
 

```

$$A_{\mu }.\overleftrightarrow{\nabla }_i.A_{\rho }$$

$$-A_{\mu }.\left(\partial _iA_{\rho }\right)-\left(\partial _iA_{\mu }\right).A_{\rho }$$