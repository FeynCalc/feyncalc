## LeftRightNablaD

`LeftRightNablaD[i]` denotes $\overleftrightarrow {\nabla}_{i}$, acting to the left and right.

`ExplicitPartialD[LeftRightNablaD[i]]` gives `1/2 (RightNablaD[i] - LeftNablaD[i])`.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftNablaD](LeftNablaD.md), [LeftRightNablaD2](LeftRightNablaD2.md), [RightNablaD](RightNablaD.md).

### Examples

```mathematica
LeftRightNablaD[i] 
 
ExplicitPartialD[%]
```

$$\overleftrightarrow{\nabla }_i$$

$$\frac{1}{2} \overleftarrow{\partial }_i-\vec{\partial }_i$$

```mathematica
LeftRightNablaD[i] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%]
```

$$\overleftrightarrow{\nabla }_i.A_{\nu }$$

$$\frac{1}{2} \left(\overleftarrow{\partial }_i.A_{\nu }-\left(\partial _iA_{\nu }\right)\right)$$

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . LeftRightNablaD[i] . QuantumField[A, LorentzIndex[\[Rho]]] 
 
ExpandPartialD[%] 
  
 

```

$$A_{\mu }.\overleftrightarrow{\nabla }_i.A_{\rho }$$

$$\frac{1}{2} \left(\left(\partial _iA_{\mu }\right).A_{\rho }-A_{\mu }.\left(\partial _iA_{\rho }\right)\right)$$