##  ExpandPartialD 

ExpandPartialD[exp] expands noncommutative products of $\text{QuantumField}$'s and partial differentiation operators in exp and applies the Leibniz rule..

###  See also 

ExplicitPartialD, LeftPartialD, LeftRightPartialD, PartialDRelations, RightPartialD.

###  Examples 

```mathematica
RightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%] 
 
LeftRightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%] 
 
QuantumField[A, LorentzIndex[\[Mu]]] . (LeftRightPartialD[OPEDelta]^2) . QuantumField[A, LorentzIndex[\[Rho]]] 
 
ExpandPartialD[%] 
 
8 LeftRightPartialD[OPEDelta]^3 
 
ExplicitPartialD[%] 
 
ExpandPartialD[%] 
 
LC[\[Mu], \[Nu], \[Rho], \[Tau]] RightPartialD[\[Alpha], \[Mu], \[Beta], \[Nu]] 
 
ExpandPartialD[%]
```

$$\vec{\partial }_{\mu }.A_{\mu }.A_{\nu }$$

$$A_{\mu }.\left(\left.(\partial _{\mu }A_{\nu }\right)\right)+\left(\left.(\partial _{\mu }A_{\mu }\right)\right).A_{\nu }$$

$$\overleftrightarrow{\partial }_{\mu }.A_{\nu }$$

$$\frac{1}{2} \left(\left.(\partial _{\mu }A_{\nu }\right)-\overleftarrow{\partial }_{\mu }.A_{\nu }\right)$$

$$A_{\mu }.\overleftrightarrow{\partial }_{\Delta }^2.A_{\rho }$$

$$\frac{1}{4} \left(-2 \left(\left.(\partial _{\Delta }A_{\mu }\right)\right).\left(\left.(\partial _{\Delta }A_{\rho }\right)\right)+A_{\mu }.\left(\partial _{\Delta }\partial _{\Delta }A_{\rho }\right)+\left(\partial _{\Delta }\partial _{\Delta }A_{\mu }\right).A_{\rho }\right)$$

$$8 \overleftrightarrow{\partial }_{\Delta }^3$$

$$\left(\vec{\partial }_{\Delta }-\overleftarrow{\partial }_{\Delta }\right){}^3$$

$$-\overleftarrow{\partial }_{\Delta }.\overleftarrow{\partial }_{\Delta }.\overleftarrow{\partial }_{\Delta }+3 \overleftarrow{\partial }_{\Delta }.\overleftarrow{\partial }_{\Delta }.\vec{\partial }_{\Delta }-3 \overleftarrow{\partial }_{\Delta }.\vec{\partial }_{\Delta }.\vec{\partial }_{\Delta }+\vec{\partial }_{\Delta }.\vec{\partial }_{\Delta }.\vec{\partial }_{\Delta }$$

$$\bar{\epsilon }^{\mu \nu \rho \tau } \vec{\partial }_{\alpha }.\vec{\partial }_{\mu }.\vec{\partial }_{\beta }.\vec{\partial }_{\nu }$$

$$0$$