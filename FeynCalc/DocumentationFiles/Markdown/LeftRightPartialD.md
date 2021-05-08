##  LeftRightPartialD 

LeftRightPartialD[mu] denotes $\overleftrightarrow{\partial }_{\mu }$, acting to the left and right. ExplicitPartialD[LeftRightPartialD[$\mu$]] gives 1/2 (RightPartialD[$\mu$] - LeftPartialD[$\mu$])..

###  See also 

ExplicitPartialD, ExpandPartialD, FCPartialD, LeftPartialD, LeftRightPartialD2, RightPartialD.

###  Examples 

```mathematica
LeftRightPartialD[\[Mu]] 
 
ExplicitPartialD[%] 
 
LeftRightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Nu]]] 
 
ExpandPartialD[%] 
 
QuantumField[A, LorentzIndex[\[Mu]]] . LeftRightPartialD[\[Nu]] . QuantumField[A, LorentzIndex[\[Rho]]] 
 
ExpandPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }$$

$$\frac{1}{2} \left(\vec{\partial }_{\mu }-\overleftarrow{\partial }_{\mu }\right)$$

$$\overleftrightarrow{\partial }_{\mu }.A_{\nu }$$

$$\frac{1}{2} \left(\left.(\partial _{\mu }A_{\nu }\right)-\overleftarrow{\partial }_{\mu }.A_{\nu }\right)$$

$$A_{\mu }.\overleftrightarrow{\partial }_{\nu }.A_{\rho }$$

$$\frac{1}{2} \left(A_{\mu }.\left(\left.(\partial _{\nu }A_{\rho }\right)\right)-\left(\left.(\partial _{\nu }A_{\mu }\right)\right).A_{\rho }\right)$$