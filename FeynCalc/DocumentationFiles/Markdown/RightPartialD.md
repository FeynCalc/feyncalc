##  RightPartialD 

RightPartialD[mu] denotes $\partial _{\mu }$, acting to the right..

###  See also 

ExpandPartialD, FCPartialD, LeftPartialD.

###  Examples 

```mathematica
RightPartialD[\[Mu]] 
 
RightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Mu]]] 
 
ExpandPartialD[%] 
 
% // StandardForm 
 
RightPartialD[\[Mu]] // StandardForm
```

$$\vec{\partial }_{\mu }$$

$$\vec{\partial }_{\mu }.A_{\mu }$$

$$\left.(\partial _{\mu }A_{\mu }\right)$$

```
(*QuantumField[FCPartialD[LorentzIndex[\[Mu]]], A, LorentzIndex[\[Mu]]]*)

(*RightPartialD[LorentzIndex[\[Mu]]]*)
```