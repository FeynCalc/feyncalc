##  LeftPartialD 

LeftPartialD[μ] denotes $\overleftarrow{\partial }_{\mu }$acting to the left..

###  See also 

ExpandPartialD, FCPartialD, LeftRightPartialD, RightPartialD.

###  Examples 

```mathematica
QuantumField[A, LorentzIndex[\[Mu]]] . LeftPartialD[\[Nu]] 
 
ExpandPartialD[%] 
 
StandardForm[%] 
 
StandardForm[LeftPartialD[\[Mu]]] 
 
QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]] . LeftPartialD[\[Rho]] 
 
ExpandPartialD[%] 
 
StandardForm[%]
```

$$A_{\mu }.\overleftarrow{\partial }_{\nu }$$

$$\left.(\partial _{\nu }A_{\mu }\right)$$

```
(*QuantumField[FCPartialD[LorentzIndex[\[Nu]]], A, LorentzIndex[\[Mu]]]*)

(*LeftPartialD[LorentzIndex[\[Mu]]]*)
```

$$A_{\mu }.A_{\nu }.\overleftarrow{\partial }_{\rho }$$

$$A_{\mu }.\left(\left.(\partial _{\rho }A_{\nu }\right)\right)+\left(\left.(\partial _{\rho }A_{\mu }\right)\right).A_{\nu }$$

```
(*QuantumField[A, LorentzIndex[\[Mu]]] . QuantumField[FCPartialD[LorentzIndex[\[Rho]]], A, LorentzIndex[\[Nu]]] + QuantumField[FCPartialD[LorentzIndex[\[Rho]]], A, LorentzIndex[\[Mu]]] . QuantumField[A, LorentzIndex[\[Nu]]]*)
```