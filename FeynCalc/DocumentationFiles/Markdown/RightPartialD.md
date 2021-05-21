##  RightPartialD 

`RightPartialD[mu]` denotes $\partial _{\mu }$, acting to the right.

###  See also 

ExpandPartialD, FCPartialD, LeftPartialD.

###  Examples 

```mathematica
RightPartialD[\[Mu]]
```

$$\vec{\partial }_{\mu }$$

```mathematica
RightPartialD[\[Mu]] . QuantumField[A, LorentzIndex[\[Mu]]]
ExpandPartialD[%]
% // StandardForm
```

$$\vec{\partial }_{\mu }.A_{\mu }$$

$$\left.(\partial _{\mu }A_{\mu }\right)$$

```
(*QuantumField[FCPartialD[LorentzIndex[\[Mu]]], A, LorentzIndex[\[Mu]]]*)
```

```mathematica
RightPartialD[\[Mu]] // StandardForm

(*RightPartialD[LorentzIndex[\[Mu]]]*)
```