##  FCPartialD 

`FCPartialD[μ]` denotes the four-dimensional $\partial _{\mu }$.

`FCPartialD` is used to denote derivative fields.

`FCPartialD[LorentzIndex[μ ,D]]` denotes the $D$-dimensional $\partial _{\mu }$.

###  See also 

ExpandPartialD, LeftPartialD, LeftRightPartialD, RightPartialD.

###  Examples 

```mathematica
QuantumField[A, {\[Mu]}] . LeftPartialD[\[Nu]]
ExpandPartialD[%]
StandardForm[%]
```

$$A_{\mu }.\overleftarrow{\partial }_{\nu }$$

$$\left.(\partial _{\nu }A_{\mu }\right)$$

```
(*QuantumField[FCPartialD[LorentzIndex[\[Nu]]], A, LorentzIndex[\[Mu]]]*)
```