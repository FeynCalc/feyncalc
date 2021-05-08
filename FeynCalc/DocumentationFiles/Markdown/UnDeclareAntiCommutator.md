##  UnDeclareAntiCommutator 

UnDeclareAntiCommutator[a, b] undeclares the value assigned to the anticommutator of a and b..

###  Examples 

```mathematica
AntiCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]], A], QuantumField[A]] = 0;
QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]] 
 
ExpandPartialD[%] 
 
UnDeclareAntiCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]], A], QuantumField[A]];
ExpandPartialD[QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]]]
```

$$A.A.\overleftarrow{\partial }_{\nu }$$

$$0$$

$$A.\left(\left.(\partial _{\nu }A\right)\right)+\left(\left.(\partial _{\nu }A\right)\right).A$$