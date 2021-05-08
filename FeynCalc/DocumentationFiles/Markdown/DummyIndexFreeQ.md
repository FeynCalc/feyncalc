##  DummyIndexFreeQ 

DummyIndexFreeQ[exp, {head1, head2, ...}] returns True if the expression contains dummy indices with heads head1, head2, ... and False otherwise. As always in FeynCalc, Einstein summation convention is implicitly assumed. The function is optimized for large expressions, i.e. it is not so good as a criterion in e.g. Select.

###  See also 

FCRenameDummyIndices, Contract, FreeIndexFreeQ.

###  Examples 

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Nu]]] 
 
DummyIndexFreeQ[%, {LorentzIndex}] 
 
FCI[FV[p, \[Mu]] FV[q, \[Mu]]] 
 
DummyIndexFreeQ[%, {LorentzIndex}] 
 
FCI[SUNT[a, b]] 
 
DummyIndexFreeQ[%, {SUNIndex}] 
 
FCI[SUNT[a, a]] 
 
DummyIndexFreeQ[%, {SUNIndex}]
```

$$\overline{p}^{\mu } \overline{q}^{\nu }$$

$$\text{True}$$

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\text{False}$$

$$T^a.T^b$$

$$\text{True}$$

$$T^a.T^a$$

$$\text{False}$$