##  MT 

MT[mu, nu] is the metric tensor in 4 dimensions..

###  See also 

FeynCalcExternal, FCE, FCI, MTD, MTE.

###  Examples 

```mathematica
MT[\[Alpha], \[Beta]] 
 
Contract[MT[\[Alpha], \[Beta]] MT[\[Alpha], \[Beta]]] 
 
MT[a, b] // StandardForm 
 
FCI[MT[a, b]] // StandardForm 
 
FCE[FCI[MT[a, b]]] // StandardForm
```

$$\bar{g}^{\alpha \beta }$$

$$4$$

```
(*MT[a, b]*)

(*Pair[LorentzIndex[a], LorentzIndex[b]]*)

(*MT[a, b]*)
```