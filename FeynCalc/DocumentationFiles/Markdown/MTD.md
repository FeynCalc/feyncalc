##  MTD 

MTD[mu, nu] is the metric tensor in D dimensions..

###  See also 

FeynCalcExternal, FCE, FCI, MT, MTE.

###  Examples 

```mathematica
MTD[\[Alpha], \[Beta]] 
 
Contract[MTD[\[Alpha], \[Beta]] MTD[\[Alpha], \[Beta]]] 
 
MTD[\[Alpha], \[Beta]] // StandardForm 
 
FCI[MTD[\[Alpha], \[Beta]]] // StandardForm 
 
FCE[FCI[MTD[\[Mu], \[Nu]]]] // StandardForm
```

$$g^{\alpha \beta }$$

$$D$$

```
(*MTD[\[Alpha], \[Beta]]*)

(*Pair[LorentzIndex[\[Alpha], D], LorentzIndex[\[Beta], D]]*)

(*MTD[\[Mu], \[Nu]]*)
```