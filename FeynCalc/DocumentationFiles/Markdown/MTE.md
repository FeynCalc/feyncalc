##  MTE 

MTE[mu, nu] is the metric tensor in D-4 dimensions..

###  See also 

FeynCalcExternal, FCE, FCI, MT, MTD.

###  Examples 

```mathematica
MTE[\[Alpha], \[Beta]] 
 
Contract[MTE[\[Alpha], \[Beta]] MTE[\[Alpha], \[Beta]]] 
 
Contract[MTE[\[Alpha], \[Beta]] MT[\[Alpha], \[Beta]]] 
 
Contract[MTE[\[Alpha], \[Beta]] MTD[\[Alpha], \[Beta]]] 
 
MTE[\[Alpha], \[Beta]] // StandardForm 
 
MTE[\[Alpha], \[Beta]] 
 
FCI[MTE[\[Alpha], \[Beta]]] // StandardForm 
 
FCE[FCI[MTE[\[Mu], \[Nu]]]] // StandardForm 
 
MTE[\[Mu], \[Nu]]
```

$$\hat{g}^{\alpha \beta }$$

$$D-4$$

$$0$$

$$D-4$$

```
(*MTE[\[Alpha], \[Beta]]*)
```

$$\hat{g}^{\alpha \beta }$$

```
(*Pair[LorentzIndex[\[Alpha], -4 + D], LorentzIndex[\[Beta], -4 + D]]*)

(*MTE[\[Mu], \[Nu]]*)
```

$$\hat{g}^{\mu \nu }$$