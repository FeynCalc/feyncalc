##  MetricTensor 

MetricTensor[mu, nu] is the metric tensor. The default dimension is 4.The shortcut MetricTensor is deprecated, please use MT instead!.

###  See also 

FCI, MT, MTD.

###  Examples 

```mathematica
MetricTensor[\[Alpha], \[Beta]] 
 
Contract[% %] 
 
MetricTensor[\[Alpha], \[Beta], Dimension -> D] 
 
Contract[% %] 
 
StandardForm[MetricTensor[a, b]]
```

$$\bar{g}^{\alpha \beta }$$

$$4$$

$$g^{\alpha \beta }$$

$$D$$

```
(*Pair[LorentzIndex[a], LorentzIndex[b]]*)
```

MetricTensor is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use MT.

```mathematica
MT[\[Mu], \[Nu]] 
 
MTD[\[Mu], \[Nu]] 
 
FCI[MT[\[Mu], \[Nu]]] === MetricTensor[\[Mu], \[Nu]] 
 
FCI[MTD[\[Mu], \[Nu]]] === MetricTensor[\[Mu], \[Nu], Dimension -> D]
```

$$\bar{g}^{\mu \nu }$$

$$g^{\mu \nu }$$

$$\text{True}$$

$$\text{True}$$