##  FV 

FV[p, mu] is the four-dimensional vector $p^{\mu }$..

###  See also 

FCE, FCI, FVD, Pair.

###  Examples 

```mathematica
FV[p, \[Mu]] 
 
FV[p - q, \[Mu]] 
 
FV[p, \[Mu]] // StandardForm 
 
FCI[FV[p, \[Mu]]] // StandardForm
```

$$\overline{p}^{\mu }$$

$$\left(\overline{p}-\overline{q}\right)^{\mu }$$

```
(*FV[p, \[Mu]]*)

(*Pair[LorentzIndex[\[Mu]], Momentum[p]]*)
```

ExpandScalarProduct is used to expand momenta in FV

```mathematica
ExpandScalarProduct[FV[p - q, \[Mu]]]
```

$$\overline{p}^{\mu }-\overline{q}^{\mu }$$