##  FVE 

FVE[p, mu] is the D-4-dimensional vector p with Lorentz index $\mu$..

###  See also 

FCE, FCI, FV, FVD, Pair.

###  Examples 

```mathematica
FVE[p, \[Mu]] 
 
FVE[p - q, \[Mu]] 
 
FVE[p, \[Mu]] // StandardForm 
 
FCI[FVE[p, \[Mu]]] // StandardForm
```

$$\hat{p}^{\mu }$$

$$\left(\hat{p}-\hat{q}\right)^{\mu }$$

```
(*FVE[p, \[Mu]]*)

(*Pair[LorentzIndex[\[Mu], -4 + D], Momentum[p, -4 + D]]*)
```

There is no special function to expand momenta in FVE.

```mathematica
ExpandScalarProduct[FVE[p - q, \[Mu]]] 
 
StandardForm[%] 
 
Contract[FVE[p, \[Mu]] FV[q, \[Mu]]]
```

$$\hat{p}^{\mu }-\hat{q}^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu], -4 + D], Momentum[p, -4 + D]] - Pair[LorentzIndex[\[Mu], -4 + D], Momentum[q, -4 + D]]*)
```

$$0$$