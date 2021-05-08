##  FVD 

FVD[p, mu] is the D-dimensional vector p with Lorentz index $\mu$..

###  See also 

FCE, FCI, FV, Pair.

###  Examples 

```mathematica
FVD[p, \[Mu]] 
 
FVD[p - q, \[Mu]] 
 
FVD[p, \[Mu]] // StandardForm 
 
FCI[FVD[p, \[Mu]]] // StandardForm
```

$$p^{\mu }$$

$$(p-q)^{\mu }$$

```
(*FVD[p, \[Mu]]*)

(*Pair[LorentzIndex[\[Mu], D], Momentum[p, D]]*)
```

There is no special function to expand momenta in FVD.

```mathematica
ExpandScalarProduct[FVD[p - q, \[Mu]]] 
 
StandardForm[%]
```

$$p^{\mu }-q^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu], D], Momentum[p, D]] - Pair[LorentzIndex[\[Mu], D], Momentum[q, D]]*)
```