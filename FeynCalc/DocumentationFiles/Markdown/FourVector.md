##  FourVector 

FourVector[p, mu] is the four-dimensional vector p with Lorentz index $\mu$. A vector with space-time Dimension D is obtained by supplying the option Dimension -> D.The shortcut FourVector is deprecated, please use FV instead!.

###  See also 

FV, FCI.

###  Examples 

```mathematica
FourVector[p, \[Mu]] 
 
FourVector[p - q, \[Mu]] 
 
StandardForm[FourVector[p, \[Mu]]] 
 
StandardForm[FourVector[p, \[Mu], Dimension -> D]]
```

$$\overline{p}^{\mu }$$

$$\left(\overline{p}-\overline{q}\right)^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu]], Momentum[p]]*)

(*Pair[LorentzIndex[\[Mu], D], Momentum[p, D]]*)
```

FourVector is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use FV.

```mathematica
FV[p, \[Mu]] 
 
FVD[p, \[Mu]] 
 
FCI[FV[p, \[Mu]]] === FourVector[p, \[Mu]] 
 
FCI[FVD[p, \[Mu]]] === FourVector[p, \[Mu], Dimension -> D]
```

$$\overline{p}^{\mu }$$

$$p^{\mu }$$

$$\text{True}$$

$$\text{True}$$