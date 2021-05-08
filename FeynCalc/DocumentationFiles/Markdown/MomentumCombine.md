##  MomentumCombine 

MomentumCombine[expr] is the inverse operation to MomentumExpand and ExpandScalarProduct. MomentumCombine combines also Pair`s..

###  See also 

ExpandScalarProduct, Momentum, MomentumExpand.

###  Examples 

```mathematica
Momentum[p] - 2 Momentum[q] // MomentumCombine // StandardForm 
 
FV[p, \[Mu]] + 2 FV[q, \[Mu]] 
 
MomentumCombine[%] 
 
% // StandardForm 
 
%% // ExpandScalarProduct 
 
StandardForm[%] 
 
3 Pair[LorentzIndex[\[Mu]], Momentum[p]] + 2 Pair[LorentzIndex[\[Mu]], Momentum[q]] 
 
MomentumCombine[%]

(*Momentum[p - 2 q]*)
```

$$\overline{p}^{\mu }+2 \overline{q}^{\mu }$$

$$\left(\overline{p}+2 \overline{q}\right)^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu]], Momentum[p + 2 q]]*)
```

$$\overline{p}^{\mu }+2 \overline{q}^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu]], Momentum[p]] + 2 Pair[LorentzIndex[\[Mu]], Momentum[q]]*)
```

$$3 \overline{p}^{\mu }+2 \overline{q}^{\mu }$$

$$\left(3 \overline{p}+2 \overline{q}\right)^{\mu }$$