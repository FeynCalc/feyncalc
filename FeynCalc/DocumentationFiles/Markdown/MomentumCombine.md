`MomentumCombine[expr]` is the inverse operation to `MomentumExpand` and `ExpandScalarProduct`. `MomentumCombine` combines also `Pair`s.

### See also

[ExpandScalarProduct](ExpandScalarProduct), [Momentum](Momentum), [MomentumExpand](MomentumExpand).

### Examples

```mathematica
Momentum[p] - 2 Momentum[q] // MomentumCombine // StandardForm
FV[p, \[Mu]] + 2 FV[q, \[Mu]] 
MomentumCombine[%]
% // StandardForm
%% // ExpandScalarProduct

(*Momentum[p - 2 q]*)
```

$$\overline{p}^{\mu }+2 \overline{q}^{\mu }$$

$$\left(\overline{p}+2 \overline{q}\right)^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu]], Momentum[p + 2 q]]*)
```

$$\overline{p}^{\mu }+2 \overline{q}^{\mu }$$

```mathematica
3 Pair[LorentzIndex[\[Mu]], Momentum[p]] + 2 Pair[LorentzIndex[\[Mu]], Momentum[q]]
MomentumCombine[%]
StandardForm[%] 
  
 

```

$$3 \overline{p}^{\mu }+2 \overline{q}^{\mu }$$

$$\left(3 \overline{p}+2 \overline{q}\right)^{\mu }$$

```
(*Pair[LorentzIndex[\[Mu]], Momentum[3 p + 2 q]]*)
```