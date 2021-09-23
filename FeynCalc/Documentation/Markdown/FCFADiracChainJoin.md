## FCFADiracChainJoin

`FCFADiracChainJoin[exp]` processes the output of FeynArts (after `FCFAConvert`) with explicit Dirac indices and joins matrices and spinors into closed chains. This is necessary e. g. for models with 4-fermion operators, where FeynArts cannot determine the correct relative signs. When two matrices have a common index but the positions do not match, as in $A_{ij} B_{ik}$, it is assumed that we can take the charge conjugate transposed of either matrix to obtain, e.g. $\left(C A^T C^{-1}\right)_{ji} B_{ik}$ or $\left(C B^TC^{-1}\right)_{ki} A_{ij}$.

### See also

[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DIDelta](DIDelta.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md), [DiracChainJoin](DiracChainJoin.md), [FCCCT](FCCCT.md).

### Examples

Create a closed chain for the 1-loop electron self-energy

```mathematica
-(1/(16 \[Pi]^4)) I el^2 DCHN[Spinor[-Momentum[p, D], me, 1], Dir1] DCHN[Spinor[Momentum[q, D], me, 1], Dir2] DCHN[GAD[Lor1], Dir1, Dir3] DCHN[GAD[Lor2], Dir2, Dir4] DCHN[me - GSD[k], Dir3, Dir4] FAD[{k, me}, k - q] MTD[Lor1, Lor2]
res = FCFADiracChainJoin[%]
```

$$-\frac{i \;\text{el}^2 g^{\text{Lor1}\;\text{Lor2}} \left(\gamma ^{\text{Lor1}}\right){}_{\text{Dir1}\;\text{Dir3}} \left(\gamma ^{\text{Lor2}}\right){}_{\text{Dir2}\;\text{Dir4}} (\text{me}-\gamma \cdot k)_{\text{Dir3}\;\text{Dir4}} (\varphi (-p,\text{me}))_{\text{Dir1}} (\varphi (q,\text{me}))_{\text{Dir2}}}{16 \pi ^4 \left(k^2-\text{me}^2\right).(k-q)^2}$$

$$-\frac{i \;\text{el}^2 g^{\text{Lor1}\;\text{Lor2}} (\varphi (q,\text{me})).\gamma ^{\text{Lor2}}.\left(-(\gamma \cdot k+\text{me}).\gamma ^{\text{Lor1}}\right).(\varphi (p,\text{me}))}{16 \pi ^4 \left(k^2-\text{me}^2\right).(k-q)^2}$$

Sometimes the ordering of the spinors is not the one wants to have. However, we can always transpose the chains to reorder the spinors as we like, which doesn't change the final result

```mathematica
SpinorChainTranspose[res, Select -> {{Spinor[__], Spinor[__]}}]
```

$$-\frac{i \;\text{el}^2 g^{\text{Lor1}\;\text{Lor2}} (\varphi (-p,\text{me})).\gamma ^{\text{Lor1}}.(\text{me}-\gamma \cdot k).\gamma ^{\text{Lor2}}.(\varphi (-q,\text{me}))}{16 \pi ^4 \left(k^2-\text{me}^2\right).(k-q)^2}$$

Using patterns in the `Select` option one can create very fine-grained criteria for transposing the chains.
