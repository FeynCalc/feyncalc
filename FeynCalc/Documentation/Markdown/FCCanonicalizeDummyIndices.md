## FCCanonicalizeDummyIndices

`FCCanonicalizeDummyIndices[expr]`  canonicalizes all dummy Lorentz indices in the expression. The option `Momentum` provides a possibility to limit the canonicalization only to particular `Momenta`.

With the option `LorentzIndexNames` one can provide a list of names to be used for the canonicalized indices, to have say $\mu$, $\nu$, $\rho$ etc. instead of some random names.

### See also

[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md).

### Examples

```mathematica
FVD[q, mu] FVD[p, mu] + FVD[q, nu] FVD[p, nu] + FVD[q, si] FVD[r, si]
FCCanonicalizeDummyIndices[%] // Factor2
```

$$p^{\text{mu}} q^{\text{mu}}+p^{\text{nu}} q^{\text{nu}}+q^{\text{si}} r^{\text{si}}$$

$$q^{\text{FCGV}(\text{li241})} \left(2 p^{\text{FCGV}(\text{li241})}+r^{\text{FCGV}(\text{li241})}\right)$$

```mathematica
Uncontract[SPD[q, p]^2, q, p, Pair -> All]
FCCanonicalizeDummyIndices[%, LorentzIndexNames -> {\[Mu], \[Nu]}] 
  
 

```

$$p^{\text{\$AL}(\text{\$32})} p^{\text{\$AL}(\text{\$33})} q^{\text{\$AL}(\text{\$32})} q^{\text{\$AL}(\text{\$33})}$$

$$p^{\mu } p^{\nu } q^{\mu } q^{\nu }$$
