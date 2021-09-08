## FCLoopSolutionList

`FCLoopSolutionList[loopList, reversedRepIndexList, canIndexList, uniqueCanIndexList}, solsList]` is an auxiliary internal function that uses the output of FCLoopCanonicalize and the list of simplified integrals solsList to create the substitution list of type `"Integral" -> "simplified Integral"`.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopCanonicalize](FCLoopCanonicalize.md).

### Examples

```mathematica
li = FCLoopCanonicalize[myHead[FVD[q, \[Mu]] FVD[q, \[Nu]] FAD[q, {q + p, m}]] + myHead[FVD[q, \[Rho]] FVD[q, \[Sigma]] FAD[q, {q + p, m}]], q, myHead] 
```

$$\left\{\left\{\text{myHead}\left(\frac{q^{\mu } q^{\nu }}{q^2.\left((p+q)^2-m^2\right)}\right),\text{myHead}\left(\frac{q^{\rho } q^{\sigma }}{q^2.\left((p+q)^2-m^2\right)}\right)\right\},\{\{\text{FCGV}(\text{cli241})\to \mu ,\text{FCGV}(\text{cli242})\to \nu \},\{\text{FCGV}(\text{cli241})\to \rho ,\text{FCGV}(\text{cli242})\to \sigma \}\},\left\{\text{myHead}\left(\frac{q^{\text{FCGV}(\text{cli241})} q^{\text{FCGV}(\text{cli242})}}{q^2.\left((p+q)^2-m^2\right)}\right),\text{myHead}\left(\frac{q^{\text{FCGV}(\text{cli241})} q^{\text{FCGV}(\text{cli242})}}{q^2.\left((p+q)^2-m^2\right)}\right)\right\},\left\{\text{myHead}\left(\frac{q^{\text{FCGV}(\text{cli241})} q^{\text{FCGV}(\text{cli242})}}{q^2.\left((p+q)^2-m^2\right)}\right)\right\}\right\}$$

```mathematica
FCLoopSolutionList[li, prefactor (li[[4]] /. myHead -> Identity /. q -> p), Dispatch -> False]
```

$$\left\{\text{myHead}\left(\frac{q^{\mu } q^{\nu }}{q^2.\left((p+q)^2-m^2\right)}\right)\to \frac{\text{prefactor} p^{\mu } p^{\nu }}{p^2.\left(4 p^2-m^2\right)},\text{myHead}\left(\frac{q^{\rho } q^{\sigma }}{q^2.\left((p+q)^2-m^2\right)}\right)\to \frac{\text{prefactor} p^{\rho } p^{\sigma }}{p^2.\left(4 p^2-m^2\right)}\right\}$$
