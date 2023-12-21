## FCLoopCanonicalize

`FCLoopCanonicalize[exp, {q1, q2, ...}, loopHead]` is an auxiliary internal function that canonicalizes indices of multi-loop integrals with loop momenta `q1, q2, ...` that are wrapped with `loopHead`. The output is given as a list of 4 entries, of which the last one contains a list of all the unique loop integrals in the given expression. After those are simplified, the original output of `FCLoopCanonicalize` together with the list of the simplified unique integrals should be inserted into `FCLoopSolutionList` to obtain the final replacement list that will be applied to the original expression.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopSolutionList](FCLoopSolutionList.md).

### Examples

```mathematica
FCLoopCanonicalize[myHead[FVD[q, \[Mu]]], q, myHead]
```

$$\left(
\begin{array}{c}
 \;\text{myHead}\left(q^{\mu }\right) \\
 \{\text{FCGV}(\text{cli191})\to \mu \} \\
 \;\text{myHead}\left(q^{\text{FCGV}(\text{cli191})}\right) \\
 \;\text{myHead}\left(q^{\text{FCGV}(\text{cli191})}\right) \\
\end{array}
\right)$$

```mathematica
FCLoopCanonicalize[myHead[FVD[q, \[Mu]] FVD[q, \[Nu]] FAD[q, {q + p, m}]] + myHead[FVD[q, \[Rho]] FVD[q, \[Sigma]] FAD[q, {q + p, m}]], q, myHead]
```

$$\left\{\left\{\text{myHead}\left(\frac{q^{\mu } q^{\nu }}{q^2.\left((p+q)^2-m^2\right)}\right),\text{myHead}\left(\frac{q^{\rho } q^{\sigma }}{q^2.\left((p+q)^2-m^2\right)}\right)\right\},\{\{\text{FCGV}(\text{cli201})\to \mu ,\text{FCGV}(\text{cli202})\to \nu \},\{\text{FCGV}(\text{cli201})\to \rho ,\text{FCGV}(\text{cli202})\to \sigma \}\},\left\{\text{myHead}\left(\frac{q^{\text{FCGV}(\text{cli201})} q^{\text{FCGV}(\text{cli202})}}{q^2.\left((p+q)^2-m^2\right)}\right),\text{myHead}\left(\frac{q^{\text{FCGV}(\text{cli201})} q^{\text{FCGV}(\text{cli202})}}{q^2.\left((p+q)^2-m^2\right)}\right)\right\},\left\{\text{myHead}\left(\frac{q^{\text{FCGV}(\text{cli201})} q^{\text{FCGV}(\text{cli202})}}{q^2.\left((p+q)^2-m^2\right)}\right)\right\}\right\}$$

```mathematica
FCLoopCanonicalize[myHead[FVD[q1, \[Mu]] FVD[q2, \[Nu]]], {q1, q2}, myHead]
```

$$\left(
\begin{array}{c}
 \;\text{myHead}\left(\text{q1}^{\mu } \;\text{q2}^{\nu }\right) \\
 \{\text{FCGV}(\text{cli211})\to \mu ,\text{FCGV}(\text{cli212})\to \nu \} \\
 \;\text{myHead}\left(\text{q1}^{\text{FCGV}(\text{cli211})} \;\text{q2}^{\text{FCGV}(\text{cli212})}\right) \\
 \;\text{myHead}\left(\text{q1}^{\text{FCGV}(\text{cli211})} \;\text{q2}^{\text{FCGV}(\text{cli212})}\right) \\
\end{array}
\right)$$