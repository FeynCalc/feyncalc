## DiracChainExpand

`DiracChainExpand[exp]` expands all Dirac chains with explicit indices using linearity, e.g. `DCHN[GA[p1]+GA[p2]+m,i,j]` becomes `DCHN[GA[p1],i,j]+DCHN[GA[p2],i,j]+m*DCHN[1,i,j]`.

### See also

[DiracChain](DiracChain), [DCHN](DCHN), [DiracIndex](DiracIndex), [DiracIndexDelta](DiracIndexDelta), [DIDelta](DIDelta), [DiracChainJoin](DiracChainJoin), [DiracChainCombine](DiracChainCombine), [DiracChainFactor](DiracChainFactor).

### Examples

```mathematica
DCHN[(GS[p] + m) . GA[mu], i, j]
DiracChainExpand[%]
```

$$\left(\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\text{mu}}\right){}_{ij}$$

$$m \left(\bar{\gamma }^{\text{mu}}\right){}_{ij}+\left(\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^{\text{mu}}\right){}_{ij}$$