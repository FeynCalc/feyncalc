## PauliChainExpand

`PauliChainExpand[exp]` expands all Pauli chains with explicit indices using linearity, e.g. `PCHN[CSIS[p1]+CSIS[p2]+m,i,j]` becomes `PCHN[CSIS[p1],i,j]+PCHN[CSIS[p2],i,j]+m*PCHN[1,i,j]`.

### See also

[PauliChain](PauliChain), [PCHN](PCHN), [PauliIndex](PauliIndex), [PauliIndexDelta](PauliIndexDelta), [DIDelta](DIDelta), [PauliChainJoin](PauliChainJoin), [PauliChainCombine](PauliChainCombine), [PauliChainFactor](PauliChainFactor).

### Examples

```mathematica
PCHN[(CSIS[p] + m) . CSI[a], i, j]
PauliChainExpand[%]
```

$$\left(\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^a\right){}_{ij}$$

$$m \left(\overline{\sigma }^a\right){}_{ij}+\left(\left(\overline{\sigma }\cdot \overline{p}\right).\overline{\sigma }^a\right){}_{ij}$$