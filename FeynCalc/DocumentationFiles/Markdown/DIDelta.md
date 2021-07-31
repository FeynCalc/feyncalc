`DIDelta[i, j]` is the Kronecker-delta in the Dirac space.

`DIDelta[i,j]` is transformed into `DiracDelta[DiracIndex[i],DiracIndex[j]]` by `FeynCalcInternal`.

### See also

[DiracChain](DiracChain), [DCHN](DCHN), [DiracIndex](DiracIndex), [DiracIndexDelta](DiracIndexDelta), [DiracChainJoin](DiracChainJoin), [DiracChainExpand](DiracChainExpand), [DiracChainFactor](DiracChainFactor).

### Examples

```mathematica
DIDelta[i, j]
```

$$\text{DIDelta}(i,j)$$

```mathematica
DIDelta[i, i]
DiracChainJoin[%]
```

$$\text{DIDelta}(i,i)$$

$$\text{DiracChainJoin}(\text{DIDelta}(i,i))$$

```mathematica
DIDelta[i, j]^2
DiracChainJoin[%]
```

$$\text{DIDelta}(i,j)^2$$

$$\text{DiracChainJoin}\left(\text{DIDelta}(i,j)^2\right)$$

```mathematica
DIDelta[i, j] DIDelta[j, k]
DiracChainJoin[%]
```

$$\text{DIDelta}(i,j) \text{DIDelta}(j,k)$$

$$\text{DiracChainJoin}(\text{DIDelta}(i,j) \text{DIDelta}(j,k))$$

```mathematica
ex = DCHN[SpinorUBar[p, m], i0] DCHN[GA[\[Mu]], i1, i2] DCHN[GS[p] + m, i3, i4] DCHN[GA[\[Nu]], i5, i6] DIDelta[i2, i3] DIDelta[i4, i5] DCHN[i7, SpinorV[q]]
```

$$\text{DIDelta}(\text{i2},\text{i3}) \text{DIDelta}(\text{i4},\text{i5}) \text{DCHN}(\text{i7},\text{SpinorV}(q)) \text{DCHN}(\text{GA}(\mu ),\text{i1},\text{i2}) \text{DCHN}(\text{GA}(\nu ),\text{i5},\text{i6}) \text{DCHN}(\text{SpinorUBar}(p,m),\text{i0}) \text{DCHN}(\text{GS}(p)+m,\text{i3},\text{i4})$$

```mathematica
ex // FCI // StandardForm

(*FCI[DCHN[i7, SpinorV[q]] DCHN[SpinorUBar[p, m], i0] DCHN[GA[\[Mu]], i1, i2] DCHN[GA[\[Nu]], i5, i6] DCHN[m + GS[p], i3, i4] DIDelta[i2,i3] DIDelta[i4, i5]]*)
```

```mathematica
DiracChainJoin[ex]
```

$$\text{DiracChainJoin}(\text{DIDelta}(\text{i2},\text{i3}) \text{DIDelta}(\text{i4},\text{i5}) \text{DCHN}(\text{i7},\text{SpinorV}(q)) \text{DCHN}(\text{GA}(\mu ),\text{i1},\text{i2}) \text{DCHN}(\text{GA}(\nu ),\text{i5},\text{i6}) \text{DCHN}(\text{SpinorUBar}(p,m),\text{i0}) \text{DCHN}(\text{GS}(p)+m,\text{i3},\text{i4}))$$

```mathematica
DiracChainJoin[ex DIDelta[i0, i1]]
```

$$\text{DiracChainJoin}(\text{DIDelta}(\text{i0},\text{i1}) \text{DIDelta}(\text{i2},\text{i3}) \text{DIDelta}(\text{i4},\text{i5}) \text{DCHN}(\text{i7},\text{SpinorV}(q)) \text{DCHN}(\text{GA}(\mu ),\text{i1},\text{i2}) \text{DCHN}(\text{GA}(\nu ),\text{i5},\text{i6}) \text{DCHN}(\text{SpinorUBar}(p,m),\text{i0}) \text{DCHN}(\text{GS}(p)+m,\text{i3},\text{i4}))$$

```mathematica
DiracChainJoin[ex DIDelta[i7, i6]]
```

$$\text{DiracChainJoin}(\text{DIDelta}(\text{i2},\text{i3}) \text{DIDelta}(\text{i4},\text{i5}) \text{DIDelta}(\text{i7},\text{i6}) \text{DCHN}(\text{i7},\text{SpinorV}(q)) \text{DCHN}(\text{GA}(\mu ),\text{i1},\text{i2}) \text{DCHN}(\text{GA}(\nu ),\text{i5},\text{i6}) \text{DCHN}(\text{SpinorUBar}(p,m),\text{i0}) \text{DCHN}(\text{GS}(p)+m,\text{i3},\text{i4}))$$