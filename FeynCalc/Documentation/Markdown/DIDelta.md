## DIDelta

`DIDelta[i, j]` is the Kronecker-delta in the Dirac space.

`DIDelta[i,j]` is transformed into `DiracDelta[DiracIndex[i],DiracIndex[j]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).

### Examples

```mathematica
DIDelta[i, j]
```

$$\delta _{ij}$$

```mathematica
DIDelta[i, i]
DiracChainJoin[%]
```

$$\delta _{ii}$$

$$4$$

```mathematica
DIDelta[i, j]^2
DiracChainJoin[%]
```

$$\delta _{ij}^2$$

$$4$$

```mathematica
DIDelta[i, j] DIDelta[j, k]
DiracChainJoin[%]
```

$$\delta _{ij} \delta _{jk}$$

$$\delta _{ik}$$

```mathematica
ex = DCHN[SpinorUBar[p, m], i0] DCHN[GA[\[Mu]], i1, i2] DCHN[GS[p] + m, i3, i4] DCHN[GA[\[Nu]], i5, i6] DIDelta[i2, i3] DIDelta[i4, i5] DCHN[i7, SpinorV[q]]
```

$$\delta _{\text{i2}\;\text{i3}} \delta _{\text{i4}\;\text{i5}} (v(q))_{\text{i7}} \left(\bar{\gamma }^{\mu }\right){}_{\text{i1}\;\text{i2}} \left(\bar{\gamma }^{\nu }\right){}_{\text{i5}\;\text{i6}} \left(\bar{u}(p,m)\right)_{\text{i0}} \left(\bar{\gamma }\cdot \overline{p}+m\right)_{\text{i3}\;\text{i4}}$$

```mathematica
ex // FCI // StandardForm

(*DiracChain[DiracIndex[i7], Spinor[-Momentum[q], 0, 1]] DiracChain[Spinor[Momentum[p], m, 1], DiracIndex[i0]] DiracChain[DiracGamma[LorentzIndex[\[Mu]]], DiracIndex[i1], DiracIndex[i2]] DiracChain[DiracGamma[LorentzIndex[\[Nu]]], DiracIndex[i5], DiracIndex[i6]] DiracChain[m + DiracGamma[Momentum[p]], DiracIndex[i3], DiracIndex[i4]] DiracIndexDelta[DiracIndex[i2], DiracIndex[i3]] DiracIndexDelta[DiracIndex[i4], DiracIndex[i5]]*)
```

```mathematica
DiracChainJoin[ex]
```

$$\left(\varphi (-\overline{q})\right)_{\text{i7}} \left(\varphi (\overline{p},m)\right)_{\text{i0}} \left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }\right){}_{\text{i1}\;\text{i6}}$$

```mathematica
DiracChainJoin[ex DIDelta[i0, i1]]
```

$$\left(\varphi (-\overline{q})\right)_{\text{i7}} \left(\varphi (\overline{p},m).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }\right){}_{\text{i6}}$$

```mathematica
DiracChainJoin[ex DIDelta[i7, i6]]
```

$$\left(\varphi (\overline{p},m)\right)_{\text{i0}} \left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }.\varphi (-\overline{q})\right){}_{\text{i1}}$$
