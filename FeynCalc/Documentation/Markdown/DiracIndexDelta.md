## DiracIndexDelta

`DiracIndexDelta[DiracIndex[i], DiracIndex[j]]` is the Kronecker-delta in the Dirac space with two explicit Dirac indices `i` and `j`.

### See also

[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DIDelta](DIDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).

### Examples

```mathematica
DiracIndexDelta[DiracIndex[i], DiracIndex[j]]
```

$$\delta _{ij}$$

```mathematica
ex = DiracIndexDelta[DiracIndex[i], DiracIndex[j]]^2
```

$$\delta _{ij}^2$$

```mathematica
DiracChainJoin[ex]
```

$$4$$

```mathematica
DiracChainJoin[ex, TraceOfOne -> D]
```

$$D$$

```mathematica
ex = DiracIndexDelta[DiracIndex[i], DiracIndex[j]] DiracIndexDelta[DiracIndex[j], DiracIndex[k]]
```

$$\delta _{ij} \delta _{jk}$$

```mathematica
DiracChainJoin[ex]
```

$$\delta _{ik}$$

```mathematica
ex = DiracIndexDelta[DiracIndex[i2], DiracIndex[i3]] DiracIndexDelta[DiracIndex[i4], DiracIndex[i5]] DiracChain[DiracIndex[i7], Spinor[-Momentum[q], 0, 1]] DiracChain[Spinor[Momentum[p], m, 1], DiracIndex[i0]] DiracChain[DiracGamma[LorentzIndex[\[Mu]]], DiracIndex[i1], DiracIndex[i2]] DiracChain[DiracGamma[LorentzIndex[\[Nu]]], DiracIndex[i5], DiracIndex[i6]] DiracChain[m + DiracGamma[Momentum[p]], DiracIndex[i3], DiracIndex[i4]]
```

$$\delta _{\text{i2}\;\text{i3}} \delta _{\text{i4}\;\text{i5}} \left(\bar{\gamma }^{\mu }\right){}_{\text{i1}\;\text{i2}} \left(\bar{\gamma }^{\nu }\right){}_{\text{i5}\;\text{i6}} \left(\varphi (-\overline{q})\right)_{\text{i7}} \left(\bar{\gamma }\cdot \overline{p}+m\right)_{\text{i3}\;\text{i4}} \left(\varphi (\overline{p},m)\right)_{\text{i0}}$$

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
