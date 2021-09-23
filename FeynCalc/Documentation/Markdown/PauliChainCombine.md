## PauliChainCombine

`PauliChainCombine[exp]`  is (nearly) the inverse operation to `PauliChainExpand`.

### See also

[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [DIDelta](DIDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).

### Examples

```mathematica
(PCHN[CSISD[q], Dir3, Dir4] FAD[{k, me}])/(2 CSPD[q, q]) + 1/(2 CSPD[q, q]) FAD[k, {k - q, me}] (-2 DCHN[CSISD[q], Dir3, Dir4] CSPD[q, q] + 2 DCHN[1, Dir3, Dir4] me CSPD[q, q] + DCHN[CSISD[q], Dir3, Dir4] (-me^2 + CSPD[q, q]))
PauliChainCombine[%]
```

$$\frac{\left(q^2-\text{me}^2\right) \left((1)_{\text{Dir3}\;\text{Dir4}} \sigma \cdot q\right)+2 \;\text{me} q^2 (1)_{\text{Dir3}\;\text{Dir4}}-2 q^2 \left((1)_{\text{Dir3}\;\text{Dir4}} \sigma \cdot q\right)}{2 q^2 k^2.\left((k-q)^2-\text{me}^2\right)}+\frac{(\sigma \cdot q)_{\text{Dir3}\;\text{Dir4}}}{2 q^2 \left(k^2-\text{me}^2\right)}$$

$$\frac{(1)_{\text{Dir3}\;\text{Dir4}} \left(q^2-\text{me}^2\right) \sigma \cdot q+2 \;\text{me} q^2 (1)_{\text{Dir3}\;\text{Dir4}}-2 q^2 (1)_{\text{Dir3}\;\text{Dir4}} \sigma \cdot q}{2 q^2 k^2.\left((k-q)^2-\text{me}^2\right)}+\frac{(\sigma \cdot q)_{\text{Dir3}\;\text{Dir4}}}{2 q^2 \left(k^2-\text{me}^2\right)}$$
