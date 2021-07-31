## DiracChainCombine

`DiracChainCombine[exp]` is (nearly) the inverse operation to `DiracChainExpand`.

### See also

[DiracChain](DiracChain), [DCHN](DCHN), [DiracIndex](DiracIndex), [DiracIndexDelta](DiracIndexDelta), [DIDelta](DIDelta), [DiracChainJoin](DiracChainJoin), [DiracChainExpand](DiracChainExpand), [DiracChainFactor](DiracChainFactor).

### Examples

```mathematica
(DCHN[GSD[q], Dir3, Dir4] FAD[{k, me}])/(2 SPD[q, q]) + 1/(2 SPD[q, q]) FAD[k, {k - q, me}] (-2 DCHN[GSD[q], Dir3, Dir4] SPD[q, q] + 2 DCHN[1, Dir3, Dir4] me SPD[q, q] + DCHN[GSD[q], Dir3, Dir4] (-me^2 + SPD[q, q]))
DiracChainCombine[%]
```

$$\frac{\left(q^2-\text{me}^2\right) (\gamma \cdot q)_{\text{Dir3}\text{Dir4}}+2 \text{me} q^2 (1)_{\text{Dir3}\text{Dir4}}-2 q^2 (\gamma \cdot q)_{\text{Dir3}\text{Dir4}}}{2 q^2 k^2.\left((k-q)^2-\text{me}^2\right)}+\frac{(\gamma \cdot q)_{\text{Dir3}\text{Dir4}}}{2 q^2 \left(k^2-\text{me}^2\right)}$$

$$\frac{\left(\left(q^2-\text{me}^2\right) \gamma \cdot q+2 \text{me} q^2-2 q^2 \gamma \cdot q\right){}_{\text{Dir3}\text{Dir4}}}{2 q^2 k^2.\left((k-q)^2-\text{me}^2\right)}+\frac{(\gamma \cdot q)_{\text{Dir3}\text{Dir4}}}{2 q^2 \left(k^2-\text{me}^2\right)}$$