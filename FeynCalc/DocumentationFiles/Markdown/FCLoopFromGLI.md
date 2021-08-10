`FCLoopFromGLI[exp, topologies]` replaces `GLI`s in `exp` with the corresponding loop integrals in the `FeynAmpDenominator` notation according to the information provided in topologies.

### See also

[FCTopology](FCTopology), [GLI](GLI), [FCLoopValidTopologyQ](FCLoopValidTopologyQ).

### Examples

```mathematica
topos = {
   FCTopology["topoBox1L", {FAD[{q, m0}], FAD[{q + p1, m1}], FAD[{q + p2, m2}], FAD[{q + p2, m3}]}, {q}, {p1, p2, p3}, {}, {}], 
   FCTopology["topoTad2L", {FAD[{q1, m1}], FAD[{q2, m2}], FAD[{q1 - q2, 0}]}, {q1, q2}, {}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topoBox1L},\left\{\frac{1}{q^2-\text{m0}^2},\frac{1}{(\text{p1}+q)^2-\text{m1}^2},\frac{1}{(\text{p2}+q)^2-\text{m2}^2},\frac{1}{(\text{p2}+q)^2-\text{m3}^2}\right\},\{q\},\{\text{p1},\text{p2},\text{p3}\},\{\},\{\}\right),\text{FCTopology}\left(\text{topoTad2L},\left\{\frac{1}{\text{q1}^2-\text{m1}^2},\frac{1}{\text{q2}^2-\text{m2}^2},\frac{1}{(\text{q1}-\text{q2})^2}\right\},\{\text{q1},\text{q2}\},\{\},\{\},\{\}\right)\right\}$$

```mathematica
exp = a1 GLI["topoBox1L", {1, 1, 1, 1}] + a2 GLI["topoTad2L", {1, 2, 2}]
```

$$\text{a1} G^{\text{topoBox1L}}(1,1,1,1)+\text{a2} G^{\text{topoTad2L}}(1,2,2)$$

```mathematica
FCLoopFromGLI[exp, topos]
```

$$\frac{\text{a1}}{\left(q^2-\text{m0}^2\right) \left((\text{p1}+q)^2-\text{m1}^2\right) \left((\text{p2}+q)^2-\text{m2}^2\right) \left((\text{p2}+q)^2-\text{m3}^2\right)}+\frac{\text{a2}}{\left(\text{q1}^2-\text{m1}^2\right) \left(\text{q2}^2-\text{m2}^2\right)^2 (\text{q1}-\text{q2})^2^2}$$

Notice that it is necessary to specify all topologies present in `exp`. The function will not accept `GLI`s
defined for unknown topologies

```mathematica
FCLoopFromGLI[GLI["topoXYZ", {1, 1, 1, 1, 1}], topos] 
  
 

```

![0kvjd5dgpo2l4](img/0kvjd5dgpo2l4.svg)

$$\text{$\$$Aborted}$$