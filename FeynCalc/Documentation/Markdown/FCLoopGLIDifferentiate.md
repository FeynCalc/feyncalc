## FCLoopGLIDifferentiate

`FCLoopGLIDifferentiate[exp , topos, inv]` calculates the partial derivative of GLIs present in `exp` with respect to the scalar quantity `inv`.
Here `inv` can be a constant (e.g. mass), a scalar product of some momenta or a 4-vector.

The list topos must contain the topologies describing all of the occurring GLIs.

To calculate multiple derivatives, use the notation `FCLoopGLIDifferentiate[exp , topos, {inv,n}]` for scalars and
`FCLoopGLIDifferentiate[exp , topos, {vec1, vec2, ...}]` for vectors.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopGLIExpand](FCLoopGLIExpand.md).

### Examples

```mathematica
FCLoopGLIDifferentiate[x GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, m1]
```

$$2 \;\text{m1} x G^{\text{tad2l}}(2,1,1)$$

```mathematica
FCLoopGLIDifferentiate[x GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, {m1, 5}]
```

$$3840 \;\text{m1}^5 x G^{\text{tad2l}}(6,1,1)+3840 \;\text{m1}^3 x G^{\text{tad2l}}(5,1,1)+720 \;\text{m1} x G^{\text{tad2l}}(4,1,1)$$

```mathematica
FCLoopGLIDifferentiate[m2^2 GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, m2]
```

$$2 \;\text{m2}^3 G^{\text{tad2l}}(1,2,1)+2 \;\text{m2} G^{\text{tad2l}}(1,1,1)$$

```mathematica
FCLoopGLIDifferentiate[ GLI[prop1l, {1, 1}] + SPD[q] GLI[prop1l, {1, 0}], 
  {FCTopology[prop1l, {FAD[{p1, m1}], FAD[{p1 + q, m2}]}, {p1}, {q}, {}, {}]}, SPD[q]]
```

$$G^{\text{prop1l}}(1,0)-G^{\text{prop1l}}(1,2)$$

```mathematica
FCLoopGLIDifferentiate[SPD[p1, p2] GLI[topo1, {1, 1, 1}], 
  {FCTopology[topo1, {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[p1 - p2]}, {p1, p2}, {}, {}, {}]}, 
  FVD[p1, mu], FCE -> True]
```

$$-2 G^{\text{topo1}}(1,1,2) \left(\text{p1}^{\text{mu}}-\text{p2}^{\text{mu}}\right) (\text{p1}\cdot \;\text{p2})-2 \;\text{p1}^{\text{mu}} G^{\text{topo1}}(2,1,1) (\text{p1}\cdot \;\text{p2})+\text{p2}^{\text{mu}} G^{\text{topo1}}(1,1,1)$$

```mathematica
FCLoopGLIDifferentiate[SPD[p1, p2] GLI[topo1, {1, 1, 1}], {FCTopology[topo1, 
    {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[p1 - p2]}, {p1, p2}, {}, {}, {}]} 
 , {FVD[p1, mu], FVD[p2, nu]}, FCE -> True]
```

$$-2 G^{\text{topo1}}(1,1,2) \left(-g^{\text{mu}\;\text{nu}} (\text{p1}\cdot \;\text{p2})-2 \;\text{p2}^{\text{mu}} \;\text{p1}^{\text{nu}}+\text{p1}^{\text{mu}} \;\text{p1}^{\text{nu}}+\text{p2}^{\text{mu}} \;\text{p2}^{\text{nu}}\right)+G^{\text{topo1}}(1,1,1) g^{\text{mu}\;\text{nu}}-8 G^{\text{topo1}}(1,1,3) \left(\text{p1}^{\text{mu}}-\text{p2}^{\text{mu}}\right) \left(\text{p1}^{\text{nu}}-\text{p2}^{\text{nu}}\right) (\text{p1}\cdot \;\text{p2})+4 \;\text{p2}^{\text{nu}} G^{\text{topo1}}(1,2,2) \left(\text{p1}^{\text{mu}}-\text{p2}^{\text{mu}}\right) (\text{p1}\cdot \;\text{p2})-4 \;\text{p1}^{\text{mu}} G^{\text{topo1}}(2,1,2) \left(\text{p1}^{\text{nu}}-\text{p2}^{\text{nu}}\right) (\text{p1}\cdot \;\text{p2})+4 \;\text{p1}^{\text{mu}} \;\text{p2}^{\text{nu}} G^{\text{topo1}}(2,2,1) (\text{p1}\cdot \;\text{p2})-2 \;\text{p1}^{\text{mu}} \;\text{p1}^{\text{nu}} G^{\text{topo1}}(2,1,1)-2 \;\text{p2}^{\text{mu}} \;\text{p2}^{\text{nu}} G^{\text{topo1}}(1,2,1)$$