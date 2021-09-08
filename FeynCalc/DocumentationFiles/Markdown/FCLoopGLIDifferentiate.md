## FCLoopGLIDifferentiate

`FCLoopGLIDifferentiate[exp , topos, inv]` calculates the partial derivative of GLIs present in `exp` with respect to the scalar quantity `inv`.
Here `inv` can be a constant (e.g. mass) or a scalar product of some momenta.  The list topos must contain the topologies describing all of the occurring GLIs.

To calculate multiple derivatives, use the notation `FCLoopGLIDifferentiate[exp , topos, {inv,n}]`

### See also

[FCTopology](FCTopology), [GLI](GLI), [FCLoopGLIExpand](FCLoopGLIExpand).

### Examples

```mathematica
FCLoopGLIDifferentiate[x GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, m1]
```

$$2 \text{m1} x G^{\text{tad2l}}(2,1,1)$$

```mathematica
FCLoopGLIDifferentiate[x GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, {m1, 5}]
```

$$288 x \left(8 \text{m1}^3 G^{\text{tad2l}}(5,1,1)+2 \text{m1} G^{\text{tad2l}}(4,1,1)\right)+384 x \left(10 \text{m1}^5 G^{\text{tad2l}}(6,1,1)+4 \text{m1}^3 G^{\text{tad2l}}(5,1,1)\right)+144 \text{m1} x G^{\text{tad2l}}(4,1,1)$$

```mathematica
FCLoopGLIDifferentiate[m2^2 GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, m2]
```

$$2 \text{m2}^3 G^{\text{tad2l}}(1,2,1)+2 \text{m2} G^{\text{tad2l}}(1,1,1)$$

```mathematica
FCLoopGLIDifferentiate[ GLI[prop1l, {1, 1}] + SPD[q] GLI[prop1l, {1, 0}], 
  {FCTopology[prop1l, {FAD[{p1, m1}], FAD[{p1 + q, m2}]}, {p1}, {q}, {}, {}]}, SPD[q]]
```

$$G^{\text{prop1l}}(1,0)-G^{\text{prop1l}}(1,2)$$