## FCLoopGLIExpand

`FCLoopGLIExpand[exp, topos, {x, x0, n}]` expands `GLI`s defined via the list of
topologies `topos` in `exp` around `x=x0` to order `n`. Here `x` must be a scalar quantity, e.g.
a mass or a scalar product.

This routine is particularly useful for doing asymptotic expansions of integrals or amplitudes.

Notice that the series is assumed to be well-defined. The function has no built-in checks against singular behavior.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopGLIDifferentiate](FCLoopGLIDifferentiate.md), [ToGFAD](ToGFAD.md).

### Examples

```mathematica
FCLoopGLIExpand[x GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, {m1, 0, 2}]
```

$$\left\{\text{m1}^2 x G^{\text{tad2l}}(2,1,1)+x G^{\text{tad2l}}(1,1,1),\left\{\text{FCTopology}\left(\text{tad2l},\left\{\frac{1}{\text{p1}^2},\frac{1}{\text{p2}^2-\text{m2}^2},\frac{1}{(\text{p1}-\text{p2})^2-\text{m3}^2}\right\},\{\text{p1},\text{p2}\},\{\},\{\},\{\}\right)\right\}\right\}$$

```mathematica
FCLoopGLIExpand[x GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, {m1, M, 4}]
```

$$\left\{16 M^8 x G^{\text{tad2l}}(5,1,1)-64 M^7 \;\text{m1} x G^{\text{tad2l}}(5,1,1)+96 M^6 \;\text{m1}^2 x G^{\text{tad2l}}(5,1,1)+4 M^6 x G^{\text{tad2l}}(4,1,1)-64 M^5 \;\text{m1}^3 x G^{\text{tad2l}}(5,1,1)-24 M^5 \;\text{m1} x G^{\text{tad2l}}(4,1,1)+16 M^4 \;\text{m1}^4 x G^{\text{tad2l}}(5,1,1)+48 M^4 \;\text{m1}^2 x G^{\text{tad2l}}(4,1,1)+M^4 x G^{\text{tad2l}}(3,1,1)-40 M^3 \;\text{m1}^3 x G^{\text{tad2l}}(4,1,1)+12 M^2 \;\text{m1}^4 x G^{\text{tad2l}}(4,1,1)-2 M^2 \;\text{m1}^2 x G^{\text{tad2l}}(3,1,1)-M^2 x G^{\text{tad2l}}(2,1,1)+\text{m1}^4 x G^{\text{tad2l}}(3,1,1)+\text{m1}^2 x G^{\text{tad2l}}(2,1,1)+x G^{\text{tad2l}}(1,1,1),\left\{\text{FCTopology}\left(\text{tad2l},\left\{\frac{1}{\text{p1}^2-M^2},\frac{1}{\text{p2}^2-\text{m2}^2},\frac{1}{(\text{p1}-\text{p2})^2-\text{m3}^2}\right\},\{\text{p1},\text{p2}\},\{\},\{\},\{\}\right)\right\}\right\}$$

```mathematica
FCLoopGLIExpand[m2^2 GLI[tad2l, {1, 1, 1}], 
  {FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, {m2, 0, 6}]
```

$$\left\{\text{m2}^6 G^{\text{tad2l}}(1,3,1)+\text{m2}^4 G^{\text{tad2l}}(1,2,1)+\text{m2}^2 G^{\text{tad2l}}(1,1,1),\left\{\text{FCTopology}\left(\text{tad2l},\left\{\frac{1}{\text{p1}^2-\text{m1}^2},\frac{1}{\text{p2}^2},\frac{1}{(\text{p1}-\text{p2})^2-\text{m3}^2}\right\},\{\text{p1},\text{p2}\},\{\},\{\},\{\}\right)\right\}\right\}$$

```mathematica
FCLoopGLIExpand[ GLI[prop1l, {1, 1}] + SPD[q] GLI[prop1l, {1, 0}], 
  {FCTopology[prop1l, {FAD[{p1, m1}], FAD[{p1 + q, m2}]}, {p1}, {q}, {}, {}]}, {SPD[q], 0, 1}]
```

$$\left\{q^2 G^{\text{prop1l}}(1,0)-q^2 G^{\text{prop1l}}(1,2)+G^{\text{prop1l}}(1,1),\left\{\text{FCTopology}\left(\text{prop1l},\left\{\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{(-\text{m2}^2+\text{p1}^2+2 (\text{p1}\cdot q)+i \eta )}\right\},\{\text{p1}\},\{q\},\{\},\{\}\right)\right\}\right\}$$