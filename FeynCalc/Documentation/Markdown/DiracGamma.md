## DiracGamma

`DiracGamma[x, dim]` is the head of all Dirac matrices and slashes (in the internal representation). Use `GA`, `GAD`, `GS` or `GSD` for manual (short) input.

`DiracGamma[x, 4]` simplifies to `DiracGamma[x]`.

`DiracGamma[5]`  is $\gamma ^5$.

`DiracGamma[6]` is $(1+\gamma ^5)/2$.

`DiracGamma[7]` is $(1-\gamma ^5)/2$.

### See also

[Overview](Extra/FeynCalc.md), [DiracGammaExpand](DiracGammaExpand.md), [GA](GA.md), [DiracSimplify](DiracSimplify.md), [GS](GS.md), [DiracTrick](DiracTrick.md).

### Examples

```mathematica
DiracGamma[5]
```

$$\bar{\gamma }^5$$

```mathematica
DiracGamma[LorentzIndex[\[Alpha]]]
```

$$\bar{\gamma }^{\alpha }$$

A Dirac-slash, i.e., $\gamma ^{\mu }q_{\mu}$, is displayed as $\gamma \cdot q$.

```mathematica
DiracGamma[Momentum[q]] 
```

$$\bar{\gamma }\cdot \overline{q}$$

```mathematica
DiracGamma[Momentum[q]] . DiracGamma[Momentum[p - q]]
```

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \left(\overline{p}-\overline{q}\right)\right)$$

```mathematica
DiracGamma[Momentum[q, D], D] 
```

$$\gamma \cdot q$$

```mathematica
GS[p - q] . GS[p]
DiracGammaExpand[%]
```

$$\left(\bar{\gamma }\cdot \left(\overline{p}-\overline{q}\right)\right).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$\left(\bar{\gamma }\cdot \overline{p}-\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}\right)$$

```mathematica
ex = GAD[\[Mu]] . GSD[p - q] . GSD[q] . GAD[\[Mu]]
```

$$\gamma ^{\mu }.(\gamma \cdot (p-q)).(\gamma \cdot q).\gamma ^{\mu }$$

```mathematica
DiracTrick[ex]
```

$$4 ((p-q)\cdot q)+(D-4) (\gamma \cdot (p-q)).(\gamma \cdot q)$$

```mathematica
DiracSimplify[ex]
```

$$D (\gamma \cdot p).(\gamma \cdot q)-D q^2-4 (\gamma \cdot p).(\gamma \cdot q)+4 (p\cdot q)$$

`DiracGamma` may also carry Cartesian indices or appear contracted with Cartesian momenta.

```mathematica
DiracGamma[CartesianIndex[i]]
```

$$\overline{\gamma }^i$$

```mathematica
DiracGamma[CartesianIndex[i, D - 1], D]
```

$$\gamma ^i$$

```mathematica
DiracGamma[CartesianMomentum[p]]
```

$$\overline{\gamma }\cdot \overline{p}$$

```mathematica
DiracGamma[CartesianMomentum[p, D - 1], D]
```

$$\gamma \cdot p$$

Temporal indices are represented using `ExplicitLorentzIndex[0]`

```mathematica
DiracGamma[ExplicitLorentzIndex[0]]
```

$$\bar{\gamma }^0$$