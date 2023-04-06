```mathematica
 
```

## GSLRD

`GSLRD[p,n,nb]` denotes the perpendicular component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb`  in $D$ dimensions. It corresponds to $(\gamma \cdot \p)_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALPD](GALPD.md), [GALND](GALND.md), [GALRD](GALRD.md), [GSLPD](GSLPD.md), [GSLND](GSLND.md).

### Examples

```mathematica
GSLRD[p, n, nb]
```

$$\gamma \cdot p_{\unicode{27c2}}$$

```mathematica
StandardForm[GSLRD[p, n, nb] // FCI]

(*DiracGamma[LightConePerpendicularComponent[Momentum[p, D], Momentum[n, D], Momentum[nb, D]], D]*)
```

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GSLRD[p, n, nb] . GSLPD[q, n, nb] // DiracSimplify
```

$$-\frac{1}{4} n^2 (\text{nb}\cdot q) (\gamma \cdot \;\text{nb}).\left(\gamma \cdot p_{\unicode{27c2}}\right)-\frac{1}{4} (n\cdot \;\text{nb}) (\text{nb}\cdot q) (\gamma \cdot n).\left(\gamma \cdot p_{\unicode{27c2}}\right)$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
GSLRD[p, n, nb] . GSLPD[q, n, nb] // DiracSimplify
```

$$-\frac{1}{2} (\text{nb}\cdot q) (\gamma \cdot n).\left(\gamma \cdot p_{\unicode{27c2}}\right)$$

```mathematica
FCClearScalarProducts[]
```