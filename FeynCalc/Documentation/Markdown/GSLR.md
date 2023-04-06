```mathematica
 
```

## GSLR

`GSLR[p,n,nb]` denotes the perpendicular component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb`. It corresponds to $(\gamma \cdot \p)_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALP](GALP.md), [GALN](GALN.md), [GALR](GALR.md), [GSLP](GSLP.md), [GSLN](GSLN.md).

### Examples

```mathematica
GSLR[p, n, nb]
```

$$\bar{\gamma }\cdot \overline{p}_{\unicode{27c2}}$$

```mathematica
StandardForm[GSLR[p, n, nb] // FCI]

(*DiracGamma[LightConePerpendicularComponent[Momentum[p], Momentum[n], Momentum[nb]]]*)
```

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GSLR[p, n, nb] . GSLP[q, n, nb] // DiracSimplify
```

$$-\frac{1}{4} \overline{n}^2 \left(\overline{\text{nb}}\cdot \overline{q}\right) \left(\bar{\gamma }\cdot \overline{\text{nb}}\right).\left(\bar{\gamma }\cdot \overline{p}_{\unicode{27c2}}\right)-\frac{1}{4} \left(\overline{n}\cdot \overline{\text{nb}}\right) \left(\overline{\text{nb}}\cdot \overline{q}\right) \left(\bar{\gamma }\cdot \overline{n}\right).\left(\bar{\gamma }\cdot \overline{p}_{\unicode{27c2}}\right)$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
GSLR[p, n, nb] . GSLP[q, n, nb] // DiracSimplify
```

$$-\frac{1}{2} \left(\overline{\text{nb}}\cdot \overline{q}\right) \left(\bar{\gamma }\cdot \overline{n}\right).\left(\bar{\gamma }\cdot \overline{p}_{\unicode{27c2}}\right)$$

```mathematica
FCClearScalarProducts[]
```