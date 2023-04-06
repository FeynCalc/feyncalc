```mathematica
 
```

## GALR

`GALR[mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb`. It corresponds to $\gamma^{\mu}_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALN](GALN.md), [GALP](GALP.md), [GSLP](GSLP.md), [GSLN](GSLN.md), [GSLR](GSLR.md).

### Examples

```mathematica
GALR[\[Mu], n, nb]
```

$$\bar{\gamma }^{\mu }{}_{\unicode{27c2}}$$

```mathematica
StandardForm[GALR[\[Mu], n, nb] // FCI]

(*DiracGamma[LightConePerpendicularComponent[LorentzIndex[\[Mu]], Momentum[n], Momentum[nb]]]*)
```

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GALR[\[Mu], n, nb] . GALP[\[Nu], n, nb] // iracSimplify
```

$$\text{iracSimplify}\left(\bar{\gamma }^{\mu }{}_{\unicode{27c2}}.\left(\frac{1}{2} \overline{\text{nb}}^{\nu } \bar{\gamma }\cdot \overline{n}\right)\right)$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
GALR[\[Mu], n, nb] . GALP[\[Nu], n, nb] // DiracSimplify
```

$$-\frac{1}{2} \overline{\text{nb}}^{\nu } \left(\bar{\gamma }\cdot \overline{n}\right).\bar{\gamma }^{\mu }{}_{\unicode{27c2}}$$

```mathematica
FCClearScalarProducts[]
```