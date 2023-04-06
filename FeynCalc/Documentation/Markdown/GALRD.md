```mathematica
 
```

## GALRD

`GALRD[mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $\gamma^{\mu}_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALND](GALND.md), [GALPD](GALPD.md), [GSLPD](GSLPD.md), [GSLND](GSLND.md), [GSLRD](GSLRD.md).

### Examples

```mathematica
GALRD[\[Mu], n, nb]
```

$$\gamma ^{\mu }{}_{\perp }$$

```mathematica
StandardForm[GALRD[\[Mu], n, nb] // FCI]

(*DiracGamma[LightConePerpendicularComponent[LorentzIndex[\[Mu], D], Momentum[n, D], Momentum[nb, D]], D]*)
```

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GALRD[\[Mu], n, nb] . GALPD[\[Nu], n, nb] // DiracSimplify
```

$$-\frac{1}{4} \;\text{nb}^{\nu } (n\cdot \;\text{nb}) (\gamma \cdot n).\gamma ^{\mu }{}_{\perp }-\frac{1}{4} n^2 \;\text{nb}^{\nu } (\gamma \cdot \;\text{nb}).\gamma ^{\mu }{}_{\perp }$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
GALRD[\[Mu], n, nb] . GALPD[\[Nu], n, nb] // DiracSimplify
```

$$-\frac{1}{2} \;\text{nb}^{\nu } (\gamma \cdot n).\gamma ^{\mu }{}_{\perp }$$

```mathematica
FCClearScalarProducts[]
```