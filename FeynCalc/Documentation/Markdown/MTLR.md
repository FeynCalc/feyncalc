```mathematica
 
```

## MTLR

`MTLR[p,mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb`. It corresponds to $g^{\mu \nu}_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLP](FVLP.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLN](MTLN.md).

### Examples

```mathematica
MTLR[\[Mu], \[Nu], n, nb]
```

$$\bar{g}^{\mu \nu }{}_{\unicode{27c2}}$$

```mathematica
MTLR[\[Mu], \[Nu], n, nb] // FCI // StandardForm

(*Pair[LightConePerpendicularComponent[LorentzIndex[\[Mu]], Momentum[n],Momentum[nb]], LightConePerpendicularComponent[LorentzIndex[\[Nu]], Momentum[n], Momentum[nb]]]*)
```

```mathematica
MTLR[\[Mu], \[Nu], n, nb] FV[p, \[Mu]] // Contract
```

$$\overline{p}^{\nu }{}_{\unicode{27c2}}$$

```mathematica
MTLR[\[Mu], \[Nu], n, nb] FVLP[q, \[Mu], n, nb] // Contract
```

$$0$$