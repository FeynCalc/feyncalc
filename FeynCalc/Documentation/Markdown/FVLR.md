```mathematica
 
```

## FVLR

`FVLR[p,mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the Lorentz vector $p^{\mu }$  along the vectors `n` and `nb`. It corresponds to $p^{\mu }_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLP](FVLP.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLN](MTLN.md), [MTLR](MTLR.md).

### Examples

```mathematica
FVLR[p, \[Mu], n, nb]
```

$$\overline{p}^{\mu }{}_{\perp }$$

```mathematica
FVLR[p, \[Mu], n, nb] // FCI // StandardForm

(*Pair[LightConePerpendicularComponent[LorentzIndex[\[Mu]], Momentum[n],Momentum[nb]], LightConePerpendicularComponent[Momentum[p], Momentum[n], Momentum[nb]]]*)
```

```mathematica
FVLR[p, \[Mu], n, nb] FVLR[q, \[Mu], n, nb] // Contract
```

$$\overline{p}\cdot \overline{q}_{\perp }$$

```mathematica
FVLR[p, \[Mu], n, nb] . FVLP[q, \[Mu], n, nb] // Contract
```

$$0$$