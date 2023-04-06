```mathematica
 
```

## FVLRD

`FVLRD[p,mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the Lorentz vector $p^{\mu }$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $p^{\mu }_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).

### Examples

```mathematica
FVLRD[p, \[Mu], n, nb]
```

$$p^{\mu }{}_{\unicode{27c2}}$$

```mathematica
FVLRD[p, \[Mu], n, nb] // FCI // StandardForm

(*Pair[LightConePerpendicularComponent[LorentzIndex[\[Mu], D], Momentum[n, D], Momentum[nb, D]], LightConePerpendicularComponent[Momentum[p, D], Momentum[n, D], Momentum[nb, D]]]*)
```

```mathematica
FVLRD[p, \[Mu], n, nb] FVLRD[q, \[Mu], n, nb] // Contract
```

$$p\cdot q_{\unicode{27c2}}$$

```mathematica
FVLRD[p, \[Mu], n, nb] . FVLPD[q, \[Mu], n, nb] // Contract
```

$$0$$